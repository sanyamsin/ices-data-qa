# Statistical QA checks: plausibility of the values themselves.
#
# These checks assume the dataset is structurally sound and ask whether
# the numbers are plausible for fisheries assessment time series.

#' Check that values fall within an allowed range
#'
#' @param df A data frame.
#' @param col Numeric column to check.
#' @param min,max Allowed bounds (inclusive). Use Inf to leave open.
check_range <- function(df, col, min = -Inf, max = Inf) {
  stopifnot(is.data.frame(df), col %in% names(df), is.numeric(df[[col]]))
  
  x <- df[[col]]
  observed <- !is.na(x)
  bad <- observed & (x < min | x > max)
  n_bad <- sum(bad)
  
  detail <- ""
  if (n_bad > 0) {
    detail <- sprintf(
      "%d values in '%s' outside [%s, %s] (observed range: [%s, %s]).",
      n_bad, col,
      format(min, scientific = FALSE), format(max, scientific = FALSE),
      format(min(x[observed]), scientific = FALSE),
      format(max(x[observed]), scientific = FALSE)
    )
  }
  
  new_qa_result(
    category = "statistical",
    check = "range",
    column = col,
    n_issues = n_bad,
    n_rows = sum(observed),
    status = ifelse(n_bad == 0, "pass", "fail"),
    detail = detail
  )
}

#' Flag statistical outliers with a robust IQR rule
#'
#' Values outside [Q1 - k*IQR, Q3 + k*IQR] are flagged. With k = 3 the
#' rule corresponds to "far out" values and stays robust for the skewed
#' distributions typical of catch and biomass series.
#'
#' @param df A data frame.
#' @param col Numeric column to check.
#' @param k IQR multiplier (default 3).
#' @param warn_at,fail_at Share-of-outlier thresholds.
check_outliers_iqr <- function(df, col, k = 3, warn_at = 0.01, fail_at = 0.10) {
  stopifnot(is.data.frame(df), col %in% names(df), is.numeric(df[[col]]), k > 0)
  
  x <- df[[col]][!is.na(df[[col]])]
  n <- length(x)
  
  if (n < 4) {
    return(new_qa_result(
      category = "statistical",
      check = "outliers_iqr",
      column = col,
      n_issues = 0L,
      n_rows = n,
      status = "warn",
      detail = sprintf("Too few observations (%d) to assess outliers in '%s'.", n, col)
    ))
  }
  
  qs <- stats::quantile(x, probs = c(0.25, 0.75), names = FALSE)
  iqr <- qs[[2]] - qs[[1]]
  
  if (iqr == 0) {
    n_distinct <- length(unique(x))
    return(new_qa_result(
      category = "statistical",
      check = "outliers_iqr",
      column = col,
      n_issues = 0L,
      n_rows = n,
      status = ifelse(n_distinct > 1, "warn", "pass"),
      detail = ifelse(n_distinct > 1,
                      sprintf("IQR is zero in '%s'; outlier rule not applicable.", col), "")
    ))
  }
  
  lower <- qs[[1]] - k * iqr
  upper <- qs[[2]] + k * iqr
  n_out <- sum(x < lower | x > upper)
  share <- n_out / n
  status <- status_from_share(share, warn_at, fail_at)
  
  new_qa_result(
    category = "statistical",
    check = "outliers_iqr",
    column = col,
    n_issues = n_out,
    n_rows = n,
    status = status,
    detail = ifelse(
      status == "pass", "",
      sprintf("%.1f%% of values in '%s' outside [%.3g, %.3g] (k = %g).",
              100 * share, col, lower, upper, k)
    )
  )
}

#' Check that columns contain no negative values
#'
#' Catches, biomass and fishing mortality are non-negative quantities;
#' a negative value always signals a data or unit problem.
check_non_negative <- function(df, cols) {
  stopifnot(is.data.frame(df), all(cols %in% names(df)))
  
  purrr::map(cols, \(col) {
    x <- df[[col]]
    n_neg <- sum(!is.na(x) & x < 0)
    new_qa_result(
      category = "statistical",
      check = "non_negative",
      column = col,
      n_issues = n_neg,
      n_rows = sum(!is.na(x)),
      status = ifelse(n_neg == 0, "pass", "fail"),
      detail = ifelse(
        n_neg == 0, "",
        sprintf("%d negative values in '%s' (impossible for a non-negative quantity).",
                n_neg, col)
      )
    )
  }) |>
    dplyr::bind_rows()
}

#' Detect suspicious plateaus (identical repeated values)
#'
#' Long runs of identical values in a continuous series often indicate
#' fill-forward errors rather than genuine stability.
#'
#' @param df A data frame.
#' @param col Numeric column to check.
#' @param max_run Longest acceptable run of identical values.
check_plateau <- function(df, col, max_run = 5) {
  stopifnot(is.data.frame(df), col %in% names(df), is.numeric(df[[col]]), max_run >= 1)
  
  x <- df[[col]]
  runs <- rle(ifelse(is.na(x), NA_real_, x))
  longest <- if (length(runs$lengths) == 0) 0L else max(runs$lengths)
  
  new_qa_result(
    category = "statistical",
    check = "plateau",
    column = col,
    n_issues = as.integer(longest > max_run),
    n_rows = 1L,
    status = ifelse(longest > max_run, "warn", "pass"),
    detail = ifelse(
      longest > max_run,
      sprintf("Longest run of identical values in '%s' is %d (threshold: %d).",
              col, longest, max_run),
      ""
    )
  )
}

#' Flag outliers within each series with a robust IQR rule
#'
#' Applies the rule of [check_outliers_iqr()] separately to each series.
#' Pooling series first would compare stocks of very different sizes and
#' exploitation levels, and flag whole stocks instead of unusual years.
#' Series that are too short or constant are not assessed.
#'
#' @param df A data frame.
#' @param col Numeric column to check.
#' @param group_col Column identifying independent series (e.g. a stock).
#' @param k IQR multiplier (default 3).
#' @param min_n Minimum number of observed values for a series to be
#'   assessed.
#' @param warn_at,fail_at Share-of-outlier thresholds.
check_outliers_iqr_by_group <- function(df, col, group_col, k = 3, min_n = 8,
                                        warn_at = 0.01, fail_at = 0.10) {
  stopifnot(
    is.data.frame(df), all(c(col, group_col) %in% names(df)),
    is.numeric(df[[col]]), k > 0, min_n >= 4
  )

  per_group <- purrr::map(split(df[[col]], df[[group_col]]), \(x) {
    x <- x[!is.na(x)]
    if (length(x) < min_n) return(NULL)
    qs <- stats::quantile(x, probs = c(0.25, 0.75), names = FALSE)
    iqr <- qs[[2]] - qs[[1]]
    if (iqr == 0) return(NULL)
    c(n = length(x), n_out = sum(x < qs[[1]] - k * iqr | x > qs[[2]] + k * iqr))
  })
  per_group <- purrr::compact(per_group)

  if (length(per_group) == 0) {
    return(new_qa_result(
      category = "statistical",
      check = "outliers_iqr_by_group",
      column = col,
      n_issues = 0L,
      n_rows = 0L,
      status = "warn",
      detail = sprintf(
        "No series with at least %d varying values to assess outliers in '%s'.",
        min_n, col
      )
    ))
  }

  n <- sum(purrr::map_dbl(per_group, "n"))
  n_out <- sum(purrr::map_dbl(per_group, "n_out"))
  n_flagged <- sum(purrr::map_dbl(per_group, "n_out") > 0)
  share <- n_out / n
  status <- status_from_share(share, warn_at, fail_at)

  new_qa_result(
    category = "statistical",
    check = "outliers_iqr_by_group",
    column = col,
    n_issues = n_out,
    n_rows = n,
    status = status,
    detail = ifelse(
      n_out == 0, "",
      sprintf("%d values in '%s' flagged in %d of %d assessed series (per-series IQR rule, k = %g).",
              n_out, col, n_flagged, length(per_group), k)
    )
  )
}

#' Detect suspicious plateaus within each series
#'
#' Applies the logic of [check_plateau()] separately to each series, so
#' that a run cannot span the boundary between two series.
#'
#' @param df A data frame, ordered in time within each series.
#' @param col Numeric column to check.
#' @param group_col Column identifying independent series (e.g. a stock).
#' @param max_run Longest acceptable run of identical values.
check_plateau_by_group <- function(df, col, group_col, max_run = 5) {
  stopifnot(
    is.data.frame(df), all(c(col, group_col) %in% names(df)),
    is.numeric(df[[col]]), max_run >= 1
  )

  longest <- purrr::map_int(split(df[[col]], df[[group_col]]), \(x) {
    runs <- rle(x)
    lengths <- runs$lengths[!is.na(runs$values)]
    if (length(lengths) == 0) 0L else max(lengths)
  })
  n_bad <- sum(longest > max_run)

  new_qa_result(
    category = "statistical",
    check = "plateau_by_group",
    column = col,
    n_issues = n_bad,
    n_rows = length(longest),
    status = ifelse(n_bad > 0, "warn", "pass"),
    detail = ifelse(
      n_bad > 0,
      sprintf("%d of %d series have a run of more than %d identical values in '%s' (longest: %d).",
              n_bad, length(longest), max_run, col, max(longest)),
      ""
    )
  )
}

#' Run the standard statistical QA suite on a SAG summary table
#'
#' @param df A SAG summary table.
#' @param group_col Column identifying independent series. When present,
#'   outliers and plateaus are assessed per series; otherwise the checks
#'   fall back to the pooled table.
run_summary_statistical_qa <- function(df, group_col = "AssessmentKey") {
  numeric_cols <- intersect(
    c("F", "SSB", "Recruitment", "Catches", "Landings", "Discards",
      "TBiomass", "FMSY", "MSYBtrigger", "Blim", "Bpa", "Flim", "Fpa"),
    names(df)
  )
  numeric_cols <- numeric_cols[purrr::map_lgl(numeric_cols, \(c) is.numeric(df[[c]]))]
  plateau_cols <- intersect(c("F", "SSB"), numeric_cols)
  by_group <- !is.null(group_col) && group_col %in% names(df)

  if (by_group) {
    return(bind_qa(
      check_non_negative(df, numeric_cols),
      purrr::map(numeric_cols, \(c) check_outliers_iqr_by_group(df, c, group_col)),
      purrr::map(plateau_cols, \(c) check_plateau_by_group(df, c, group_col))
    ))
  }

  bind_qa(
    check_non_negative(df, numeric_cols),
    purrr::map(numeric_cols, \(c) check_outliers_iqr(df, c)),
    purrr::map(plateau_cols, \(c) check_plateau(df, c))
  )
}
