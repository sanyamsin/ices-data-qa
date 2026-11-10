# Timeliness QA checks: is the data recent and continuous enough
# to support advice? ICES advice is produced on an annual cycle, so
# coverage gaps and stale series directly affect its usability.

#' Check that a series extends to a recent year
#'
#' @param df A data frame.
#' @param year_col Column holding the data year.
#' @param reference_year Year the advice is produced for (defaults to
#'   the current calendar year).
#' @param max_lag Largest acceptable gap between the latest data year
#'   and `reference_year` (assessment series typically lag by 1-2 years).
check_recency <- function(df,
                          year_col,
                          reference_year = as.integer(format(Sys.Date(), "%Y")),
                          max_lag = 3) {
  stopifnot(is.data.frame(df), year_col %in% names(df))
  
  years <- suppressWarnings(as.integer(df[[year_col]]))
  years <- years[!is.na(years)]
  
  if (length(years) == 0) {
    return(new_qa_result(
      category = "timeliness",
      check = "recency",
      column = year_col,
      n_issues = 1L, n_rows = 1L,
      status = "fail",
      detail = sprintf("No usable years found in '%s'.", year_col)
    ))
  }
  
  lag <- reference_year - max(years)
  
  new_qa_result(
    category = "timeliness",
    check = "recency",
    column = year_col,
    n_issues = as.integer(lag > max_lag),
    n_rows = 1L,
    status = ifelse(lag > max_lag, "fail", "pass"),
    detail = ifelse(
      lag > max_lag,
      sprintf("Latest data year is %d, i.e. a %d-year lag vs %d (allowed: %d).",
              max(years), lag, reference_year, max_lag),
      ""
    )
  )
}

#' Check the continuity of a yearly series
#'
#' @param df A data frame.
#' @param year_col Column holding the data year.
#' @param allow_gaps Number of missing intermediate years tolerated.
check_year_continuity <- function(df, year_col, allow_gaps = 0) {
  stopifnot(is.data.frame(df), year_col %in% names(df), allow_gaps >= 0)
  
  years <- sort(unique(suppressWarnings(as.integer(df[[year_col]]))))
  years <- years[!is.na(years)]
  
  if (length(years) < 2) {
    return(new_qa_result(
      category = "timeliness",
      check = "year_continuity",
      column = year_col,
      n_issues = 0L,
      n_rows = length(years),
      status = "pass",
      detail = ""
    ))
  }
  
  expected <- seq(min(years), max(years))
  missing_years <- setdiff(expected, years)
  n_missing <- length(missing_years)
  
  new_qa_result(
    category = "timeliness",
    check = "year_continuity",
    column = year_col,
    n_issues = n_missing,
    n_rows = length(expected),
    status = ifelse(n_missing <= allow_gaps, "pass", "warn"),
    detail = ifelse(
      n_missing == 0, "",
      sprintf("Missing years between %d and %d: %s.",
              min(years), max(years), paste(missing_years, collapse = ", "))
    )
  )
}

#' Check coverage continuity per group (e.g. per stock)
#'
#' @param df A data frame.
#' @param year_col Column holding the data year.
#' @param group_col Column identifying independent series.
#' @param min_share Minimum share of groups that must pass the
#'   continuity check for the overall result to pass.
check_group_continuity <- function(df, year_col, group_col, min_share = 0.90) {
  stopifnot(is.data.frame(df), all(c(year_col, group_col) %in% names(df)))
  
  groups <- split(df, df[[group_col]])
  results <- purrr::map_lgl(groups, \(g) {
    check_year_continuity(g, year_col)$status == "pass"
  })
  
  n_bad <- sum(!results)
  n <- length(results)
  share_ok <- ifelse(n > 0, 1 - n_bad / n, 1)
  
  new_qa_result(
    category = "timeliness",
    check = "group_year_continuity",
    column = paste0(year_col, " by ", group_col),
    n_issues = n_bad,
    n_rows = n,
    status = ifelse(share_ok >= min_share, "pass", "warn"),
    detail = ifelse(
      n_bad == 0, "",
      sprintf("%d of %d series in '%s' have gaps in yearly coverage.",
              n_bad, n, group_col)
    )
  )
}