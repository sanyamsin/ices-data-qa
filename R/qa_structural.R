# Structural QA checks: schema conformance, completeness, uniqueness.
#
# Structural checks answer the question "can this dataset be used at all?"
# before any statistical look at the values.

#' Check that required columns exist and have an acceptable type
#'
#' @param df A data frame.
#' @param required Named character vector: names are expected columns,
#'   values are allowed types ("numeric", "integer", "character", ...).
#'   Use NA to only check presence.
check_schema <- function(df, required) {
  stopifnot(is.data.frame(df), !is.null(names(required)))
  
  purrr::imap(names(required), \(col, i) {
    expected_type <- unname(required[[i]])
    
    if (!col %in% names(df)) {
      return(new_qa_result(
        category = "structural",
        check = "schema_column",
        column = col,
        n_issues = 1L,
        n_rows = 1L,
        status = "fail",
        detail = sprintf("Required column '%s' is missing.", col)
      ))
    }
    
    actual_type <- class(df[[col]])[[1]]
    type_ok <- is.na(expected_type) ||
      actual_type %in% strsplit(expected_type, "|", fixed = TRUE)[[1]]
    
    new_qa_result(
      category = "structural",
      check = "schema_column",
      column = col,
      n_issues = ifelse(type_ok, 0L, 1L),
      n_rows = 1L,
      status = ifelse(type_ok, "pass", "fail"),
      detail = ifelse(
        type_ok, "",
        sprintf("Column '%s' has type '%s', expected '%s'.",
                col, actual_type, expected_type)
      )
    )
  }) |>
    dplyr::bind_rows()
}

#' Check the share of missing values per column
#'
#' @param df A data frame.
#' @param cols Columns to check.
#' @param warn_at,fail_at Share-of-NA thresholds.
check_missing <- function(df, cols, warn_at = 0.05, fail_at = 0.20) {
  stopifnot(is.data.frame(df), all(cols %in% names(df)))
  
  purrr::map(cols, \(col) {
    n_na <- sum(is.na(df[[col]]))
    n <- nrow(df)
    share <- ifelse(n > 0, n_na / n, 0)
    status <- status_from_share(share, warn_at, fail_at)
    
    new_qa_result(
      category = "structural",
      check = "missing_values",
      column = col,
      n_issues = n_na,
      n_rows = n,
      status = status,
      detail = ifelse(
        status == "pass", "",
        sprintf("%.1f%% of values are missing in '%s'.", 100 * share, col)
      )
    )
  }) |>
    dplyr::bind_rows()
}

#' Check the uniqueness of a (composite) key
#'
#' @param df A data frame.
#' @param key_cols Columns that together should identify a row.
check_key_uniqueness <- function(df, key_cols) {
  stopifnot(is.data.frame(df), all(key_cols %in% names(df)))
  
  n_dup <- sum(duplicated(df[key_cols]))
  n <- nrow(df)
  
  new_qa_result(
    category = "structural",
    check = "key_uniqueness",
    column = paste(key_cols, collapse = " + "),
    n_issues = n_dup,
    n_rows = n,
    status = ifelse(n_dup == 0, "pass", "fail"),
    detail = ifelse(
      n_dup == 0, "",
      sprintf("%d duplicate rows on key (%s).",
              n_dup, paste(key_cols, collapse = ", "))
    )
  )
}

#' Check missing values per series, separating structure from gaps
#'
#' In a pooled table, three very different situations all show up as
#' missing values:
#'
#' * a series that never reports the variable (e.g. no fishing mortality
#'   estimate for a data-limited stock);
#' * years before the first or after the last reported value (the
#'   variable covers a shorter period than the series, or the latest
#'   year is not estimated yet);
#' * holes inside the reported period.
#'
#' Only the last one is a data gap. This check counts the three
#' separately and bases its status on the gaps alone; the other two are
#' described in `detail` so that nothing is hidden.
#'
#' @param df A data frame.
#' @param cols Columns to check.
#' @param group_col Column identifying independent series (e.g. a stock).
#' @param order_col Optional column ordering each series in time (e.g.
#'   the year). When supplied, values missing before the first or after
#'   the last reported value of a series are set apart from gaps. When
#'   `NULL`, every missing value in a reporting series counts as a gap.
#' @param warn_at,fail_at Share-of-NA thresholds, applied to the gaps.
check_missing_by_group <- function(df, cols, group_col, order_col = NULL,
                                   warn_at = 0.05, fail_at = 0.20) {
  stopifnot(
    is.data.frame(df),
    all(c(cols, group_col, order_col) %in% names(df))
  )

  groups <- split(df, df[[group_col]])
  n_groups <- length(groups)

  purrr::map(cols, \(col) {
    per_group <- purrr::map(groups, \(g) {
      x <- g[[col]]
      if (!is.null(order_col)) x <- x[order(g[[order_col]])]
      observed <- which(!is.na(x))
      if (length(observed) == 0) return(NULL)

      span <- if (is.null(order_col)) seq_along(x) else seq(min(observed), max(observed))
      c(n_span = length(span),
        n_gaps = sum(is.na(x[span])),
        n_outside = length(x) - length(span))
    })
    per_group <- purrr::compact(per_group)

    n_reporting <- length(per_group)
    n_not_reporting <- n_groups - n_reporting

    if (n_reporting == 0) {
      return(new_qa_result(
        category = "structural",
        check = "missing_values_by_group",
        column = col,
        n_issues = 0L,
        n_rows = 0L,
        status = "warn",
        detail = sprintf("'%s' is not reported by any of the %d series.",
                         col, n_groups)
      ))
    }

    n_rows <- sum(purrr::map_dbl(per_group, "n_span"))
    n_gaps <- sum(purrr::map_dbl(per_group, "n_gaps"))
    n_outside <- sum(purrr::map_dbl(per_group, "n_outside"))
    share <- n_gaps / n_rows
    status <- status_from_share(share, warn_at, fail_at)

    notes <- c(
      if (n_gaps > 0) {
        sprintf("%.1f%% of values missing within the %d series reporting '%s'",
                100 * share, n_reporting, col)
      },
      if (n_outside > 0) {
        sprintf("%d values fall before the first or after the last reported value of their series",
                n_outside)
      },
      if (n_not_reporting > 0) {
        sprintf("%d of %d series do not report '%s'", n_not_reporting, n_groups, col)
      }
    )

    new_qa_result(
      category = "structural",
      check = "missing_values_by_group",
      column = col,
      n_issues = n_gaps,
      n_rows = n_rows,
      status = status,
      detail = if (length(notes) == 0) "" else paste0(paste(notes, collapse = "; "), ".")
    )
  }) |>
    dplyr::bind_rows()
}

#' Run the standard structural QA suite on a table
#'
#' @param df A data frame.
#' @param required Named character vector for [check_schema()].
#' @param key_cols Key columns for [check_key_uniqueness()].
#' @param na_cols Columns for the missing-value check (defaults to
#'   `required`).
#' @param group_col Optional column identifying independent series. When
#'   supplied, missing values are assessed per series with
#'   [check_missing_by_group()] instead of on the pooled table.
#' @param order_col Optional time column, passed on to
#'   [check_missing_by_group()].
run_structural_qa <- function(df, required, key_cols,
                              na_cols = names(required), group_col = NULL,
                              order_col = NULL) {
  present_na_cols <- intersect(na_cols, names(df))
  by_group <- !is.null(group_col) && group_col %in% names(df)

  bind_qa(
    check_schema(df, required),
    if (by_group) {
      check_missing_by_group(df, present_na_cols, group_col, order_col)
    } else {
      check_missing(df, present_na_cols)
    },
    if (all(key_cols %in% names(df))) check_key_uniqueness(df, key_cols)
  )
}
