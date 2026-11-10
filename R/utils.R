# Shared helpers for the QA framework.
#
# Every check in this project returns a *QA result*: a single-row tibble
# with a standard schema. Standardising the output of checks makes them
# trivial to combine, summarise, test and render in reports.

#' Columns present in every QA result
QA_RESULT_COLS <- c(
  "category", "check", "column", "n_issues", "n_rows",
  "share_issues", "status", "detail"
)

#' Create a single QA result row
#'
#' @param category QA tier: "structural", "statistical" or "timeliness".
#' @param check Short machine-readable check name.
#' @param column Column the check applies to (NA if table-level).
#' @param n_issues Number of offending rows/values found.
#' @param n_rows Number of rows the check was applied to.
#' @param status One of "pass", "warn" or "fail".
#' @param detail Human-readable explanation, empty string when passing.
new_qa_result <- function(category,
                          check,
                          column = NA_character_,
                          n_issues = 0L,
                          n_rows = 0L,
                          status = c("pass", "warn", "fail"),
                          detail = "") {
  status <- match.arg(status)
  
  stopifnot(
    is.character(category), length(category) == 1,
    is.character(check), length(check) == 1,
    is.numeric(n_issues), n_issues >= 0,
    is.numeric(n_rows), n_rows >= 0
  )
  
  tibble::tibble(
    category = category,
    check = check,
    column = as.character(column),
    n_issues = as.integer(n_issues),
    n_rows = as.integer(n_rows),
    share_issues = ifelse(n_rows > 0, n_issues / n_rows, 0),
    status = status,
    detail = detail
  )
}

#' Combine QA results into a single table
bind_qa <- function(...) {
  results <- purrr::list_flatten(list(...))
  results <- purrr::keep(results, \(x) !is.null(x) && nrow(x) > 0)
  if (length(results) == 0) {
    return(tibble::tibble(
      category = character(), check = character(), column = character(),
      n_issues = integer(), n_rows = integer(), share_issues = double(),
      status = character(), detail = character()
    ))
  }
  dplyr::bind_rows(results)
}

#' Worst status across a QA result table
overall_status <- function(results) {
  if (nrow(results) == 0) return("pass")
  if (any(results$status == "fail")) return("fail")
  if (any(results$status == "warn")) return("warn")
  "pass"
}

#' Summarise QA results by category and status
summarise_qa <- function(results) {
  results |>
    dplyr::count(.data$category, .data$status, name = "n_checks") |>
    tidyr::pivot_wider(
      names_from = "status",
      values_from = "n_checks",
      values_fill = 0
    )
}

#' Status derived from a share of issues against thresholds
status_from_share <- function(share, warn_at, fail_at) {
  stopifnot(share >= 0, share <= 1, warn_at <= fail_at)
  if (share >= fail_at) return("fail")
  if (share >= warn_at) return("warn")
  "pass"
}