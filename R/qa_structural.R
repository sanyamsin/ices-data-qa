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

#' Run the standard structural QA suite on a table
#'
#' @param df A data frame.
#' @param required Named character vector for [check_schema()].
#' @param key_cols Key columns for [check_key_uniqueness()].
#' @param na_cols Columns for [check_missing()] (defaults to `required`).
run_structural_qa <- function(df, required, key_cols, na_cols = names(required)) {
  present_na_cols <- intersect(na_cols, names(df))
  
  bind_qa(
    check_schema(df, required),
    check_missing(df, present_na_cols),
    if (all(key_cols %in% names(df))) check_key_uniqueness(df, key_cols)
  )
}