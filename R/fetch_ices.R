# Client for the ICES Stock Assessment Graphs (SAG) web services.
#
# The SAG API is documented at https://sag.ices.dk/SAG_API/ and provides
# open access to the stock assessment outputs that underpin ICES advice.
# All functions return tibbles and fail loudly with an informative
# message so that pipelines break early rather than silently passing
# corrupted data downstream.

SAG_BASE_URL <- "https://sag.ices.dk/SAG_API/api"

#' Perform a GET request against the SAG API
#'
#' @param endpoint Character scalar, e.g. "StockList".
#' @param query Named list of query parameters.
sag_get <- function(endpoint, query = list()) {
  req <- httr2::request(paste(SAG_BASE_URL, endpoint, sep = "/")) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("ices-data-qa (https://github.com/sanyamsin/ices-data-qa)") |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2^.x) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = \(resp) FALSE)
  
  resp <- httr2::req_perform(req)
  
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    cli::cli_abort(
      c("x" = "SAG API request failed with HTTP {status}.",
        "i" = "Endpoint: {.val {endpoint}}",
        "i" = "URL: {.url {httr2::resp_url(resp)}}")
    )
  }
  
  body <- httr2::resp_body_string(resp)
  if (!nzchar(body) || body == "null") {
    return(list())
  }
  jsonlite::fromJSON(body, simplifyVector = TRUE)
}

#' Coerce a parsed SAG payload to a tibble
#'
#' Handles the empty-result edge case and drops elements/columns that
#' are entirely NULL (the API returns JSON null for fields not populated
#' in a given assessment, which jsonlite keeps as NULLs and tibble
#' rejects). Works on both data frames and plain lists.
sag_to_tibble <- function(x) {
  if (length(x) == 0) {
    return(tibble::tibble())
  }
  x <- x[!vapply(x, is.null, logical(1))]
  tibble::as_tibble(x)
}

#' Fetch the list of assessed stocks for one or more assessment years
#'
#' @param years Integer vector of assessment years.
fetch_stock_list <- function(years) {
  stopifnot(is.numeric(years), length(years) >= 1, all(years >= 1900))
  
  purrr::map(years, \(yr) {
    raw <- sag_get("StockList", query = list(year = as.integer(yr)))
    out <- sag_to_tibble(raw)
    if (nrow(out) > 0 && !"AssessmentYear" %in% names(out)) {
      out$AssessmentYear <- as.integer(yr)
    }
    out
  }) |>
    purrr::list_rbind()
}

#' Fetch the SAG summary table for one or more assessments
#'
#' The API returns an *envelope* object: assessment metadata (stock,
#' units, model description) plus the time series itself nested under
#' `Lines` (one record per year). We extract and unnest the series,
#' tagging it with its assessment key.
#'
#' @param assessment_keys Integer vector of SAG assessment keys.
fetch_summary_table <- function(assessment_keys) {
  stopifnot(is.numeric(assessment_keys), length(assessment_keys) >= 1)
  
  purrr::map(assessment_keys, \(key) {
    raw <- sag_get("SummaryTable", query = list(assessmentKey = as.integer(key)))
    
    lines <- raw[["Lines"]]
    if (length(lines) == 0) {
      return(tibble::tibble())
    }
    
    out <- sag_to_tibble(lines)
    if (nrow(out) > 0) {
      out$AssessmentKey <- as.integer(key)
    }
    out
  }) |>
    purrr::list_rbind()
}

#' Fetch biological reference points for one or more assessments
#'
#' @param assessment_keys Integer vector of SAG assessment keys.
fetch_reference_points <- function(assessment_keys) {
  stopifnot(is.numeric(assessment_keys), length(assessment_keys) >= 1)
  
  purrr::map(assessment_keys, \(key) {
    raw <- sag_get("FishStockReferencePoints", query = list(assessmentKey = as.integer(key)))
    out <- sag_to_tibble(raw)
    if (nrow(out) > 0 && !"AssessmentKey" %in% names(out)) {
      out$AssessmentKey <- as.integer(key)
    }
    out
  }) |>
    purrr::list_rbind()
}