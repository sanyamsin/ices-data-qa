# Parsing is tested offline; live endpoints are exercised only when
# the API is reachable.

test_that("sag_to_tibble handles edge cases", {
  expect_equal(nrow(sag_to_tibble(list())), 0)
  # NULL elements are dropped, not fatal
  out <- sag_to_tibble(list(a = 1:3, b = NULL, c = letters[1:3]))
  expect_equal(nrow(out), 3)
  expect_false("b" %in% names(out))
})

test_that("fetch_stock_list validates its arguments", {
  expect_error(fetch_stock_list("2025"))
  expect_error(fetch_stock_list(numeric(0)))
  expect_error(fetch_stock_list(1850))
})

test_that("live SAG API returns a usable stock list", {
  testthat::skip_if_offline("sag.ices.dk")
  
  stocks <- fetch_stock_list(2025)
  expect_s3_class(stocks, "tbl_df")
  expect_gt(nrow(stocks), 50)
  expect_true(all(c("StockKeyLabel", "AssessmentKey") %in% names(stocks)))
})

test_that("live SAG API returns summary tables for a known stock", {
  testthat::skip_if_offline("sag.ices.dk")
  
  stocks <- fetch_stock_list(2025)
  key <- stocks$AssessmentKey[!is.na(stocks$AssessmentKey)][[1]]
  summary_tbl <- fetch_summary_table(key)
  
  expect_s3_class(summary_tbl, "tbl_df")
  expect_gt(nrow(summary_tbl), 0)
  expect_true("Year" %in% names(summary_tbl))
})