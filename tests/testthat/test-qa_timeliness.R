test_that("check_recency passes on recent data and fails on stale data", {
  current <- as.integer(format(Sys.Date(), "%Y"))
  expect_equal(check_recency(data.frame(year = (current - 2):current), "year",
                             max_lag = 3)$status, "pass")
  expect_equal(check_recency(data.frame(year = 1990:2000), "year",
                             max_lag = 3)$status, "fail")
})

test_that("check_recency fails loudly when no usable years exist", {
  expect_equal(check_recency(data.frame(year = c(NA, NA)), "year")$status, "fail")
})

test_that("check_year_continuity detects and tolerates gaps", {
  df_gap <- data.frame(year = c(2015, 2016, 2018, 2019, 2021))
  res <- check_year_continuity(df_gap, "year")
  expect_equal(res$status, "warn")
  expect_equal(res$n_issues, 2)
  expect_match(res$detail, "2017")
  expect_equal(check_year_continuity(df_gap, "year", allow_gaps = 2)$status, "pass")
  expect_equal(check_year_continuity(data.frame(year = 2015:2021), "year")$status, "pass")
})

test_that("check_group_continuity aggregates per series", {
  df <- data.frame(
    stock = c(rep("cod", 6), rep("haddock", 6)),
    year = c(2015:2020, c(2015, 2016, 2018, 2019, 2020, 2021))
  )
  res <- check_group_continuity(df, "year", "stock", min_share = 0.9)
  expect_equal(res$n_issues, 1)
  expect_equal(res$status, "warn")
  expect_equal(check_group_continuity(df, "year", "stock", min_share = 0.5)$status, "pass")
})