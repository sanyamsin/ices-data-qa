test_that("check_range flags out-of-range values", {
  df <- data.frame(f = c(0.1, 0.2, 5, NA))
  expect_equal(check_range(df, "f", min = 0, max = 1)$status, "fail")
  expect_equal(check_range(df, "f", min = 0, max = 10)$status, "pass")
})

test_that("check_outliers_iqr detects far-out values", {
  set.seed(42)
  df <- data.frame(x = c(rnorm(100, 50, 5), 500))
  expect_gte(check_outliers_iqr(df, "x")$n_issues, 1)
  expect_equal(check_outliers_iqr(data.frame(x = rnorm(100)), "x")$n_issues, 0)
})

test_that("check_outliers_iqr handles degenerate inputs", {
  expect_equal(check_outliers_iqr(data.frame(x = c(1, 2, 2)), "x")$status, "warn")
  expect_equal(check_outliers_iqr(data.frame(x = rep(7, 10)), "x")$status, "pass")
})

test_that("check_non_negative catches impossible values", {
  df <- data.frame(ssb = c(100, 200, -5), f = c(0.1, 0.2, 0.3))
  res <- check_non_negative(df, c("ssb", "f"))
  expect_equal(res$status[res$column == "ssb"], "fail")
  expect_equal(res$status[res$column == "f"], "pass")
})

test_that("check_plateau warns on long constant runs", {
  expect_equal(check_plateau(data.frame(x = c(1:5, rep(9, 8))), "x")$status, "warn")
  expect_equal(check_plateau(data.frame(x = rnorm(30)), "x")$status, "pass")
})

test_that("run_summary_statistical_qa runs on a SAG-like table", {
  sag_like <- data.frame(
    AssessmentKey = rep(1:2, each = 10),
    Year = rep(2010:2019, 2),
    F = runif(20, 0.05, 0.6),
    SSB = runif(20, 1000, 5000),
    Catches = runif(20, 500, 2000)
  )
  res <- run_summary_statistical_qa(sag_like)
  expect_gt(nrow(res), 0)
  expect_equal(overall_status(res), "pass")
})