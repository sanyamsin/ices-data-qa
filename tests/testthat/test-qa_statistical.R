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

test_that("per-series outlier check ignores differences of scale between series", {
  set.seed(1)
  df <- data.frame(
    stock = rep(c("small", "large"), times = c(40, 10)),
    ssb = c(rnorm(40, 100, 5), rnorm(10, 1e6, 5e4))
  )
  # Pooled, every value of the large stock looks like an outlier.
  expect_equal(check_outliers_iqr(df, "ssb")$n_issues, 10)
  # Within each stock, nothing is unusual.
  res <- check_outliers_iqr_by_group(df, "ssb", "stock")
  expect_equal(res$n_issues, 0)
  expect_equal(res$status, "pass")
})

test_that("per-series outlier check still flags an unusual year", {
  set.seed(2)
  df <- data.frame(
    stock = rep(c("a", "b"), each = 30),
    ssb = c(rnorm(30, 100, 5), rnorm(30, 1e6, 5e4))
  )
  df$ssb[5] <- 10000
  res <- check_outliers_iqr_by_group(df, "ssb", "stock")
  expect_equal(res$n_issues, 1)
  expect_match(res$detail, "1 of 2 assessed series")
})

test_that("per-series outlier check skips short or constant series", {
  df <- data.frame(stock = rep(c("a", "b"), each = 5),
                   x = c(1, 2, 3, 4, 5, 7, 7, 7, 7, 7))
  res <- check_outliers_iqr_by_group(df, "x", "stock")
  expect_equal(res$status, "warn")
  expect_equal(res$n_rows, 0)
})

test_that("check_plateau_by_group does not join runs across series", {
  df <- data.frame(stock = rep(c("a", "b"), each = 4), x = 9)
  # Pooled: one run of 8. Per series: two runs of 4.
  expect_equal(check_plateau(df, "x", max_run = 5)$status, "warn")
  expect_equal(check_plateau_by_group(df, "x", "stock", max_run = 5)$status, "pass")
  res <- check_plateau_by_group(df, "x", "stock", max_run = 3)
  expect_equal(res$status, "warn")
  expect_equal(res$n_issues, 2)
})

test_that("check_plateau_by_group does not count missing values as a plateau", {
  df <- data.frame(stock = "a", x = c(1, 2, rep(NA, 10), 3))
  expect_equal(check_plateau_by_group(df, "x", "stock")$status, "pass")
})

test_that("run_summary_statistical_qa uses per-series checks when it can", {
  sag_like <- data.frame(
    AssessmentKey = rep(1:2, each = 10),
    F = runif(20, 0.05, 0.6),
    SSB = c(runif(10, 1000, 5000), runif(10, 1e6, 5e6))
  )
  expect_true("outliers_iqr_by_group" %in% run_summary_statistical_qa(sag_like)$check)
  pooled <- run_summary_statistical_qa(sag_like[c("F", "SSB")])
  expect_true("outliers_iqr" %in% pooled$check)
})
