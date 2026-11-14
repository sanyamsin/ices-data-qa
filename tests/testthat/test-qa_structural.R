make_demo_df <- function() {
  data.frame(
    id = 1:6,
    year = 2018:2023,
    value = c(10, 11, NA, 9, 10.5, 400),
    label = letters[1:6],
    stringsAsFactors = FALSE
  )
}

test_that("check_schema passes on conforming data", {
  res <- check_schema(make_demo_df(), c(id = "integer", label = "character"))
  expect_true(all(res$status == "pass"))
})

test_that("check_schema fails on missing columns and wrong types", {
  res <- check_schema(make_demo_df(), c(id = "integer", missing_col = NA))
  expect_equal(res$status[res$column == "missing_col"], "fail")
  expect_equal(check_schema(make_demo_df(), c(label = "numeric"))$status, "fail")
})

test_that("check_missing flags high NA shares", {
  res <- check_missing(make_demo_df(), "value", warn_at = 0.05, fail_at = 0.20)
  expect_equal(res$n_issues, 1)
  expect_equal(res$status, "warn")
  df_all_na <- data.frame(x = rep(NA_real_, 5))
  expect_equal(check_missing(df_all_na, "x")$status, "fail")
})

test_that("check_key_uniqueness detects duplicates", {
  expect_equal(check_key_uniqueness(make_demo_df(), "id")$status, "pass")
  df_dup <- rbind(make_demo_df(), make_demo_df()[1, ])
  expect_equal(check_key_uniqueness(df_dup, "id")$status, "fail")
})

test_that("run_structural_qa combines the standard checks", {
  res <- run_structural_qa(
    make_demo_df(),
    required = c(id = "integer", year = "integer", value = "numeric"),
    key_cols = "id"
  )
  expect_true(all(c("schema_column", "missing_values", "key_uniqueness") %in% res$check))
})

test_that("check_missing_by_group separates non-reporting series from gaps", {
  df <- data.frame(
    stock = rep(c("cod", "sandeel"), each = 10),
    f = c(runif(10, 0.1, 0.5), rep(NA, 10))
  )
  # Pooled, half of the values are missing...
  expect_equal(check_missing(df, "f")$status, "fail")
  # ...but the one series that reports F is complete.
  res <- check_missing_by_group(df, "f", "stock")
  expect_equal(res$status, "pass")
  expect_equal(res$n_issues, 0)
  expect_equal(res$n_rows, 10)
  expect_match(res$detail, "1 of 2 series do not report 'f'")
})

test_that("check_missing_by_group still catches gaps inside a series", {
  df <- data.frame(
    stock = rep(c("cod", "sandeel"), each = 10),
    f = c(0.2, NA, NA, NA, 0.3, 0.2, 0.4, 0.3, 0.2, 0.3, rep(NA, 10))
  )
  res <- check_missing_by_group(df, "f", "stock")
  expect_equal(res$n_issues, 3)
  expect_equal(res$status, "fail")
})

test_that("check_missing_by_group warns when no series reports the column", {
  df <- data.frame(stock = rep(c("a", "b"), each = 3), f = NA_real_)
  expect_equal(check_missing_by_group(df, "f", "stock")$status, "warn")
})

test_that("run_structural_qa switches to per-series missingness with group_col", {
  df <- data.frame(
    stock = rep(c("cod", "sandeel"), each = 5),
    year = rep(2019:2023, 2),
    f = c(runif(5), rep(NA, 5))
  )
  res <- run_structural_qa(df, required = c(f = "numeric"),
                           key_cols = c("stock", "year"), group_col = "stock")
  expect_true("missing_values_by_group" %in% res$check)
  expect_false("missing_values" %in% res$check)
  expect_equal(overall_status(res), "pass")
})

test_that("check_missing_by_group sets apart years outside the reported period", {
  df <- data.frame(
    stock = "pol",
    year = 2010:2021,
    # assessment starts in 2013, one hole in 2017, 2021 not estimated yet
    f = c(NA, NA, NA, 0.3, 0.2, 0.3, 0.2, NA, 0.3, 0.2, 0.3, NA)
  )
  # Without a time column every missing value is treated as a gap.
  expect_equal(check_missing_by_group(df, "f", "stock")$n_issues, 5)

  res <- check_missing_by_group(df, "f", "stock", order_col = "year")
  expect_equal(res$n_issues, 1)
  expect_equal(res$n_rows, 8)
  expect_match(res$detail, "4 values fall before the first or after the last")
})

test_that("check_missing_by_group orders each series before locating its span", {
  df <- data.frame(stock = "a", year = c(2003, 2001, 2002, 2004),
                   f = c(0.2, NA, 0.3, 0.1))
  res <- check_missing_by_group(df, "f", "stock", order_col = "year")
  expect_equal(res$n_issues, 0)
  expect_equal(res$n_rows, 3)
})

test_that("check_missing_by_group stays silent when there is nothing to say", {
  df <- data.frame(stock = rep(c("a", "b"), each = 3), year = rep(1:3, 2), f = 0.2)
  res <- check_missing_by_group(df, "f", "stock", order_col = "year")
  expect_equal(res$status, "pass")
  expect_equal(res$detail, "")
})
