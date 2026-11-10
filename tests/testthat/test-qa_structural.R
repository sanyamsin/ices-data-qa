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