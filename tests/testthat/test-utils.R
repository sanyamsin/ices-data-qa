test_that("new_qa_result builds a valid row", {
  res <- new_qa_result("structural", "demo", column = "x",
                       n_issues = 2, n_rows = 10, status = "warn", detail = "d")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_true(all(QA_RESULT_COLS %in% names(res)))
  expect_equal(res$share_issues, 0.2)
})

test_that("new_qa_result validates its inputs", {
  expect_error(new_qa_result("structural", "demo", n_issues = -1))
  expect_error(new_qa_result("structural", "demo", status = "maybe"))
})

test_that("bind_qa stacks results and handles empties", {
  a <- new_qa_result("structural", "a", n_rows = 5)
  b <- new_qa_result("statistical", "b", n_issues = 1, n_rows = 5, status = "warn")
  combined <- bind_qa(a, NULL, b)
  expect_equal(nrow(combined), 2)
  expect_equal(nrow(bind_qa()), 0)
})

test_that("overall_status escalates correctly", {
  expect_equal(overall_status(bind_qa()), "pass")
  expect_equal(
    overall_status(bind_qa(
      new_qa_result("structural", "a", n_issues = 1, n_rows = 1, status = "warn"),
      new_qa_result("structural", "b", n_issues = 1, n_rows = 1, status = "fail")
    )),
    "fail"
  )
})

test_that("summarise_qa counts by category and status", {
  results <- bind_qa(
    new_qa_result("structural", "a", n_rows = 1),
    new_qa_result("structural", "b", n_issues = 1, n_rows = 1, status = "fail"),
    new_qa_result("timeliness", "c", n_issues = 1, n_rows = 1, status = "warn")
  )
  out <- summarise_qa(results)
  expect_equal(nrow(out), 2)
  expect_equal(out$fail[out$category == "structural"], 1)
})

test_that("status_from_share respects thresholds", {
  expect_equal(status_from_share(0, 0.05, 0.2), "pass")
  expect_equal(status_from_share(0.05, 0.05, 0.2), "warn")
  expect_equal(status_from_share(0.2, 0.05, 0.2), "fail")
  expect_error(status_from_share(1.5, 0.05, 0.2))
})