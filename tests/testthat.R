# testthat entry point: source the project functions, then run the suite.
# Run from the project root with: source("tests/testthat.R")

invisible(lapply(list.files("R", full.names = TRUE), source))

testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)