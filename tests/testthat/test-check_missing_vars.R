# Tests for check_missing_vars function

test_that("check_missing_vars returns NULL when all variables are present", {
  df <- data.frame(a = 1, b = 2)
  vars <- c("a", "b")
  expect_true(check_missing_vars(df, vars))
})

test_that("check_missing_vars throws error when variables are missing", {
  df <- data.frame(a = 1, b = 2)
  vars <- c("a", "c")
  expect_error(
    check_missing_vars(df, vars, property = "test_vars"),
    regexp = "Missing variables|c"
  )
})
