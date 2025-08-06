# Tests for check_values_in_set function

test_that("check_values_in_set returns TRUE when all values are allowed", {
  x <- c("a", "b")
  allowed <- c("a", "b", "c")
  expect_true(check_values_in_set(x, allowed))
})

test_that("check_values_in_set notifies on invalid values", {
  x <- c("a", "d")
  allowed <- c("a", "b", "c")
  expect_error(
    check_values_in_set(x, allowed)
  )
})

test_that("check_values_in_set allows missing values when allow_missing = TRUE", {
  x <- c("a", NA)
  allowed <- c("a", "b", "c")
  expect_true(check_values_in_set(x, allowed, allow_missing = TRUE))
})

test_that("check_values_in_set errors on missing values when allow_missing = FALSE", {
  x <- c("a", NA)
  allowed <- c("a", "b", "c")
  expect_error(check_values_in_set(x, allowed, allow_missing = FALSE))
})
