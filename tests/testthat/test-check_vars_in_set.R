# Tests for check_vars_in_set function

test_that('check_vars_in_set throws an error when df is not a data frame', {
  df <- list(a = c("x", "y"), b = c("x", "y"))
  vars <- c("a", "b")
  set <- c("x", "y")
  expect_error(
    check_vars_in_set(df, vars, set)
  )
})

test_that('check_vars_in_set throws an error when vars is not a character vector', {
  df <- data.frame(a = c("x", "y"), b = c("x", "y"))
  vars <- list("a", "b")
  set <- c("x", "y")
  expect_error(
    check_vars_in_set(df, vars, set)
  )
})

test_that('check_vars_in_set throws an error if vars are missing from df', {
  df <- data.frame(a = c("x", "y"))
  vars <- c("a", "b")
  set <- c("x", "y")
  expect_error(
    check_vars_in_set(df, vars, set),
    regexp = "Missing variables|b"
  )
})

test_that("check_vars_in_set returns TRUE when all values are in set", {
  df <- data.frame(a = c("x", "y"), b = c("x", "y"))
  vars <- c("a", "b")
  set <- c("x", "y")
  expect_true(check_vars_in_set(df, vars, set))
})

test_that("check_vars_in_set throws error when values are not in set", {
  df <- data.frame(a = c("x", "z"), b = c("x", "y"))
  vars <- c("a", "b")
  set <- c("x", "y")
  expect_error(
    check_vars_in_set(df, vars, set)
  )
})

test_that("check_vars_in_set throws error when a variable is missing from df", {
  df <- data.frame(a = c("x", "y"))
  vars <- c("a", "b")
  set <- c("x", "y")
  expect_error(
    check_vars_in_set(df, vars, set),
    regexp = "Missing variables|b"
  )
})

test_that("check_vars_in_set allows missing values when allow_missing = TRUE", {
  df <- data.frame(a = c("x", NA), b = c(NA, "y"))
  vars <- c("a", "b")
  set <- c("x", "y")
  expect_true(check_vars_in_set(df, vars, set, allow_missing = TRUE))
})

test_that("check_vars_in_set errors on missing values when allow_missing = FALSE", {
  df <- data.frame(a = c("x", NA), b = c(NA_character_, "y"))
  vars <- c("a", "b")
  set <- c("x", "y")
  expect_error(check_vars_in_set(df, vars, set, allow_missing = FALSE))
})
