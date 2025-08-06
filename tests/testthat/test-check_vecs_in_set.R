# Test for check_vecs_in_set function

test_that("check_vecs_in_set returns TRUE when all values are in set", {
  l_x <- list(
    var1 = c("A", "B", "C"),
    var2 = c("B", "C", "A")
  )
  allowed_set <- c("A", "B", "C")
  expect_true(check_vecs_in_set(l_x, allowed_set))
})

test_that("check_vecs_in_set throws error when values are not in set", {
  l_x <- list(
    var1 = c("A", "B", "C"),
    var2 = c("B", "C", "D", NA)
  )
  allowed_set <- c("A", "B", "C")
  expect_error(
    check_vecs_in_set(l_x, allowed_set),
    regexp = "Following values are not allowed: D, and NA"
  )
})

test_that("check_vecs_in_set handles NA values correctly", {
  l_x <- list(
    var1 = c("A", "B", NA),
    var2 = c(NA, "B", "C")
  )
  allowed_set <- c("A", "B", "C", NA)
  expect_true(check_vecs_in_set(l_x, allowed_set))
})

test_that("check_vecs_in_set throws error for multiple variables with invalid values", {
  l_x <- list(
    var1 = c("A", "B", "X"),
    var2 = c("Y", "C", "A")
  )
  allowed_set <- c("A", "B", "C")
  expect_error(
    check_vecs_in_set(l_x, allowed_set),
    regexp = "Following values are not allowed: X, and Y"
  )
})

test_that("check_vecs_in_set works with numeric values", {
  l_x <- list(
    var1 = c(1, 2, 3),
    var2 = c(2, 3, NA)
  )
  allowed_set <- c(1, 2, 3, NA)
  expect_true(check_vecs_in_set(l_x, allowed_set))
})

test_that("check_vecs_in_set throws error with numeric values not in set", {
  l_x <- list(
    var1 = c(1, 2, 4),
    var2 = c(2, 3, 5)
  )
  allowed_set <- c(1, 2, 3)
  expect_error(
    check_vecs_in_set(l_x, allowed_set),
    regexp = "Invalid|not allowed"
  )
})

test_that("check_vecs_in_set allows missing values when allow_missing = TRUE", {
  l_x <- list(var1 = c("A", NA), var2 = c(NA, "B"))
  allowed_set <- c("A", "B")
  expect_true(check_vecs_in_set(l_x, allowed_set, allow_missing = TRUE))
})

test_that("check_vecs_in_set errors on missing values when allow_missing = FALSE", {
  l_x <- list(var1 = c("A", NA), var2 = c(NA, "B"))
  allowed_set <- c("A", "B")
  expect_error(check_vecs_in_set(l_x, allowed_set, allow_missing = FALSE))
})

test_that("check_vecs_in_set errors if l_x is not a named list", {
  l_x <- list(c("A", "B", "C"))
  allowed_set <- c("A", "B", "C")
  expect_error(
    check_vecs_in_set(l_x, allowed_set),
    regexp = "Assertion on 'l_x' failed: Must have names."
  )
})

test_that("check_vecs_in_set errors if set is empty", {
  l_x <- list(var1 = c("A", "B"))
  allowed_set <- character(0)
  expect_error(
    check_vecs_in_set(l_x, allowed_set),
    regexp = "Assertion on 'set' failed: Must have length >= 1, but has length 0."
  )
})
