# Tests for msg.R functions

test_that("msg_invalid_values returns correct message structure", {
  invalid <- c("foo", "bar")
  allowed <- c("baz", "qux")
  property <- "test_property"
  result <- msg_invalid_values(invalid, allowed, property)
  expect_type(result, "character")
  expect_named(result, c("", "*", "i"))
  expect_equal(length(result), 3)
  expect_equal(result[[1]], paste("Invalid values in", property))
  expect_equal(result[[2]], "Following values are not allowed: foo, and bar")
  expect_equal(result[[3]], "Values must be one of: baz, and qux")
})

test_that("msg_invalid_values uses full_message when provided", {
  invalid <- c("foo", "bar")
  allowed <- c("baz", "qux")
  property <- "test_property"
  full_message <- "Custom error message"
  result <- msg_invalid_values(invalid, allowed, property, full_message)
  expect_type(result, "character")
  expect_named(result, c("", "*", "i"))
  expect_equal(length(result), 3)
  expect_equal(result[[1]], full_message)
  expect_equal(result[[2]], "Following values are not allowed: foo, and bar")
  expect_equal(result[[3]], "Values must be one of: baz, and qux")
})


test_that("msg_missing_vars returns correct message structure with property being default NULL value", {
  df <- data.frame(a = 1, b = 2)
  vars <- c("c", "d")
  property <- NULL
  result <- msg_missing_vars('df', vars, property)
  expect_type(result, "character")
  expect_named(result, c("", "*", "i"))
  expect_equal(length(result), 3)
  expect_equal(result[[1]], "Missing variables in df")
  expect_equal(
    result[[2]],
    "The following variables are missing: c, and d"
  )
  expect_equal(
    result[[3]],
    "To check, available variables in the data frame, run `colnames(df)`"
  )
})


test_that("msg_missing_vars returns correct message structure with property q character string", {
  df <- data.frame(a = 1, b = 2)
  vars <- c("c", "d")
  property <- "test_vars"
  result <- msg_missing_vars('df', vars, property)
  expect_type(result, "character")
  expect_named(result, c("", "*", "i"))
  expect_equal(length(result), 3)
  expect_equal(result[[1]], "Missing variables in df")
  expect_equal(
    result[[2]],
    "The following variables from test_vars are missing: c, and d"
  )
  expect_equal(
    result[[3]],
    "To check, available variables in the data frame, run `colnames(df)`"
  )
})
