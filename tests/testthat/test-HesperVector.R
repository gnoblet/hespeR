# Tests for HesperVector S7 class

test_that("HesperVector accepts valid hesper_var and hesper_vals", {
  valid_var <- hesper_vars()[1]
  valid_vals <- hesper_opts()[1:2]
  obj <- HesperVector(
    hesper_var = valid_var,
    hesper_vals = valid_vals
  )
  expect_s7_class(obj, HesperVector)
  expect_equal(obj@hesper_var, valid_var)
  expect_equal(obj@hesper_vals, valid_vals)
})

test_that("HesperVector errors if hesper_var is not in hesper_vars()", {
  invalid_var <- "not_a_hesper_var"
  valid_vals <- hesper_opts()[1:2]
  expect_error(
    HesperVector(hesper_var = invalid_var, hesper_vals = valid_vals),
    regexp = "invalid|hesper_var"
  )
})

test_that("HesperVector errors if hesper_vals contains values not in hesper_opts()", {
  valid_var <- hesper_vars()[1]
  invalid_vals <- c(hesper_opts()[1], "not_a_valid_option")
  expect_error(
    HesperVector(hesper_var = valid_var, hesper_vals = invalid_vals),
    regexp = "invalid|hesper_vals"
  )
})

test_that("HesperVector errors if hesper_var is not a character scalar and non missing", {
  valid_vals <- hesper_opts()[1:2]
  expect_error(
    HesperVector(hesper_var = c("a", "b"), hesper_vals = valid_vals),
    regexp = "Assertion on 'self@hesper_var' failed: Must have length 1, but has length 2."
  )
  expect_error(
    HesperVector(hesper_var = NA_character_, hesper_vals = valid_vals),
    regexp = "Following values are not allowed: NA"
  )
})

test_that("HesperVector errors if hesper_vals is empty", {
  valid_var <- hesper_vars()[1]
  expect_error(
    HesperVector(hesper_var = valid_var, hesper_vals = character(0)),
    regexp = "Assertion on 'self@hesper_vals' failed: Must have length >= 1, but has length 0."
  )
})

test_that("HesperVector accepts hesper_vals with NA (if NA is allowed)", {
  valid_var <- hesper_vars()[1]
  vals_with_na <- c(hesper_opts()[1], NA)
  # If NA is allowed in hesper_opts(), this should be accepted
  if (any(is.na(hesper_opts()))) {
    expect_silent(HesperVector(
      hesper_var = valid_var,
      hesper_vals = vals_with_na
    ))
  } else {
    expect_error(
      HesperVector(hesper_var = valid_var, hesper_vals = vals_with_na),
      regexp = "invalid|hesper_vals"
    )
  }
})

test_that("HesperVector errors if hesper_vals is not a character vector", {
  valid_var <- hesper_vars()[1]
  expect_error(
    HesperVector(hesper_var = valid_var, hesper_vals = 1:3),
    regexp = "@hesper_vals must be <character>, not <integer>"
  )
})

test_that("HesperVector errors if hesper_var is missing", {
  valid_vals <- hesper_opts()[1:2]
  expect_error(
    HesperVector(hesper_vals = valid_vals),
    regexp = "Assertion on 'self@hesper_var' failed: Must have length 1, but has length 0."
  )
})

test_that("HesperVector errors if hesper_vals is missing", {
  valid_var <- hesper_vars()[1]
  expect_error(
    HesperVector(hesper_var = valid_var),
    regexp = "Assertion on 'self@hesper_vals' failed: Must have length >= 1, but has length 0."
  )
})
