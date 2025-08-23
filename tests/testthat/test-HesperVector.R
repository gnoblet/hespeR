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
    regexp = "Assertion on 'self@hesper_var' failed"
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

test_that("hesper_bins property returns correct binary encoding", {
  valid_var <- hesper_vars()[1]
  valid_vals <- c("serious_problem", "no_serious_problem")
  obj <- HesperVector(
    hesper_var = valid_var,
    hesper_vals = valid_vals
  )

  bins <- obj@hesper_bins
  all_opts <- hesper_opts()

  # Should return a named list with all HESPER options
  expect_type(bins, "list")
  expect_equal(names(bins), all_opts)
  expect_length(bins, length(all_opts))

  # Each element should be an integer vector
  purrr::walk(bins, ~ expect_type(.x, "integer"))

  # Check specific values
  expect_equal(bins$serious_problem, c(1L, 0L))
  expect_equal(bins$no_serious_problem, c(0L, 1L))
  expect_equal(bins$dnk, c(0L, 0L))
  expect_equal(bins$pnta, c(0L, 0L))
  expect_equal(bins$not_applicable, c(0L, 0L))
})

test_that("hesper_bins works with single value", {
  valid_var <- hesper_vars()[1]
  obj <- HesperVector(
    hesper_var = valid_var,
    hesper_vals = "dnk"
  )

  bins <- obj@hesper_bins

  # Check that only the matching option has 1, others have 0
  expect_equal(bins$serious_problem, 0L)
  expect_equal(bins$no_serious_problem, 0L)
  expect_equal(bins$dnk, 1L)
  expect_equal(bins$pnta, 0L)
  expect_equal(bins$not_applicable, 0L)
})

test_that("hesper_bins works with duplicate values", {
  valid_var <- hesper_vars()[1]
  valid_vals <- c("serious_problem", "serious_problem", "dnk")
  obj <- HesperVector(
    hesper_var = valid_var,
    hesper_vals = valid_vals
  )

  bins <- obj@hesper_bins

  # Should handle duplicates correctly
  expect_equal(bins$serious_problem, c(1L, 1L, 0L))
  expect_equal(bins$no_serious_problem, c(0L, 0L, 0L))
  expect_equal(bins$dnk, c(0L, 0L, 1L))
})

test_that("hesper_bins works with all possible options", {
  valid_var <- hesper_vars()[1]
  all_opts <- hesper_opts()
  obj <- HesperVector(
    hesper_var = valid_var,
    hesper_vals = all_opts
  )

  bins <- obj@hesper_bins

  # Each option should have exactly one 1 and the rest 0s
  for (i in seq_along(all_opts)) {
    expected_vector <- integer(length(all_opts))
    expected_vector[i] <- 1L
    expect_equal(
      bins[[all_opts[i]]],
      expected_vector,
      info = paste("Failed for option:", all_opts[i])
    )
  }
})

test_that("hesper_bins updates when hesper_vals changes", {
  valid_var <- hesper_vars()[1]
  obj <- HesperVector(
    hesper_var = valid_var,
    hesper_vals = "serious_problem"
  )

  # Initial state
  bins1 <- obj@hesper_bins
  expect_equal(bins1$serious_problem, 1L)
  expect_equal(bins1$dnk, 0L)

  # Change hesper_vals
  obj@hesper_vals <- c("serious_problem", "dnk")

  # Bins should update accordingly
  bins2 <- obj@hesper_bins
  expect_equal(bins2$serious_problem, c(1L, 0L))
  expect_equal(bins2$dnk, c(0L, 1L))
})
