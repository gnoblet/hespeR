test_that("SL class validates correct input", {
  # Assume hesper_vars is defined in the package and includes "hesper_drinking_water"
  valid_sl <- SL(
    hesper_var = "hesper_drinking_water",
    subset_var = "pop_group",
    subset_vals = c("refugees", "host")
  )
  expect_s7_class(valid_sl, SL)
  expect_equal(valid_sl@hesper_var, "hesper_drinking_water")
  expect_equal(valid_sl@subset_var, "pop_group")
  expect_equal(valid_sl@subset_vals, c("refugees", "host"))
})

test_that("SL class errors on invalid hesper_var", {
  expect_error(
    SL(
      hesper_var = "not_a_hesper_var",
      subset_var = "pop_group",
      subset_vals = c("refugees", "host")
    ),
    regexp = "hesper_var"
  )
})

test_that("SL class errors on invalid subset_var type", {
  expect_error(
    SL(
      hesper_var = "hesper_drinking_water",
      subset_var = c("pop_group", "other_group"),
      subset_vals = c("refugees", "host")
    ),
    regexp = "subset_var"
  )
})

test_that("SL class errors on empty subset_vals", {
  expect_error(
    SL(
      hesper_var = "hesper_drinking_water",
      subset_var = "pop_group",
      subset_vals = character(0)
    ),
    regexp = "subset_vals"
  )
})

test_that("SL class errors when subset_vals contains unique non-atomic or missing values", {
  expect_error(
    SL(
      hesper_var = "hesper_drinking_water",
      subset_var = "pop_group",
      subset_vals = list("refugees", "host")
    ),
    regexp = "subset_vals"
  )
  expect_error(
    SL(
      hesper_var = "hesper_drinking_water",
      subset_var = "pop_group",
      subset_vals = c("refugees", NA)
    ),
    regexp = "subset_vals"
  )
  expect_error(
    SL(
      hesper_var = "hesper_drinking_water",
      subset_var = "pop_group",
      subset_vals = c("refugees", "other", "other")
    ),
    regexp = "subset_vals"
  )
})
