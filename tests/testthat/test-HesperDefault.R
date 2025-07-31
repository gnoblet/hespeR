# Tests for HesperDefault S7 class

test_that("HesperDefault accepts valid data frame with allowed columns and values", {
  df <- data.frame(
    hesper_drinking_water = c("serious_problem", "dnk"),
    hesper_food = c("no_serious_problem", "serious_problem"),
    stringsAsFactors = FALSE
  )
  # should produce a warning if some allowed columns are missing
  expect_warning(obj <- HesperDefault(df = df))
  expect_s7_class(obj, HesperDefault)
})

test_that("HesperDefault errors with data frame with zero columns", {
  df <- data.frame()
  expect_error(
    HesperDefault(df = df),
    regexp = "Must have at least 1 cols, but has 0 cols."
  )
})

test_that("HesperDefault errors if columns are not in allowed HESPER variables", {
  df <- data.frame(
    not_a_hesper_column = c("serious_problem", "dnk"),
    stringsAsFactors = FALSE
  )
  expect_error(
    HesperDefault(df = df)
  )
})

test_that("HesperDefault warns if allowed columns are missing", {
  # Only one allowed column present, others missing
  df <- data.frame(
    hesper_drinking_water = c("serious_problem", "dnk"),
    stringsAsFactors = FALSE
  )
  expect_warning(HesperDefault(df = df), regexp = "Missing variables")
})

test_that("HesperDefault errors if values are not in allowed HESPER options", {
  df <- data.frame(
    hesper_drinking_water = c("serious_problem", "not_a_valid_option"),
    stringsAsFactors = FALSE
  )
  expect_error(
    HesperDefault(df = df)
  )
})

test_that("HesperDefault accepts data frame with all allowed columns but with no row", {
  # Simulate all allowed columns, but no rows
  allowed_vars <- hesper_vars
  df <- as.data.frame(
    setNames(
      replicate(length(allowed_vars), character(0), simplify = FALSE),
      allowed_vars
    ),
    stringsAsFactors = FALSE
  )
  expect_silent(obj <- HesperDefault(df = df))
  expect_s7_class(obj, HesperDefault)
})

test_that("HesperDefault accepts data frame with all allowed columns but with only a few rows", {
  # Simulate all allowed columns, but only a subset of rows
  allowed_vars <- hesper_vars
  df <- as.data.frame(
    setNames(
      replicate(
        length(allowed_vars),
        c("serious_problem", "dnk"),
        simplify = FALSE
      ),
      allowed_vars
    ),
    stringsAsFactors = FALSE
  )
  expect_silent(obj <- HesperDefault(df = df))
  expect_s7_class(obj, HesperDefault)
})

test_that("HesperDefault errors if any allowed column contains invalid values", {
  allowed_vars <- hesper_vars
  df <- as.data.frame(
    setNames(
      replicate(
        length(allowed_vars),
        c("serious_problem", "dnk"),
        simplify = FALSE
      ),
      allowed_vars
    ),
    stringsAsFactors = FALSE
  )
  # Introduce an invalid value in one column
  df[[allowed_vars[1]]][1] <- "not_a_valid_option"
  expect_error(
    HesperDefault(df = df),
    regexp = "Invalid values|check_missing_vars"
  )
})
