# Tests for HesperPriorities S7 class

# ---- Test Data Setup ----
valid_hesper_vars <- hesper_vars()[1:5] # Get first 5 valid variables

test_that("HesperPriorities creates valid object with correct properties", {
  obj <- HesperPriorities(
    top1 = valid_hesper_vars[1],
    top2 = valid_hesper_vars[2],
    top3 = valid_hesper_vars[3]
  )

  expect_s7_class(obj, HesperPriorities)
  expect_equal(obj@top1, valid_hesper_vars[1])
  expect_equal(obj@top2, valid_hesper_vars[2])
  expect_equal(obj@top3, valid_hesper_vars[3])
  expect_false(obj@allow_missing)
})

test_that("HesperPriorities accepts multiple respondents with same length vectors", {
  obj <- HesperPriorities(
    top1 = c(valid_hesper_vars[1], valid_hesper_vars[2]),
    top2 = c(valid_hesper_vars[2], valid_hesper_vars[3]),
    top3 = c(valid_hesper_vars[3], valid_hesper_vars[4])
  )

  expect_s7_class(obj, HesperPriorities)
  expect_length(obj@top1, 2)
  expect_length(obj@top2, 2)
  expect_length(obj@top3, 2)
})

test_that("HesperPriorities allows allow_missing parameter", {
  obj <- HesperPriorities(
    top1 = valid_hesper_vars[1],
    top2 = valid_hesper_vars[2],
    top3 = valid_hesper_vars[3],
    allow_missing = TRUE
  )

  expect_s7_class(obj, HesperPriorities)
  expect_true(obj@allow_missing)
})

test_that("HesperPriorities errors when top vectors have different lengths", {
  expect_error(
    HesperPriorities(
      top1 = valid_hesper_vars[1],
      top2 = c(valid_hesper_vars[2], valid_hesper_vars[3]),
      top3 = valid_hesper_vars[4]
    ),
    regexp = "Not all 'top\\*' have the same length"
  )
})

test_that("HesperPriorities errors when top1 is empty", {
  expect_error(
    HesperPriorities(
      top1 = character(0),
      top2 = valid_hesper_vars[2],
      top3 = valid_hesper_vars[3]
    ),
    regexp = "Assertion on 'self@top1' failed.*length >= 1"
  )
})

test_that("HesperPriorities errors when top2 is empty", {
  expect_error(
    HesperPriorities(
      top1 = valid_hesper_vars[1],
      top2 = character(0),
      top3 = valid_hesper_vars[3]
    ),
    regexp = "Assertion on 'self@top2' failed.*length >= 1"
  )
})

test_that("HesperPriorities errors when top3 is empty", {
  expect_error(
    HesperPriorities(
      top1 = valid_hesper_vars[1],
      top2 = valid_hesper_vars[2],
      top3 = character(0)
    ),
    regexp = "Assertion on 'self@top3' failed.*length >= 1"
  )
})

test_that("HesperPriorities errors when top1 contains invalid values", {
  expect_error(
    HesperPriorities(
      top1 = "Following values are not allowed: invalid_hesper_var",
      top2 = valid_hesper_vars[2],
      top3 = valid_hesper_vars[3]
    ),
    regexp = "Invalid values in top1"
  )
})

test_that("HesperPriorities errors when top2 contains invalid values", {
  expect_error(
    HesperPriorities(
      top1 = valid_hesper_vars[1],
      top2 = "invalid_hesper_var",
      top3 = valid_hesper_vars[3]
    ),
    regexp = "Invalid values in top2"
  )
})

test_that("HesperPriorities errors when top3 contains invalid values", {
  expect_error(
    HesperPriorities(
      top1 = valid_hesper_vars[1],
      top2 = valid_hesper_vars[2],
      top3 = "invalid_hesper_var"
    ),
    regexp = "Invalid values in top3"
  )
})

test_that("HesperPriorities errors when priorities are not unique within respondent", {
  expect_error(
    HesperPriorities(
      top1 = valid_hesper_vars[1],
      top2 = valid_hesper_vars[1], # Same as top1
      top3 = valid_hesper_vars[3]
    ),
    regexp = "Some item positions have non-unique hesper priorities"
  )
})

test_that("HesperPriorities errors when all three priorities are the same", {
  expect_error(
    HesperPriorities(
      top1 = rep(valid_hesper_vars[1], 2),
      top2 = rep(valid_hesper_vars[1], 2), # Same as top1
      top3 = rep(valid_hesper_vars[1], 2) # Same as top1 and top2
    ),
    regexp = "Some item positions have non-unique hesper priorities"
  )
})

test_that("HesperPriorities errors when some respondents have duplicate priorities", {
  expect_error(
    HesperPriorities(
      top1 = c(valid_hesper_vars[1], valid_hesper_vars[1]),
      top2 = c(valid_hesper_vars[2], valid_hesper_vars[1]), # Second respondent duplicates
      top3 = c(valid_hesper_vars[3], valid_hesper_vars[3])
    ),
    regexp = "Some item positions have non-unique hesper priorities.*position.*2"
  )
})

test_that("HesperPriorities accepts NA values when allow_missing = TRUE", {
  obj <- HesperPriorities(
    top1 = c(valid_hesper_vars[1], NA),
    top2 = c(valid_hesper_vars[2], valid_hesper_vars[2]),
    top3 = c(valid_hesper_vars[3], valid_hesper_vars[3]),
    allow_missing = TRUE
  )

  expect_s7_class(obj, HesperPriorities)
  expect_equal(obj@top1[2], NA_character_)
})

test_that("HesperPriorities errors on NA values when allow_missing = FALSE", {
  expect_error(
    HesperPriorities(
      top1 = c(valid_hesper_vars[1], NA),
      top2 = c(valid_hesper_vars[2], valid_hesper_vars[2]),
      top3 = c(valid_hesper_vars[3], valid_hesper_vars[3]),
      allow_missing = FALSE
    ),
    regexp = "Invalid values in top1"
  )
})

test_that("HesperPriorities allows NA values without uniqueness conflict when allow_missing = TRUE", {
  # NA values should not conflict with each other or other values for uniqueness
  obj <- HesperPriorities(
    top1 = c(valid_hesper_vars[1], NA, NA),
    top2 = c(valid_hesper_vars[2], NA, valid_hesper_vars[2]),
    top3 = c(valid_hesper_vars[3], valid_hesper_vars[3], NA),
    allow_missing = TRUE
  )

  expect_s7_class(obj, HesperPriorities)
})

test_that("HesperPriorities works with maximum length vectors", {
  n_vars <- length(valid_hesper_vars)
  if (n_vars >= 3) {
    # Create vectors where each respondent has different priorities
    obj <- HesperPriorities(
      top1 = rep(valid_hesper_vars[1], 10),
      top2 = rep(valid_hesper_vars[2], 10),
      top3 = rep(valid_hesper_vars[3], 10)
    )

    expect_s7_class(obj, HesperPriorities)
    expect_length(obj@top1, 10)
  }
})

test_that("HesperPriorities default allow_missing is FALSE", {
  obj <- HesperPriorities(
    top1 = valid_hesper_vars[1],
    top2 = valid_hesper_vars[2],
    top3 = valid_hesper_vars[3]
  )

  expect_false(obj@allow_missing)
})

test_that("HesperPriorities errors when top properties are not character", {
  expect_error(
    HesperPriorities(
      top1 = 1,
      top2 = valid_hesper_vars[2],
      top3 = valid_hesper_vars[3]
    ),
    regexp = "@top1 must be <character>"
  )
})

test_that("HesperPriorities validation runs after property modification", {
  obj <- HesperPriorities(
    top1 = valid_hesper_vars[1],
    top2 = valid_hesper_vars[2],
    top3 = valid_hesper_vars[3]
  )

  # Try to set duplicate values - should error
  expect_error(
    {
      obj@top2 <- valid_hesper_vars[1] # Same as top1
    },
    regexp = "non-unique hesper priorities"
  )
})
