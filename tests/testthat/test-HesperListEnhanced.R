# Tests for HesperListEnhanced S7 class

# ---- Test Data Setup ----
hesper_vars <- c("hesper_drinking_water", "hesper_food", "hesper_shelter")
hesper_opts <- c(
  "serious_problem",
  "no_serious_problem",
  "dnk",
  "pnta",
  "not_applicable"
)

# Helper function to create test data
create_test_data <- function() {
  # Create HesperVector objects
  hv1 <- HesperVector("hesper_drinking_water", hesper_opts[1:3])
  hv2 <- HesperVector("hesper_food", hesper_opts[2:4])
  hv3 <- HesperVector("hesper_shelter", hesper_opts[1:3])

  # Create HesperList
  hl <- HesperList(hesper_list = list(hv1, hv2, hv3))

  # Create other_list (same length as hesper_vals)
  other_list <- list(
    pop_group = c("refugees", "host", "refugees"),
    household_id = c("hh001", "hh002", "hh003")
  )

  # Create SL objects
  sl1 <- SL(
    hesper_var = "hesper_drinking_water",
    subset_var = "pop_group",
    subset_vals = c("host")
  )
  sl2 <- SL(
    hesper_var = "hesper_food",
    subset_var = "pop_group",
    subset_vals = c("refugees")
  )

  list(
    hesper_list = list(hv1, hv2, hv3),
    other_list = other_list,
    SL = list(sl1, sl2)
  )
}

# ---- Basic Constructor Tests ----
test_that("HesperListEnhanced accepts valid input", {
  test_data <- create_test_data()

  hle <- HesperListEnhanced(
    hesper_list = test_data$hesper_list,
    SL = test_data$SL,
    other_list = test_data$other_list
  )

  expect_s7_class(hle, HesperListEnhanced)
  expect_length(hle@hesper_list, 3)
  expect_length(hle@SL, 2)
  expect_length(hle@other_list, 2)
})

test_that("HesperListEnhanced works with empty SL list", {
  test_data <- create_test_data()

  hle <- HesperListEnhanced(
    hesper_list = test_data$hesper_list,
    SL = list(),
    other_list = test_data$other_list
  )

  expect_s7_class(hle, HesperListEnhanced)
  expect_length(hle@SL, 0)
})

test_that("HesperListEnhanced works with empty other_list", {
  test_data <- create_test_data()

  hle <- HesperListEnhanced(
    hesper_list = test_data$hesper_list,
    SL = list(),
    other_list = list()
  )

  expect_s7_class(hle, HesperListEnhanced)
  expect_length(hle@other_list, 0)
})

# ---- Validation Tests ----
test_that("HesperListEnhanced inherits HesperList validation", {
  test_data <- create_test_data()

  # Test duplicate hesper_vars (should fail from parent validation)
  hv_duplicate <- HesperVector("hesper_drinking_water", hesper_opts[1:2])

  expect_error(
    HesperListEnhanced(
      hesper_list = c(test_data$hesper_list, list(hv_duplicate)),
      SL = test_data$SL,
      other_list = test_data$other_list
    ),
    regexp = "Duplicate values found"
  )
})

test_that("HesperListEnhanced validates SL objects", {
  test_data <- create_test_data()

  # Test with non-SL object in SL list
  fake_sl <- list(
    hesper_var = "fake",
    subset_var = "fake",
    subset_vals = "fake"
  )

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = list(fake_sl),
      other_list = test_data$other_list
    ),
    regexp = "Not all items in 'SL' are of the specified class"
  )
})

test_that("HesperListEnhanced validates SL hesper_vars are in hesper_list", {
  test_data <- create_test_data()

  # Create SL with hesper_var not in hesper_list
  sl_invalid <- SL(
    hesper_var = "hesper_health", # not in test hesper_list
    subset_var = "pop_group",
    subset_vals = c("refugees")
  )

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = list(sl_invalid),
      other_list = test_data$other_list
    ),
    regexp = "are missing in hesper_list: hesper_health"
  )
})

test_that("HesperListEnhanced validates SL subset_vars are in other_list", {
  test_data <- create_test_data()

  # Create SL with subset_var not in other_list
  sl_invalid <- SL(
    hesper_var = "hesper_drinking_water",
    subset_var = "missing_var", # not in other_list
    subset_vals = c("some_val")
  )

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = list(sl_invalid),
      other_list = test_data$other_list
    ),
    regexp = "are missing in other_list: missing_var"
  )
})

test_that("HesperListEnhanced validates other_list lengths match hesper_list", {
  test_data <- create_test_data()

  # Create other_list with wrong length
  other_list_wrong <- list(
    pop_group = c("refugees", "host"), # length 2 instead of 3
    household_id = c("hh001", "hh002") # length 2 instead of 3
  )

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = other_list_wrong
    ),
    regexp = "All items in 'other_list' must have the same length as items in 'hesper_list'"
  )
})

test_that("HesperListEnhanced validates other_list items have consistent lengths", {
  test_data <- create_test_data()

  # Create other_list with inconsistent lengths
  other_list_inconsistent <- list(
    pop_group = c("refugees", "host", "refugees"), # length 3
    household_id = c("hh001", "hh002") # length 2
  )

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = other_list_inconsistent
    ),
    regexp = "All items in 'other_list' must have the same length as items in 'hesper_list'"
  )
})
