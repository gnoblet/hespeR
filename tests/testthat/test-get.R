# Tests for get method on HesperListEnhanced objects

# ---- Test Data Setup ----
create_test_hle <- function() {
  # Create HesperVector objects
  hv1 <- HesperVector(
    "hesper_drinking_water",
    c("serious_problem", "no_serious_problem", "serious_problem")
  )
  hv2 <- HesperVector(
    "hesper_food",
    c("no_serious_problem", "serious_problem", "dnk")
  )
  hv3 <- HesperVector(
    "hesper_shelter",
    c("serious_problem", "dnk", "serious_problem")
  )
  hv4 <- HesperVector(
    "hesper_health",
    c("serious_problem", "serious_problem", "serious_problem")
  )

  # Create other_list
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

  # Create HesperListEnhanced
  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3, hv4),
    SL = list(sl1, sl2),
    other_list = other_list,
    priority_list = list(),
    category_list = list()
  )

  return(hle)
}

# ---- Basic get Tests ----
test_that("get returns hesper_list property correctly", {
  hle <- create_test_hle()

  result <- get(hle, "hesper_list")

  expect_type(result, "list")
  expect_length(result, 4)
  expect_s7_class(result[[1]], HesperVector)
  expect_equal(result[[1]]@hesper_var, "hesper_drinking_water")
  expect_equal(result[[2]]@hesper_var, "hesper_food")
  expect_equal(result[[3]]@hesper_var, "hesper_shelter")
  expect_equal(result[[4]]@hesper_var, "hesper_health")
})

test_that("get returns hesper_list_pp property correctly", {
  hle <- create_test_hle()

  result <- get(hle, "hesper_list_pp")

  expect_type(result, "list")
  expect_length(result, 4)
  # hesper_list_pp should be processed by skip logic
  expect_s7_class(result[[1]], HesperVector)
})

test_that("get returns SL property correctly", {
  hle <- create_test_hle()

  result <- get(hle, "SL")

  expect_type(result, "list")
  expect_length(result, 2)
  expect_s7_class(result[[1]], SL)
  expect_equal(result[[1]]@hesper_var, "hesper_drinking_water")
  expect_equal(result[[2]]@hesper_var, "hesper_food")
})

# ---- Filtering with hesper_vars Tests ----
test_that("get filters hesper_list by hesper_vars correctly", {
  hle <- create_test_hle()

  result <- get(
    hle,
    "hesper_list",
    hesper_vars = c("hesper_food", "hesper_health")
  )

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(result[[1]]@hesper_var, "hesper_food")
  expect_equal(result[[2]]@hesper_var, "hesper_health")
})

test_that("get filters hesper_list_pp by hesper_vars correctly", {
  hle <- create_test_hle()

  result <- get(hle, "hesper_list_pp", hesper_vars = c("hesper_drinking_water"))

  expect_type(result, "list")
  expect_length(result, 1)
  expect_s7_class(result[[1]], HesperVector)
  expect_equal(result[[1]]@hesper_var, "hesper_drinking_water")
})

test_that("get filters SL by hesper_vars correctly", {
  hle <- create_test_hle()

  result <- get(hle, "SL", hesper_vars = c("hesper_food"))

  expect_type(result, "list")
  expect_length(result, 1)
  expect_s7_class(result[[1]], SL)
  expect_equal(result[[1]]@hesper_var, "hesper_food")
})

test_that("get errors when using nonexistent_var", {
  hle <- create_test_hle()

  expect_error(
    get(hle, "hesper_list", hesper_vars = c("nonexistent_var")),
    "Some or all provided `hesper_vars` not found",
    class = "rlang_error"
  )
})

test_that("get with empty hesper_vars returns empty list", {
  hle <- create_test_hle()

  result <- get(hle, "hesper_list", hesper_vars = character(0))

  expect_type(result, "list")
  expect_length(result, 0)
})

# ---- Validation Tests ----
test_that("get validates property argument correctly", {
  hle <- create_test_hle()

  expect_error(
    get(hle, "invalid_property"),
    class = "simpleError"
  )

  expect_error(
    get(hle, c("hesper_list", "SL")),
    class = "simpleError"
  )

  expect_error(
    get(hle, NULL),
    class = "simpleError"
  )
})

test_that("get validates hesper_vars argument correctly", {
  hle <- create_test_hle()

  # Should work with NULL
  expect_no_error(get(hle, "hesper_list", hesper_vars = NULL))

  # Should work with valid character vector
  expect_no_error(get(hle, "hesper_list", hesper_vars = c("hesper_food")))

  # Should error with non-character
  expect_error(
    get(hle, "hesper_list", hesper_vars = 123),
    class = "simpleError"
  )

  # Should error with factor
  expect_error(
    get(hle, "hesper_list", hesper_vars = factor("hesper_food")),
    class = "simpleError"
  )
})

test_that("get provides informative error for non-existent hesper_vars", {
  hle <- create_test_hle()

  expect_error(
    get(hle, "hesper_list", hesper_vars = c("nonexistent1", "nonexistent2")),
    "Some or all provided `hesper_vars` not found",
    class = "rlang_error"
  )

  # Mixed valid and invalid should also error
  expect_error(
    get(hle, "hesper_list", hesper_vars = c("hesper_food", "nonexistent")),
    "Some or all provided `hesper_vars` not found",
    class = "rlang_error"
  )
})

# ---- Edge Cases ----
test_that("get works with empty HesperListEnhanced", {
  hv1 <- HesperVector(
    "hesper_drinking_water",
    c("serious_problem", "no_serious_problem", "serious_problem")
  )
  hv2 <- HesperVector(
    "hesper_food",
    c("no_serious_problem", "serious_problem", "dnk")
  )
  hv3 <- HesperVector(
    "hesper_shelter",
    c("serious_problem", "dnk", "serious_problem")
  )
  hv4 <- HesperVector(
    "hesper_health",
    c("serious_problem", "serious_problem", "serious_problem")
  )
  hle_empty <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3, hv4),
    SL = list(),
    other_list = list(),
    priority_list = list(),
    category_list = list()
  )

  result_hesper <- get(hle_empty, "hesper_list")
  result_sl <- get(hle_empty, "SL")

  expect_type(result_hesper, "list")
  expect_length(result_hesper, 4)
  expect_type(result_sl, "list")
  expect_length(result_sl, 0)
})

test_that("get works when hesper_vars is NULL", {
  hle <- create_test_hle()

  result <- get(hle, "hesper_list", hesper_vars = NULL)

  expect_type(result, "list")
  expect_length(result, 4)
  expect_equal(result, hle@hesper_list)
})

test_that("get maintains order when filtering", {
  hle <- create_test_hle()

  # Test that order is preserved when filtering
  result <- get(
    hle,
    "hesper_list",
    hesper_vars = c("hesper_health", "hesper_food")
  )

  expect_length(result, 2)
  # Should maintain the order they appear in the original list
  expect_equal(result[[1]]@hesper_var, "hesper_food") # appears 2nd in original
  expect_equal(result[[2]]@hesper_var, "hesper_health") # appears 4th in original
})

# ---- Integration with get_vars Tests ----
test_that("get works correctly with get_vars output", {
  hle <- create_test_hle()

  # Get all vars and use them for filtering
  all_vars <- get_vars(hle)
  result <- get(hle, "hesper_list", hesper_vars = all_vars)

  expect_equal(result, hle@hesper_list)

  # Get subset of vars
  subset_vars <- get_vars(hle)[1:2]
  result_subset <- get(hle, "hesper_list", hesper_vars = subset_vars)

  expect_length(result_subset, 2)
  expect_equal(result_subset[[1]]@hesper_var, subset_vars[1])
  expect_equal(result_subset[[2]]@hesper_var, subset_vars[2])
})

test_that("get works with SL vars filtering", {
  hle <- create_test_hle()

  # Get SL vars and use for filtering SL
  sl_vars <- get_vars(hle, property = "SL")
  result <- get(hle, "SL", hesper_vars = sl_vars)

  expect_equal(result, hle@SL)

  # Filter SL by subset
  result_subset <- get(hle, "SL", hesper_vars = sl_vars[1])

  expect_length(result_subset, 1)
  expect_equal(result_subset[[1]]@hesper_var, sl_vars[1])
})
