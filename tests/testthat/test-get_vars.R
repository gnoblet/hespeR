# Tests for get_vars generic method and its S7 implementations

# ---- Test Data Setup ----
hesper_vars <- c(
  "hesper_drinking_water",
  "hesper_food",
  "hesper_shelter",
  "hesper_health"
)
hesper_opts <- c(
  "serious_problem",
  "no_serious_problem",
  "dnk",
  "pnta",
  "not_applicable"
)

# Helper function to create test objects
create_test_objects <- function() {
  # Create HesperVector objects
  hv1 <- HesperVector("hesper_drinking_water", c("serious_problem", "dnk"))
  hv2 <- HesperVector("hesper_food", c("no_serious_problem", "serious_problem"))
  hv3 <- HesperVector("hesper_shelter", c("dnk", "not_applicable"))

  # Create HesperList
  hl <- HesperList(hesper_list = list(hv1, hv2, hv3))

  # Create HesperCategory objects
  cat1 <- HesperCategory(
    cat = "basic_needs",
    vars = c("hesper_drinking_water", "hesper_food")
  )
  cat2 <- HesperCategory(
    cat = "shelter_wash",
    vars = c("hesper_shelter")
  )

  # Create HesperCategories
  hcs <- HesperCategories(category_list = list(cat1, cat2))

  # Create SL object
  sl <- SL(
    hesper_var = "hesper_drinking_water",
    subset_var = "pop_group",
    subset_vals = c("refugees", "host")
  )

  # Create HesperListEnhanced with correct constructor
  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3),
    SL = list(sl),
    other_list = list(
      pop_group = c("refugees", "host"),
      survey_round = c("baseline", "endline")
    ),
    priority_list = list(),
    category_list = list(hcs)
  )

  list(
    hv1 = hv1,
    hv2 = hv2,
    hv3 = hv3,
    hl = hl,
    cat1 = cat1,
    cat2 = cat2,
    hcs = hcs,
    sl = sl,
    hle = hle
  )
}

# ---- Tests for HesperVector ----

test_that("get_vars works for HesperVector", {
  test_objs <- create_test_objects()

  # Test single variable extraction
  result <- get_vars(test_objs$hv1)
  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(result, "hesper_drinking_water")

  # Test different variable
  result2 <- get_vars(test_objs$hv2)
  expect_equal(result2, "hesper_food")
})

test_that("get_vars returns character vector for HesperVector", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$hv1)
  expect_type(result, "character")
  expect_false(any(is.na(result)))
  expect_true(all(nchar(result) > 0))
})

# ---- Tests for HesperList ----

test_that("get_vars works for HesperList", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$hl)
  expect_type(result, "character")
  expect_length(result, 3)
  expect_equal(
    result,
    c("hesper_drinking_water", "hesper_food", "hesper_shelter")
  )
  expect_false(any(is.na(result)))
})

test_that("get_vars preserves order for HesperList", {
  # Create HesperList with specific order
  hv_z <- HesperVector("hesper_shelter", "serious_problem")
  hv_a <- HesperVector("hesper_drinking_water", "dnk")
  hv_m <- HesperVector("hesper_food", "no_serious_problem")

  hl_ordered <- HesperList(hesper_list = list(hv_z, hv_a, hv_m))

  result <- get_vars(hl_ordered)
  expect_equal(
    result,
    c("hesper_shelter", "hesper_drinking_water", "hesper_food")
  )
})

test_that("get_vars works for single item HesperList", {
  hv <- HesperVector("hesper_health", "serious_problem")
  hl_single <- HesperList(hesper_list = list(hv))

  result <- get_vars(hl_single)
  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(result, "hesper_health")
})

# ---- Tests for HesperCategory ----

test_that("get_vars works for HesperCategory", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$cat1)
  expect_type(result, "character")
  expect_length(result, 2)
  expect_equal(result, c("hesper_drinking_water", "hesper_food"))
})

test_that("get_vars returns vars not cat for HesperCategory", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$cat1)
  # Should return @vars, not @cat
  expect_false("basic_needs" %in% result)
  expect_true(all(c("hesper_drinking_water", "hesper_food") %in% result))
})

test_that("get_vars works for single var HesperCategory", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$cat2)
  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(result, "hesper_shelter")
})

# ---- Tests for HesperCategories ----

test_that("get_vars works for HesperCategories", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$hcs)
  expect_type(result, "character")
  expect_length(result, 3)
  expect_setequal(
    result,
    c("hesper_drinking_water", "hesper_food", "hesper_shelter")
  )
})

test_that("get_vars flattens all variables from HesperCategories", {
  # Create multiple categories with overlapping structure
  cat1 <- HesperCategory(
    cat = "wash",
    vars = c("hesper_drinking_water", "hesper_toilet")
  )
  cat2 <- HesperCategory(cat = "food", vars = c("hesper_food"))
  cat3 <- HesperCategory(
    cat = "health",
    vars = c("hesper_health", "hesper_health_care_male")
  )

  hcs <- HesperCategories(category_list = list(cat1, cat2, cat3))

  result <- get_vars(hcs)
  expected_vars <- c(
    "hesper_drinking_water",
    "hesper_toilet",
    "hesper_food",
    "hesper_health",
    "hesper_health_care_male"
  )

  expect_type(result, "character")
  expect_length(result, 5)
  expect_setequal(result, expected_vars)
})

test_that("get_vars preserves order from HesperCategories", {
  # Test that order is maintained through the categories
  cat1 <- HesperCategory(cat = "first", vars = c("hesper_shelter"))
  cat2 <- HesperCategory(
    cat = "second",
    vars = c("hesper_drinking_water", "hesper_food")
  )

  hcs <- HesperCategories(category_list = list(cat1, cat2))

  result <- get_vars(hcs)
  expect_equal(
    result,
    c("hesper_shelter", "hesper_drinking_water", "hesper_food")
  )
})

# ---- Tests for SL ----

test_that("get_vars works for SL", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$sl)
  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(result, "hesper_drinking_water")
})

test_that("get_vars returns hesper_var from SL object", {
  sl_test <- SL(
    hesper_var = "hesper_safety",
    subset_var = "age_group",
    subset_vals = c("adult", "child")
  )

  result <- get_vars(sl_test)
  expect_equal(result, "hesper_safety")
  # Should not return subset_var
  expect_false("age_group" %in% result)
})

# ---- Tests for HesperListEnhanced ----

test_that("get_vars works for HesperListEnhanced with default property", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$hle)
  expect_type(result, "character")
  expect_length(result, 3)
  expect_equal(
    result,
    c("hesper_drinking_water", "hesper_food", "hesper_shelter")
  )
})

test_that("get_vars works for HesperListEnhanced hesper_list property", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$hle, property = "hesper_list")
  expect_type(result, "character")
  expect_length(result, 3)
  expect_equal(
    result,
    c("hesper_drinking_water", "hesper_food", "hesper_shelter")
  )
})

test_that("get_vars works for HesperListEnhanced hesper_list_pp property", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$hle, property = "hesper_list_pp")
  expect_type(result, "character")
  expect_length(result, 3)
  expect_equal(
    result,
    c("hesper_drinking_water", "hesper_food", "hesper_shelter")
  )
})

test_that("get_vars works for HesperListEnhanced category_list property", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$hle, property = "category_list")
  expect_type(result, "character")
  expect_length(result, 3)
  expect_setequal(
    result,
    c("hesper_drinking_water", "hesper_food", "hesper_shelter")
  )
})

test_that("get_vars works for HesperListEnhanced SL property", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$hle, property = "SL")
  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(result, "hesper_drinking_water")
})

test_that("get_vars works for HesperListEnhanced other_list property", {
  test_objs <- create_test_objects()

  result <- get_vars(test_objs$hle, property = "other_list")
  expect_type(result, "character")
  expect_length(result, 2)
  expect_setequal(result, c("pop_group", "survey_round"))
})

test_that("get_vars handles empty properties in HesperListEnhanced", {
  # Create HLE with empty category_list and SL
  hv1 <- HesperVector("hesper_drinking_water", "serious_problem")
  hle_empty <- HesperListEnhanced(
    hesper_list = list(hv1),
    SL = list(),
    other_list = list(),
    priority_list = list(),
    category_list = list()
  )

  # Empty category_list should return character(0)
  result_cats <- get_vars(hle_empty, property = "category_list")
  expect_type(result_cats, "character")
  expect_length(result_cats, 0)

  # Empty SL should return character(0)
  result_sl <- get_vars(hle_empty, property = "SL")
  expect_type(result_sl, "character")
  expect_length(result_sl, 0)

  # Empty other_list should work
  result_other <- get_vars(hle_empty, property = "other_list")
  expect_type(result_other, "character")
  expect_length(result_other, 0)
})

test_that("get_vars errors for invalid property in HesperListEnhanced", {
  test_objs <- create_test_objects()

  expect_error(
    get_vars(test_objs$hle, property = "invalid_property"),
    regexp = "Assertion on 'property' failed"
  )

  expect_error(
    get_vars(test_objs$hle, property = c("hesper_list", "SL")),
    regexp = "Assertion on 'property' failed"
  )
})

test_that("get_vars validates property argument type in HesperListEnhanced", {
  test_objs <- create_test_objects()

  expect_error(
    get_vars(test_objs$hle, property = 123),
    regexp = "Assertion on 'property' failed"
  )

  expect_error(
    get_vars(test_objs$hle, property = TRUE),
    regexp = "Assertion on 'property' failed"
  )
})

# ---- Edge Cases and Error Handling ----

test_that("get_vars maintains character type across all methods", {
  test_objs <- create_test_objects()

  # Test all object types return character vectors
  results <- list(
    hv = get_vars(test_objs$hv1),
    hl = get_vars(test_objs$hl),
    hc = get_vars(test_objs$cat1),
    hcs = get_vars(test_objs$hcs),
    sl = get_vars(test_objs$sl),
    hle = get_vars(test_objs$hle)
  )

  purrr::walk(
    results,
    ~ {
      expect_type(.x, "character")
      expect_false(any(is.na(.x)))
    }
  )
})

test_that("get_vars returns meaningful variable names", {
  test_objs <- create_test_objects()

  # All results should be valid HESPER variable names or other meaningful names
  results <- list(
    hv = get_vars(test_objs$hv1),
    hl = get_vars(test_objs$hl),
    hc = get_vars(test_objs$cat1),
    hcs = get_vars(test_objs$hcs),
    sl = get_vars(test_objs$sl),
    hle_other = get_vars(test_objs$hle, property = "other_list")
  )

  purrr::walk(
    results,
    ~ {
      expect_true(all(nchar(.x) > 0)) # No empty strings
      expect_false(any(is.na(.x))) # No NA values
    }
  )
})

# ---- Integration Tests ----

test_that("get_vars works consistently across nested structures", {
  # Test that get_vars works the same whether called on individual objects
  # or when those objects are nested in collections

  hv1 <- HesperVector("hesper_drinking_water", "serious_problem")
  hv2 <- HesperVector("hesper_food", "dnk")

  # Individual calls
  var1 <- get_vars(hv1)
  var2 <- get_vars(hv2)

  # Through HesperList
  hl <- HesperList(hesper_list = list(hv1, hv2))
  vars_from_list <- get_vars(hl)

  expect_equal(vars_from_list, c(var1, var2))
})

test_that("get_vars handles complex HesperListEnhanced scenarios", {
  # Create a complex HLE with multiple properties containing different variables
  hv1 <- HesperVector("hesper_drinking_water", "serious_problem")
  hv2 <- HesperVector("hesper_food", "dnk")
  cat1 <- HesperCategory("wash", c("hesper_drinking_water"))
  hcs1 <- HesperCategories(category_list = list(cat1))
  sl1 <- SL("hesper_food", "pop_group", c("refugees"))

  hle_complex <- HesperListEnhanced(
    hesper_list = list(hv1, hv2),
    SL = list(sl1),
    other_list = list(
      location = c("camp_a"),
      pop_group = c("refugees")
    ),
    priority_list = list(),
    category_list = list(hcs1)
  )

  # Test each property returns expected results
  expect_equal(
    get_vars(hle_complex, property = "hesper_list"),
    c("hesper_drinking_water", "hesper_food")
  )
  expect_equal(
    get_vars(hle_complex, property = "hesper_list_pp"),
    c("hesper_drinking_water", "hesper_food")
  )
  expect_equal(
    get_vars(hle_complex, property = "category_list"),
    "hesper_drinking_water"
  )
  expect_equal(get_vars(hle_complex, property = "SL"), "hesper_food")
  expect_equal(
    get_vars(hle_complex, property = "other_list"),
    c("location", "pop_group")
  )
})

# ---- Test category_list_pp property ----

test_that("get_vars should handle category_list_pp property", {
  test_objs <- create_test_objects()

  expect_equal(
    get_vars(test_objs$hle, property = "category_list_pp"),
    c("basic_needs", "shelter_wash")
  )
})
