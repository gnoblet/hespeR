# Test file for HesperCategory and HesperCategories classes

test_that("HesperCategory can be created with valid inputs", {
  category <- HesperCategory(
    cat = "health",
    vars = c("hesper_health", "hesper_health_care_male")
  )

  expect_s7_class(category, HesperCategory)
  expect_equal(category@cat, "health")
  expect_equal(
    category@vars,
    c("hesper_health", "hesper_health_care_male")
  )
})

test_that("HesperCategory validates cat property", {
  # Empty cat should fail
  expect_error(
    HesperCategory(
      cat = "",
      vars = c("hesper_health")
    ),
    "must have at least 1 characters"
  )

  # Missing cat should fail
  expect_error(
    HesperCategory(
      cat = character(0),
      vars = c("hesper_health")
    ),
    "Must have length 1"
  )

  # Multiple cat values should fail
  expect_error(
    HesperCategory(
      cat = c("health", "wash"),
      vars = c("hesper_health")
    ),
    "Must have length 1"
  )
})

test_that("HesperCategory validates vars property", {
  # Empty vars should fail
  expect_error(
    HesperCategory(
      cat = "health",
      vars = character(0)
    ),
    "Must have length >= 1"
  )

  # Invalid HESPER variable should fail
  expect_error(
    HesperCategory(
      cat = "health",
      vars = c("invalid_var")
    ),
    "Following values are not allowed: invalid_var"
  )

  # Mix of valid and invalid should fail
  expect_error(
    HesperCategory(
      cat = "health",
      vars = c("hesper_health", "invalid_var")
    ),
    "vars"
  )

  # Duplicate vars should fail
  expect_error(
    HesperCategory(
      cat = "health",
      vars = c("hesper_health", "hesper_health")
    ),
    "Duplicate values found in vars for category 'health'"
  )
})

test_that("HesperCategory works with different valid HESPER variables", {
  # Single variable
  cat1 <- HesperCategory(
    cat = "fsl",
    vars = c("hesper_food")
  )
  expect_s7_class(cat1, HesperCategory)
  expect_equal(cat1@vars, "hesper_food")

  # Multiple variables
  cat2 <- HesperCategory(
    cat = "protection",
    vars = c("hesper_safety", "hesper_gbv", "hesper_law")
  )
  expect_s7_class(cat2, HesperCategory)
  expect_length(cat2@vars, 3)
})

test_that("HesperCategories can be created with valid inputs", {
  cat1 <- HesperCategory(
    cat = "health",
    vars = c("hesper_health", "hesper_health_care_male")
  )
  cat2 <- HesperCategory(
    cat = "wash",
    vars = c("hesper_drinking_water", "hesper_toilet")
  )
  cat3 <- HesperCategory(
    cat = "protection",
    vars = c("hesper_safety", "hesper_gbv")
  )

  categories <- HesperCategories(category_list = list(cat1, cat2, cat3))

  expect_s7_class(categories, HesperCategories)
  expect_length(categories@category_list, 3)
})

test_that("HesperCategories validates category_list structure", {
  # Empty list should fail
  expect_error(
    HesperCategories(category_list = list()),
    "Must have length >= 1"
  )

  # Non-HesperCategory items should fail
  expect_error(
    HesperCategories(category_list = list("not_a_category")),
    "HesperCategory"
  )

  # Mix of valid and invalid should fail
  cat1 <- HesperCategory(cat = "health", vars = c("hesper_health"))
  expect_error(
    HesperCategories(category_list = list(cat1, "invalid")),
    "HesperCategory"
  )
})

test_that("HesperCategories validates unique category names", {
  cat1 <- HesperCategory(
    cat = "health",
    vars = c("hesper_health")
  )
  cat2 <- HesperCategory(
    cat = "health", # duplicate name
    vars = c("hesper_health_care_male")
  )

  expect_error(
    HesperCategories(category_list = list(cat1, cat2)),
    "category names \\(cat\\)"
  )
})

test_that("HesperCategories validates unique HESPER variables across categories", {
  cat1 <- HesperCategory(
    cat = "health",
    vars = c("hesper_health", "hesper_drinking_water")
  )
  cat2 <- HesperCategory(
    cat = "wash",
    vars = c("hesper_drinking_water", "hesper_toilet") # duplicate var
  )

  expect_error(
    HesperCategories(category_list = list(cat1, cat2)),
    "HESPER variables \\(vars\\) across categories"
  )
})

test_that("HesperCategories can be created with single category", {
  cat1 <- HesperCategory(cat = "health", vars = c("hesper_health"))
  categories <- HesperCategories(category_list = list(cat1))

  expect_s7_class(categories, HesperCategories)
  expect_length(categories@category_list, 1)
})

test_that("HesperCategories preserves order of categories", {
  cat1 <- HesperCategory(cat = "z_last", vars = c("hesper_health"))
  cat2 <- HesperCategory(cat = "a_first", vars = c("hesper_food"))
  cat3 <- HesperCategory(cat = "m_middle", vars = c("hesper_safety"))

  categories <- HesperCategories(category_list = list(cat1, cat2, cat3))

  # Order should be preserved as input, not alphabetical
  expect_equal(
    purrr::map_chr(categories@category_list, \(x) x@cat),
    c("z_last", "a_first", "m_middle")
  )
})

test_that("HesperCategories validates all HESPER variables are valid", {
  # This should work with all valid variables
  cat1 <- HesperCategory(cat = "health", vars = c("hesper_health"))
  cat2 <- HesperCategory(cat = "wash", vars = c("hesper_drinking_water"))

  categories <- HesperCategories(category_list = list(cat1, cat2))
  expect_s7_class(categories, HesperCategories)
})
