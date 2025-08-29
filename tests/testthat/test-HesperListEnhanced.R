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
  # Create HesperVector objects with position-specific values
  # Position 1: serious_problem, Position 2: no_serious_problem, Position 3: serious_problem
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
  # only serious problem vectors for priorities
  hv4 <- HesperVector(
    "hesper_health",
    c("serious_problem", "serious_problem", "serious_problem")
  )
  hv5 <- HesperVector(
    "hesper_education",
    c("serious_problem", "serious_problem", "serious_problem")
  )
  hv6 <- HesperVector(
    "hesper_toilet",
    c("serious_problem", "serious_problem", "serious_problem")
  )

  # Create HesperList
  hl <- HesperList(hesper_list = list(hv1, hv2, hv3, hv4, hv5, hv6))

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

  # Create priority_list based on hv4, hv5, hv6
  priority_list <- HesperPriorities(
    top1 = c("hesper_education", "hesper_toilet", "hesper_health"),
    top2 = c("hesper_toilet", "hesper_education", "hesper_toilet"),
    top3 = c("hesper_health", "hesper_health", "hesper_education"),
    allow_missing = FALSE
  )

  list(
    hesper_list = list(hv1, hv2, hv3, hv4, hv5, hv6),
    other_list = other_list,
    SL = list(sl1, sl2),
    priority_list = list(priority_list)
  )
}

# ---- Basic Constructor Tests ----
test_that("HesperListEnhanced accepts valid input", {
  test_data <- create_test_data()

  hle <- HesperListEnhanced(
    hesper_list = test_data$hesper_list,
    SL = test_data$SL,
    other_list = test_data$other_list,
    priority_list = test_data$priority_list
  )

  expect_s7_class(hle, HesperListEnhanced)
  expect_length(hle@hesper_list, 6)
  expect_length(hle@SL, 2)
  expect_length(hle@other_list, 2)
  expect_length(hle@priority_list, 1)
  expect_s7_class(hle@priority_list[[1]], HesperPriorities)
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
    hesper_var = "hesper_movement", # not in test hesper_list
    subset_var = "pop_group",
    subset_vals = c("refugees")
  )

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = list(sl_invalid),
      other_list = test_data$other_list
    ),
    regexp = "are missing in hesper_list: hesper_movement"
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

# ---- HesperPriorities Tests ----
test_that("HesperListEnhanced works with NULL priority_list (default)", {
  test_data <- create_test_data()

  hle <- HesperListEnhanced(
    hesper_list = test_data$hesper_list,
    SL = test_data$SL,
    other_list = test_data$other_list
  )

  expect_s7_class(hle, HesperListEnhanced)
  expect_length(hle@hesper_list, 6)
  expect_length(hle@priority_list, 0)
})

test_that("HesperListEnhanced errors when priority_list reference non-serious-problem variables at specific positions", {
  test_data <- create_test_data()

  # Create HesperPriorities with position-wise validation errors
  # Position 1: food doesn't have serious_problem (has no_serious_problem)
  # Position 2: drinking_water doesn't have serious_problem (has no_serious_problem)
  priority_list <- HesperPriorities(
    top1 = c("hesper_education", "hesper_toilet", "hesper_health"),
    top2 = c("hesper_toilet", "hesper_education", "hesper_toilet"),
    top3 = c("hesper_health", "hesper_drinking_water", "hesper_education"),
    allow_missing = FALSE
  )
  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = test_data$other_list,
      priority_list = list(priority_list)
    ),
    regexp = "Some hesper priorities do not correspond to items marked as 'serious_problem' at the same position"
  )
})

test_that("HesperListEnhanced errors when priority_list reference variables not in hesper_list", {
  test_data <- create_test_data()

  # Create HesperPriorities with variable not in hesper_list
  priority_list <- HesperPriorities(
    top1 = c("hesper_education", "hesper_toilet", "hesper_health"),
    top2 = c("hesper_toilet", "hesper_education", "hesper_toilet"),
    top3 = c("hesper_time", "hesper_food", "hesper_education"),
    allow_missing = FALSE
  )

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = test_data$other_list,
      priority_list = list(priority_list)
    ),
    regexp = "not present in hesper_list"
  )
})

test_that("HesperListEnhanced validates priority_list length matches hesper_list", {
  test_data <- create_test_data()

  # Create HesperPriorities with wrong length (2 instead of 3)
  priority_list <- HesperPriorities(
    top1 = c("hesper_education", "hesper_toilet"),
    top2 = c("hesper_toilet", "hesper_education"),
    top3 = c("hesper_time", "hesper_food"),
    allow_missing = FALSE
  )
  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = test_data$other_list,
      priority_list = list(priority_list)
    ),
    regexp = "HesperPriorities vectors must have the same length as hesper_list items.*Expected length: 3.*got lengths: 2"
  )
})

test_that("HesperListEnhanced accepts priority_list with NA values when allow_missing = TRUE", {
  test_data <- create_test_data()

  # Create HesperPriorities with NA values - position-wise validation still applies to non-NA values
  priorities <- HesperPriorities(
    top1 = c("hesper_drinking_water", NA, "hesper_drinking_water"), # Positions 1,3: OK
    top2 = c("hesper_shelter", "hesper_food", NA), # Positions 1,2: OK
    top3 = c(NA, "hesper_health", "hesper_health"), # Positions 2,3: OK
    allow_missing = TRUE
  )

  hle <- HesperListEnhanced(
    hesper_list = test_data$hesper_list,
    SL = test_data$SL,
    other_list = test_data$other_list,
    priority_list = list(priorities)
  )

  expect_s7_class(hle, HesperListEnhanced)
  expect_s7_class(hle@priority_list[[1]], HesperPriorities)
})

test_that("HesperListEnhanced shows position-specific error details", {
  test_data <- create_test_data()

  # Create HesperPriorities with position-specific validation errors
  priority_list <- HesperPriorities(
    top1 = c("hesper_education", "hesper_toilet", "hesper_health"),
    top2 = c("hesper_toilet", "hesper_education", "hesper_toilet"),
    top3 = c("hesper_health", "hesper_food", "hesper_food"),
    allow_missing = FALSE
  )

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = test_data$other_list,
      priority_list = list(priority_list)
    ),
    regexp = "Position 3.*hesper_food.*not marked as 'serious_problem'"
  )
})

test_that("HesperListEnhanced validates HesperPriorities object itself", {
  test_data <- create_test_data()
  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = test_data$other_list,
      priority_list = list(HesperPriorities(
        top1 = c(
          "hesper_drinking_water", # Position 1: OK
          "hesper_food", # Position 2: OK
          "hesper_drinking_water" # Position 3: OK
        ),
        top2 = c(
          "hesper_drinking_water", # Position 1: DUPLICATE with top1
          "hesper_food", # Position 2: OK
          "hesper_shelter" # Position 3: OK
        ),
        top3 = c(
          "hesper_shelter", # Position 1: OK
          "hesper_food", # Position 2: DUPLICATE with top1 and top2
          "hesper_drinking_water" # Position 3: OK
        )
      ))
    ),
    regexp = "have non-unique"
  )
})


# ---- HesperCategories Tests ----
test_that("HesperListEnhanced works with empty category_list (default)", {
  test_data <- create_test_data()

  hle <- HesperListEnhanced(
    hesper_list = test_data$hesper_list,
    SL = test_data$SL,
    other_list = test_data$other_list
  )

  expect_s7_class(hle, HesperListEnhanced)
  expect_length(hle@category_list, 0)
})

test_that("HesperListEnhanced accepts valid category_list", {
  test_data <- create_test_data()

  # Create valid categories using variables from hesper_list
  cat1 <- HesperCategory(
    cat = "basic_needs",
    vars = c("hesper_drinking_water", "hesper_food")
  )
  cat2 <- HesperCategory(
    cat = "shelter_health",
    vars = c("hesper_shelter", "hesper_health")
  )
  cat3 <- HesperCategory(
    cat = "services",
    vars = c("hesper_education", "hesper_toilet")
  )

  categories <- HesperCategories(category_list = list(cat1, cat2, cat3))

  hle <- HesperListEnhanced(
    hesper_list = test_data$hesper_list,
    SL = test_data$SL,
    other_list = test_data$other_list,
    category_list = list(categories)
  )

  expect_s7_class(hle, HesperListEnhanced)
  expect_length(hle@category_list, 1)
  expect_s7_class(hle@category_list[[1]], HesperCategories)
  expect_length(hle@category_list[[1]]@category_list, 3)
})

test_that("HesperListEnhanced validates maximum one category_list object", {
  test_data <- create_test_data()

  cat1 <- HesperCategory(cat = "basic_needs", vars = c("hesper_drinking_water"))
  cat2 <- HesperCategory(cat = "shelter", vars = c("hesper_shelter"))

  categories1 <- HesperCategories(category_list = list(cat1))
  categories2 <- HesperCategories(category_list = list(cat2))

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = test_data$other_list,
      category_list = list(categories1, categories2)
    ),
    regexp = "Must have length <= 1"
  )
})

test_that("HesperListEnhanced validates category_list contains HesperCategories object", {
  test_data <- create_test_data()

  # Test with non-HesperCategories object in category_list
  fake_category <- list(
    category_list = list(
      list(cat = "fake", vars = c("hesper_drinking_water"))
    )
  )

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = test_data$other_list,
      category_list = list(fake_category)
    ),
    regexp = "Not all items in 'category_list' are of the specified class"
  )
})

test_that("HesperListEnhanced validates category variables are in hesper_list", {
  test_data <- create_test_data()

  # Create categories with variables not in hesper_list
  cat1 <- HesperCategory(
    cat = "basic_needs",
    vars = c("hesper_drinking_water", "hesper_movement") # hesper_movement not in hesper_list
  )
  cat2 <- HesperCategory(
    cat = "missing_vars",
    vars = c("hesper_safety", "hesper_time") # both not in hesper_list
  )

  categories <- HesperCategories(category_list = list(cat1, cat2))

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = test_data$other_list,
      category_list = list(categories)
    ),
    regexp = "are missing in hesper_list: hesper_movement, hesper_safety and hesper_time"
  )
})

test_that("HesperListEnhanced validates single missing category variable", {
  test_data <- create_test_data()

  # Create category with single missing variable
  cat1 <- HesperCategory(
    cat = "basic_needs",
    vars = c("hesper_drinking_water", "hesper_food")
  )
  cat2 <- HesperCategory(
    cat = "missing_var",
    vars = c("hesper_movement") # not in hesper_list
  )

  categories <- HesperCategories(category_list = list(cat1, cat2))

  expect_error(
    HesperListEnhanced(
      hesper_list = test_data$hesper_list,
      SL = test_data$SL,
      other_list = test_data$other_list,
      category_list = list(categories)
    ),
    regexp = "are missing in hesper_list: hesper_movement"
  )
})

test_that("HesperListEnhanced validates HesperCategories object itself", {
  test_data <- create_test_data()

  # Create categories with duplicate category names (should fail HesperCategories validation)
  cat1 <- HesperCategory(cat = "basic_needs", vars = c("hesper_drinking_water"))
  cat2 <- HesperCategory(cat = "basic_needs", vars = c("hesper_food")) # duplicate name

  expect_error(
    {
      categories <- HesperCategories(category_list = list(cat1, cat2))
      HesperListEnhanced(
        hesper_list = test_data$hesper_list,
        SL = test_data$SL,
        other_list = test_data$other_list,
        category_list = list(categories)
      )
    },
    regexp = "category names"
  )
})

test_that("HesperListEnhanced works with single category", {
  test_data <- create_test_data()

  # Create single category
  cat1 <- HesperCategory(
    cat = "all_hesper",
    vars = c(
      "hesper_drinking_water",
      "hesper_food",
      "hesper_shelter",
      "hesper_health",
      "hesper_education",
      "hesper_toilet"
    )
  )

  categories <- HesperCategories(category_list = list(cat1))

  hle <- HesperListEnhanced(
    hesper_list = test_data$hesper_list,
    SL = test_data$SL,
    other_list = test_data$other_list,
    category_list = list(categories)
  )

  expect_s7_class(hle, HesperListEnhanced)
  expect_length(hle@category_list[[1]]@category_list, 1)
  expect_equal(hle@category_list[[1]]@category_list[[1]]@cat, "all_hesper")
  expect_length(hle@category_list[[1]]@category_list[[1]]@vars, 6)
})

test_that("HesperListEnhanced works with categories covering subset of hesper_list", {
  test_data <- create_test_data()

  # Create categories that only cover some variables from hesper_list
  cat1 <- HesperCategory(
    cat = "water_food",
    vars = c("hesper_drinking_water", "hesper_food")
  )
  cat2 <- HesperCategory(
    cat = "services",
    vars = c("hesper_education") # only one variable
  )

  categories <- HesperCategories(category_list = list(cat1, cat2))

  hle <- HesperListEnhanced(
    hesper_list = test_data$hesper_list,
    SL = test_data$SL,
    other_list = test_data$other_list,
    category_list = list(categories)
  )

  expect_s7_class(hle, HesperListEnhanced)
  expect_length(hle@category_list[[1]]@category_list, 2)

  # Check that categories contain expected variables
  all_category_vars <- purrr::map(hle@category_list[[1]]@category_list, \(x) {
    x@vars
  }) |>
    unlist(use.names = FALSE)
  expect_true(all(
    all_category_vars %in%
      c("hesper_drinking_water", "hesper_food", "hesper_education")
  ))
})


# Tests for category_list function ----

test_that("HesperListEnhanced's category_list has correct behavior", {
  hesper_vars <- c("hesper_drinking_water", "hesper_food", "hesper_shelter")
  hesper_opts <- c(
    "serious_problem",
    "no_serious_problem",
    "dnk",
    "pnta",
    "not_applicable"
  )
  hv1 <- HesperVector(
    "hesper_drinking_water",
    c("serious_problem", "serious_problem", "serious_problem")
  )
  hv2 <- HesperVector(
    "hesper_food",
    c("serious_problem", "serious_problem", "serious_problem")
  )
  hv3 <- HesperVector(
    "hesper_shelter",
    c("serious_problem", "serious_problem", "serious_problem")
  )

  cat1 <- HesperCategory(cat = "z_last", vars = c("hesper_shelter"))
  cat2 <- HesperCategory(
    cat = "a_first",
    vars = c("hesper_food", "hesper_drinking_water")
  )
  cat3 <- HesperCategory(cat = "m_middle", vars = c("hesper_health"))

  categories <- HesperCategories(category_list = list(cat1, cat2))

  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3),
    category_list = list(categories)
  )

  cat_list <- hle@category_hesper_list

  # rightly named
  expect_type(cat_list, "list")
  expect_equal(length(cat_list), 2)
  expect_named(
    cat_list,
    c("z_last", "a_first")
  )

  # correct values for all serious_problem
  expect_equal(
    cat_list$z_last$vals,
    c(
      "serious_problem",
      "serious_problem",
      "serious_problem"
    )
  )
  expect_equal(
    cat_list$a_first$vals,
    c(
      "serious_problem",
      "serious_problem",
      "serious_problem"
    )
  )

  # if category list does not exist returns empty list
  hle_no_cat <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3)
  )
  cat_list_no_cat <- hle_no_cat@category_hesper_list
  expect_type(cat_list_no_cat, "list")
  expect_equal(length(cat_list_no_cat), 0)

  # change hv1 and hv2 to no_serious_problem and re-test
  hv1 <- HesperVector(
    "hesper_drinking_water",
    c("no_serious_problem", "no_serious_problem", "no_serious_problem")
  )
  hv2 <- HesperVector(
    "hesper_food",
    c("no_serious_problem", "no_serious_problem", "no_serious_problem")
  )
  hle@hesper_list <- list(hv1, hv2, hv3)
  cat_list <- hle@category_hesper_list
  expect_equal(
    cat_list$a_first$vals,
    c(
      "no_serious_problem",
      "no_serious_problem",
      "no_serious_problem"
    )
  )

  # validates binaries
  expect_equal(cat_list$a_first$bins$no_serious_problem, c(1L, 1L, 1L))
  expect_equal(cat_list$a_first$bins$serious_problem, c(0, 0, 0))

  # validates logic
  # if any is dnk, dnk
  hv1 <- HesperVector(
    "hesper_drinking_water",
    c("dnk", "pnta", "not_applicable")
  )
  hle@hesper_list <- list(hv1, hv2, hv3)
  cat_list <- hle@category_hesper_list
  expect_equal(
    cat_list$a_first$vals,
    c("dnk", "pnta", "not_applicable")
  )
  expect_equal(
    cat_list$a_first$bins$dnk,
    c(1, 0, 0)
  )
  expect_equal(
    cat_list$a_first$bins$pnta,
    c(0, 1, 0)
  )
  expect_equal(
    cat_list$a_first$bins$not_applicable,
    c(0, 0, 1)
  )
})
