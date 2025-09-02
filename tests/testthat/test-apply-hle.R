test_that("providing SL cleans out hesper_list", {
  hesper_vars <- c("hesper_drinking_water", "hesper_food", "hesper_shelter")
  hesper_opts <- c(
    "serious_problem",
    "no_serious_problem",
    "dnk",
    "pnta",
    "not_applicable"
  )
  hv1 <- HesperVector("hesper_drinking_water", hesper_opts[1:3])
  hv2 <- HesperVector("hesper_food", hesper_opts[2:4])
  hv3 <- HesperVector("hesper_shelter", hesper_opts[1:3])
  other_list <- list(
    pop_group = c("refugees", "host", "refugees"),
    household_id = c("hh001", "hh002", "hh003")
  )
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
  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3),
    SL = list(sl1, sl2),
    other_list = other_list
  )

  hl <- list(hv1, hv2, hv3)

  expect_s7_class(hle, HesperListEnhanced)
  expect_equal(length(hle@hesper_list_pp), 3)

  # Check that vectors are cleaned
  expect_equal(
    hle@hesper_list_pp[[1]]@hesper_vals,
    c("serious_problem", NA, "dnk")
  )
  expect_equal(
    hle@hesper_list_pp[[2]]@hesper_vals,
    c(NA, "dnk", NA)
  )
  expect_equal(
    hle@hesper_list_pp[[3]]@hesper_vals,
    c("serious_problem", "no_serious_problem", "dnk")
  )

  # Check that other_list is unchanged
  expect_equal(hle@other_list$pop_group, c("refugees", "host", "refugees"))

  # Check that binaries got cleaned too
  expect_equal(
    hle@hesper_list_pp[[1]]@hesper_bins$serious_problem,
    c(1, NA, 0)
  )
  expect_equal(
    hle@hesper_list_pp[[2]]@hesper_bins$no_serious_problem,
    c(NA, 0, NA)
  )
  expect_equal(
    hle@hesper_list_pp[[3]]@hesper_bins$dnk,
    c(0, 0, 1)
  )
})


# self is returned if no SL objects are present
test_that("apply_hle_hesper_list_sl returns self if no SL objects", {
  hesper_vars <- c("hesper_drinking_water", "hesper_food", "hesper_shelter")
  hesper_opts <- c(
    "serious_problem",
    "no_serious_problem",
    "dnk",
    "pnta",
    "not_applicable"
  )
  hv1 <- HesperVector("hesper_drinking_water", hesper_opts[1:3])
  hv2 <- HesperVector("hesper_food", hesper_opts[2:4])
  hv3 <- HesperVector("hesper_shelter", hesper_opts[1:3])
  other_list <- list(
    pop_group = c("refugees", "host", "refugees"),
    household_id = c("hh001", "hh002", "hh003")
  )
  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3),
    other_list = other_list
  )

  expect_identical(hle@hesper_list, list(hv1, hv2, hv3))
})

# Tests for apply_hle_priority_list_sl function

test_that("apply_hle_priority_list_sl cleans priorities with upcycle cascading", {
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

  # Create priorities where some will be affected by skip logic
  priorities <- HesperPriorities(
    top1 = c("hesper_drinking_water", "hesper_drinking_water", "hesper_food"),
    top2 = c("hesper_food", "hesper_shelter", "hesper_shelter"),
    top3 = c("hesper_shelter", "hesper_food", "hesper_drinking_water"),
    allow_missing = TRUE
  )

  other_list <- list(
    pop_group = c("refugees", "host", "refugees"),
    household_id = c("hh001", "hh002", "hh003")
  )

  # Skip logic: host population shouldn't have drinking water issues
  # refugees shouldn't have food issues
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

  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3),
    priority_list = list(priorities),
    SL = list(sl1, sl2),
    other_list = other_list
  )

  # Check that priorities were cleaned
  expect_s7_class(hle@priority_list_pp[[1]], HesperPriorities)

  # Respondent 1 (refugees): food should be removed from priorities (top2) and cascade
  expect_equal(hle@priority_list_pp[[1]]@top1[1], "hesper_drinking_water") # unchanged
  expect_equal(hle@priority_list_pp[[1]]@top2[1], "hesper_shelter") # cascaded from top3
  expect_equal(hle@priority_list_pp[[1]]@top3[1], NA_character_) # food removed, shelter moved up

  # Respondent 2 (host): drinking_water should be removed and cascade
  expect_equal(hle@priority_list_pp[[1]]@top1[2], "hesper_shelter") # cascaded from top2
  expect_equal(hle@priority_list_pp[[1]]@top2[2], "hesper_food") # cascaded from top3
  expect_equal(hle@priority_list_pp[[1]]@top3[2], NA_character_) # drinking_water removed, others cascaded

  # Respondent 3 (refugees): food should be removed from top1 and cascade
  expect_equal(hle@priority_list_pp[[1]]@top1[3], "hesper_shelter") # cascaded from top2
  expect_equal(hle@priority_list_pp[[1]]@top2[3], "hesper_drinking_water") # cascaded from top3
  expect_equal(hle@priority_list_pp[[1]]@top3[3], NA_character_) # food removed, others cascaded

  # Check that hesper_list was not modified (only priorities should be affected)
  expect_equal(
    hle@hesper_list[[1]]@hesper_vals,
    hle@hesper_list[[1]]@hesper_vals
  )
  expect_equal(
    hle@hesper_list[[2]]@hesper_vals,
    hle@hesper_list[[2]]@hesper_vals
  )
  expect_equal(
    hle@hesper_list[[3]]@hesper_vals,
    hle@hesper_list[[3]]@hesper_vals
  )
})

test_that("apply_hle_priority_list_sl returns unchanged object when no SL rules", {
  hesper_vars <- c("hesper_drinking_water", "hesper_food")
  hesper_opts <- c("serious_problem", "no_serious_problem", "dnk")
  hv1 <- HesperVector("hesper_drinking_water", rep("serious_problem", 2))
  hv2 <- HesperVector("hesper_food", rep("serious_problem", 2))
  hv3 <- HesperVector("hesper_shelter", rep("serious_problem", 2))

  priorities <- HesperPriorities(
    top1 = c("hesper_drinking_water", "hesper_food"),
    top2 = c("hesper_food", NA_character_),
    top3 = c(NA_character_, NA_character_),
    allow_missing = TRUE
  )

  other_list <- list(
    pop_group = c("refugees", "host"),
    household_id = c("hh001", "hh002")
  )

  # Create HesperListEnhanced without SL rules
  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3),
    priority_list = list(priorities),
    other_list = other_list
  )

  expect_identical(hle@priority_list, hle@priority_list_pp)
})

test_that("apply_hle_priority_list_sl returns unchanged object when no priorities", {
  hesper_vars <- c("hesper_drinking_water", "hesper_food")
  hesper_opts <- c("serious_problem", "no_serious_problem", "dnk")
  hv1 <- HesperVector("hesper_drinking_water", hesper_opts[1:2])
  hv2 <- HesperVector("hesper_food", hesper_opts[2:3])

  other_list <- list(
    pop_group = c("refugees", "host"),
    household_id = c("hh001", "hh002")
  )

  sl1 <- SL(
    hesper_var = "hesper_drinking_water",
    subset_var = "pop_group",
    subset_vals = c("host")
  )

  # Create HesperListEnhanced without priority_list
  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2),
    SL = list(sl1),
    other_list = other_list
  )

  expect_identical(hle@priority_list, hle@priority_list_pp)
})

test_that("apply_hle_priority_list_sl preserves binary vectors with upcycle cascading", {
  hesper_vars <- c("hesper_drinking_water", "hesper_food")
  hesper_opts <- c("serious_problem", "no_serious_problem")
  hv1 <- HesperVector("hesper_drinking_water", hesper_opts)
  hv2 <- HesperVector("hesper_food", hesper_opts)
  hv3 <- HesperVector("hesper_shelter", hesper_opts)

  priorities <- HesperPriorities(
    top1 = c("hesper_drinking_water", NA_character_),
    top2 = c("hesper_food", NA_character_),
    top3 = c("hesper_shelter", NA_character_),
    allow_missing = TRUE
  )

  other_list <- list(
    pop_group = c("host", "refugees")
  )

  sl1 <- SL(
    hesper_var = "hesper_drinking_water",
    subset_var = "pop_group",
    subset_vals = c("host")
  )

  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3),
    priority_list = list(priorities),
    SL = list(sl1),
    other_list = other_list
  )

  # Check that binary vectors are properly updated after cascading
  # Respondent 1: drinking_water removed, food moves to top1, shelter moves to top2
  # Respondent 2: no priorities, all remain NA
  expect_equal(
    hle@priority_list_pp[[1]]@top1_bins$hesper_drinking_water,
    c(0L, NA_integer_)
  )
  expect_equal(
    hle@priority_list_pp[[1]]@top1_bins$hesper_food,
    c(1L, NA_integer_)
  )
  expect_equal(
    hle@priority_list_pp[[1]]@top2_bins$hesper_drinking_water,
    c(0L, NA_integer_)
  )
  expect_equal(
    hle@priority_list_pp[[1]]@top2_bins$hesper_food,
    c(0L, NA_integer_)
  )
  expect_equal(
    hle@priority_list_pp[[1]]@top2_bins$hesper_shelter,
    c(1L, NA_integer_)
  )
  expect_equal(
    hle@priority_list_pp[[1]]@top3_bins$hesper_drinking_water,
    c(NA_integer_, NA_integer_)
  )
  expect_equal(
    hle@priority_list_pp[[1]]@top3_bins$hesper_food,
    c(NA_integer_, NA_integer_)
  )
  expect_equal(
    hle@priority_list_pp[[1]]@top3_bins$hesper_shelter,
    c(NA_integer_, NA_integer_)
  )
})

# Tests for apply_sl function

test_that("apply_sl cleans both hesper_list and priority_list", {
  hesper_vars <- c("hesper_drinking_water", "hesper_food", "hesper_shelter")
  hesper_opts <- c(
    "serious_problem",
    "no_serious_problem",
    "dnk",
    "pnta",
    "not_applicable"
  )
  hv1 <- HesperVector("hesper_drinking_water", rep(hesper_opts[1], 3))
  hv2 <- HesperVector("hesper_food", rep(hesper_opts[1], 3))
  hv3 <- HesperVector("hesper_shelter", hesper_opts[1:3])
  hv4 <- HesperVector("hesper_health", hesper_opts[1:3])
  hv5 <- HesperVector("hesper_education", hesper_opts[1:3])
  hv6 <- HesperVector("hesper_safety", hesper_opts[1:3])

  # Create priorities where some will be affected by skip logic
  priorities <- HesperPriorities(
    top1 = c("hesper_drinking_water", "hesper_food", "hesper_drinking_water"),
    top2 = c("hesper_food", "hesper_drinking_water", "hesper_food"),
    top3 = c("hesper_shelter", NA_character_, NA_character_),
    allow_missing = TRUE
  )

  other_list <- list(
    pop_group = c("refugees", "host", "refugees"),
    household_id = c("hh001", "hh002", "hh003")
  )

  # Skip logic: host population shouldn't have drinking water issues
  # refugees shouldn't have food issues
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

  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3, hv4, hv5, hv6),
    priority_list = list(priorities),
    SL = list(sl1, sl2),
    other_list = other_list
  )

  # Check that both hesper_list and priorities were cleaned
  expect_s7_class(hle, HesperListEnhanced)
  expect_s7_class(hle@priority_list_pp[[1]], HesperPriorities)

  # Check hesper_list cleaning
  expect_equal(
    hle@hesper_list_pp[[1]]@hesper_vals,
    c("serious_problem", NA, "serious_problem")
  )
  expect_equal(
    hle@hesper_list_pp[[2]]@hesper_vals,
    c(NA, "serious_problem", NA)
  )
  expect_equal(
    hle@hesper_list_pp[[3]]@hesper_vals,
    c("serious_problem", "no_serious_problem", "dnk")
  )

  # Check priorities cleaning with cascading
  # Respondent 1 (refugees): food should be removed and cascade
  expect_equal(hle@priority_list_pp[[1]]@top1[1], "hesper_drinking_water") # unchanged
  expect_equal(hle@priority_list_pp[[1]]@top2[1], "hesper_shelter") # cascaded from top3
  expect_equal(hle@priority_list_pp[[1]]@top3[1], NA_character_) # food removed, shelter moved up

  # Respondent 2 (host): drinking_water should be removed and cascade
  expect_equal(hle@priority_list_pp[[1]]@top1[2], "hesper_food") # unchanged
  expect_equal(hle@priority_list_pp[[1]]@top2[2], NA_character_) # drinking_water removed, no more priorities
  expect_equal(hle@priority_list_pp[[1]]@top3[2], NA_character_) # unchanged

  # Respondent 3 (refugees): food should be removed and cascade
  expect_equal(hle@priority_list_pp[[1]]@top1[3], "hesper_drinking_water") # unchanged
  expect_equal(hle@priority_list_pp[[1]]@top2[3], NA_character_) # food removed, no more priorities
  expect_equal(hle@priority_list_pp[[1]]@top3[3], NA_character_) # unchanged

  # Check that binary vectors are consistent
  expect_equal(
    hle@hesper_list_pp[[1]]@hesper_bins$serious_problem,
    c(1, NA, 1)
  )
  # Check that binary vectors are consistent
  expect_equal(
    hle@priority_list_pp[[1]]@top1_bins$hesper_drinking_water,
    c(1L, 0L, 1L)
  )
})

test_that("apply_sl handles missing components gracefully", {
  hesper_vars <- c("hesper_drinking_water", "hesper_food")
  hesper_opts <- c("serious_problem", "no_serious_problem", "dnk")
  hv1 <- HesperVector("hesper_drinking_water", hesper_opts[1:2])
  hv2 <- HesperVector("hesper_food", hesper_opts[2:3])

  other_list <- list(
    pop_group = c("refugees", "host"),
    household_id = c("hh001", "hh002")
  )

  sl1 <- SL(
    hesper_var = "hesper_drinking_water",
    subset_var = "pop_group",
    subset_vals = c("host")
  )

  # Create HesperListEnhanced without priority_list
  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2),
    SL = list(sl1),
    other_list = other_list
  )

  # Should clean hesper_list

  expect_s7_class(hle, HesperListEnhanced)
  expect_equal(length(hle@priority_list_pp), 0)

  # Check that hesper_list was still cleaned
  expect_equal(
    hle@hesper_list_pp[[1]]@hesper_vals,
    c("serious_problem", NA)
  )
})
