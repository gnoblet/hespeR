test_that("apply_hesper_list_sl returns cleaned hesper_list", {
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
  hlf <- apply_hesper_list_sl(hle)

  expect_s7_class(hlf, HesperListEnhanced)
  expect_equal(length(hlf@hesper_list), 3)

  # Check that vectorsare cleaned
  expect_equal(
    hlf@hesper_list[[1]]@hesper_vals,
    c("serious_problem", NA, "dnk")
  )
  expect_equal(
    hlf@hesper_list[[2]]@hesper_vals,
    c(NA, "dnk", NA)
  )
  expect_equal(
    hlf@hesper_list[[3]]@hesper_vals,
    c("serious_problem", "no_serious_problem", "dnk")
  )

  # Check that other_list is unchanged
  expect_equal(hlf@other_list$pop_group, c("refugees", "host", "refugees"))

  # Check that binaries got cleaned too
  expect_equal(
    hlf@hesper_list[[1]]@hesper_bins$serious_problem,
    c(1, NA, 0)
  )
  expect_equal(
    hlf@hesper_list[[2]]@hesper_bins$no_serious_problem,
    c(NA, 0, NA)
  )
  expect_equal(
    hlf@hesper_list[[3]]@hesper_bins$dnk,
    c(0, 0, 1)
  )
})
