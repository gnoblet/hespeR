test_that("HesperVector.get returns correct properties", {
  hv <- HesperVector(
    hesper_var = "hesper_food",
    hesper_vals = c("serious_problem", "no_serious_problem", "dnk"),
    allow_missing = TRUE
  )
  expect_equal(HesperVector.get(hv, "hesper_var"), "hesper_food")
  expect_equal(HesperVector.get(hv, "hesper_vals"), c("serious_problem", "no_serious_problem", "dnk"))
  expect_equal(HesperVector.get(hv, "allow_missing"), TRUE)
  bins <- HesperVector.get(hv, "hesper_bins")
  expect_type(bins, "list")
  expect_true(all(names(bins) %in% hesper_opts()))
  expect_equal(bins[["serious_problem"]], c(1L, 0L, 0L))
})

test_that("HesperVector.get errors on invalid property", {
  hv <- HesperVector(
    hesper_var = "hesper_food",
    hesper_vals = c("serious_problem"),
    allow_missing = TRUE
  )
  expect_error(HesperVector.get(hv, "not_a_property"), "assert_subset")
})
