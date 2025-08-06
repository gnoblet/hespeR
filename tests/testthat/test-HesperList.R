# Tests for HesperList S7 class

# ---- Test Data Setup ----
hesper_vars <- c("hesper_drinking_water", "hesper_food")
hesper_opts <- c(
  "serious_problem",
  "no_serious_problem",
  "dnk",
  "pnta",
  "not_applicable"
)

test_that("HesperList accepts a valid vector of HesperVector objects", {
  hv1 <- HesperVector(hesper_vars[1], hesper_opts[1:3])
  hv2 <- HesperVector(hesper_vars[2], hesper_opts[3:5])
  hl <- HesperList(hesper_list = c(hv1, hv2))
  expect_s7_class(hl, HesperList)
  expect_length(hl@hesper_list, 2)
  expect_s7_class(hl@hesper_list[[1]], HesperVector)
})

test_that("HesperList errors if hesper_list is empty", {
  expect_error(
    HesperList(hesper_list = vector("list", 0)),
    regexp = "Must have length >= 1"
  )
})

test_that("HesperList errors if any item is not a HesperVector", {
  hv1 <- HesperVector(hesper_vars[1], hesper_opts[1:2])
  not_hv <- list(a = 1, b = 2)
  expect_error(
    HesperList(hesper_list = c(hv1, not_hv)),
    regexp = "Not all items in 'hesper_list' are of the specified class."
  )
})

test_that("HesperList errors if there are duplicate hesper_var names", {
  hv1 <- HesperVector(hesper_vars[1], hesper_opts[1:2])
  hv2 <- HesperVector(hesper_vars[1], hesper_opts[3:5]) # duplicate hesper_var
  expect_error(
    HesperList(hesper_list = c(hv1, hv2)),
    regexp = "Duplicate values found in"
  )
})

test_that("HesperList works with a single HesperVector", {
  hv1 <- HesperVector(hesper_vars[1], hesper_opts[1:2])
  hl <- HesperList(hesper_list = list(hv1))
  expect_s7_class(hl, HesperList)
  expect_length(hl@hesper_list, 1)
})

# Edge case: NA in hesper_vals if allowed (handled by HesperVector anyway)
test_that("HesperList accepts HesperVector with NA in hesper_vals if NA is allowed", {
  hv1 <- HesperVector(
    hesper_vars[1],
    c(hesper_opts[1], NA),
    allow_missing = TRUE
  )
  hv2 <- HesperVector(hesper_vars[2], hesper_opts[2:3], allow_missing = TRUE)
  expect_silent(HesperList(hesper_list = c(hv1, hv2)))
})

# If fake HesperVector is used, it should throw an error
test_that("HesperList errors if a fake HesperVector is used", {
  fake_hv <- list(hesper_var = "fake_var", hesper_vals = c("val1", "val2"))
  expect_error(
    HesperList(hesper_list = list(fake_hv)),
    regexp = "Not all items in 'hesper_list' are of the specified class."
  )
})
# even if given the right classm fake_hv with wrong values shouldn't pass the validator
test_that("HesperList errors if a fake HesperVector with class is used", {
  fake_HesperVector <- S7::new_class(
    "hespeR::HesperVector",
    properties = list(
      hesper_var = S7::class_character,
      hesper_vals = S7::class_character,
      allow_missing = S7::class_logical
    )
  )
  fake_hv <- fake_HesperVector(
    hesper_var = "fake_var",
    hesper_vals = c("val1", "val2"),
    allow_missing = FALSE
  )

  expect_error(
    HesperList(hesper_list = list(fake_hv)),
    regexp = "Not all items in 'hesper_list' are of the specified class."
  )
})
