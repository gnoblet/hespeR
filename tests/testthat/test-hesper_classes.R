test_that("HesperDefault and HesperEnhanced validate hesper columns only", {
  hesper_levels <- c(
    "serious_problem",
    "no_serious_problem",
    "dnk",
    "pnta",
    "not_applicable"
  )
  hesper_all_items <- c(
    "hesper_drinking_water",
    "hesper_food",
    "hesper_shelter",
    "hesper_toilet",
    "hesper_clean",
    "hesper_clean_female",
    "hesper_clothes_etc",
    "hesper_income_livelihood",
    "hesper_health",
    "hesper_health_care_male",
    "hesper_health_care_female",
    "hesper_distress",
    "hesper_safety",
    "hesper_education",
    "hesper_care",
    "hesper_support",
    "hesper_separation",
    "hesper_displaced",
    "hesper_information",
    "hesper_aid",
    "hesper_respect",
    "hesper_movement",
    "hesper_time",
    "hesper_law",
    "hesper_gbv",
    "hesper_drug",
    "hesper_mental_health",
    "hesper_care_community",
    "hesper_other"
  )
  # Valid
  df <- HesperDefault(
    data = data.frame(
      hesper_drinking_water = c("serious_problem", "dnk"),
      pop_group = c("refugees", "host")
    ),
    hesper_levels = hesper_levels
  )
  expect_s7_class(df, HesperDefault)
  # Invalid hesper column
  expect_error(HesperDefault(
    data = data.frame(
      hesper_drinking_water = c("foo", "dnk"),
      pop_group = c("refugees", "host")
    ),
    hesper_levels = hesper_levels
  ))
  # HesperEnhanced allows metadata columns
  df2 <- HesperEnhanced(
    data = data.frame(
      hesper_drinking_water = c("serious_problem", "dnk"),
      pop_group = c("refugees", "host")
    ),
    hesper_levels = hesper_levels
  )
  expect_s7_class(df2, HesperEnhanced)
})

test_that("All HesperDefault columns must be in hesper_all_items", {
  hesper_levels <- c(
    "serious_problem",
    "no_serious_problem",
    "dnk",
    "pnta",
    "not_applicable"
  )
  hesper_all_items <- c(
    "hesper_drinking_water",
    "hesper_food",
    "hesper_shelter"
  )
  # All columns valid
  df <- HesperDefault(
    data = data.frame(
      hesper_drinking_water = c("serious_problem", "dnk"),
      hesper_food = c("no_serious_problem", "serious_problem"),
      hesper_shelter = c("dnk", "pnta")
    ),
    hesper_opts = hesper_levels
  )
  qexpect_s7_class(df, HesperDefault)
  # One column not in hesper_all_items
  expect_s7_error(
    HesperDefault(
      data = data.frame(
        hesper_drinking_water = c("serious_problem", "dnk"),
        hesper_fake = c("no_serious_problem", "serious_problem")
      ),
      hesper_levels = hesper_levels
    ),
    regexp = "Invalid values in columns"
  )
})

test_that("All HesperDefault values must be in hesper_levels", {
  hesper_levels <- c(
    "serious_problem",
    "no_serious_problem",
    "dnk",
    "pnta",
    "not_applicable"
  )
  hesper_all_items <- c(
    "hesper_drinking_water",
    "hesper_food"
  )
  # All values valid
  df <- HesperDefault(
    data = data.frame(
      hesper_drinking_water = c("serious_problem", "dnk"),
      hesper_food = c("no_serious_problem", "serious_problem")
    ),
    hesper_levels = hesper_levels
  )
  expect_s7_class(df, HesperDefault)
  # Value not in hesper_levels
  expect_error(
    HesperDefault(
      data = data.frame(
        hesper_drinking_water = c("serious_problem", "foo"),
        hesper_food = c("no_serious_problem", "serious_problem")
      ),
      hesper_levels = hesper_levels
    ),
    regexp = "Invalid values in columns"
  )
})
