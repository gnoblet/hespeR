test_that("from_data_frame converts to HesperList", {
  df <- data.frame(
    hesper_drinking_water = c("serious_problem", "no_serious_problem"),
    hesper_food = c("dnk", "pnta"),
    stringsAsFactors = FALSE
  )
  attr(df, "allow_missing") <- FALSE
  hl <- from_data_frame(
    df,
    hesper_vars = c("hesper_drinking_water", "hesper_food"),
    enhanced = FALSE
  )
  expect_s7_class(hl, HesperList)
  expect_equal(
    purrr::map_chr(hl@hesper_list, function(x) x@hesper_var),
    c("hesper_drinking_water", "hesper_food")
  )
  expect_equal(
    purrr::map(hl@hesper_list, function(x) x@hesper_vals),
    list(c("serious_problem", "no_serious_problem"), c("dnk", "pnta"))
  )
  expect_equal(
    purrr::map_lgl(hl@hesper_list, function(x) x@allow_missing),
    c(TRUE, TRUE)
  )
})

test_that("from_data_frame converts to HesperListEnhanced with other_list", {
  df <- data.frame(
    hesper_drinking_water = c("serious_problem", "no_serious_problem"),
    hesper_food = c("dnk", "pnta"),
    region = c("A", "B"),
    age = c(30, 40),
    stringsAsFactors = FALSE
  )
  attr(df, "allow_missing") <- TRUE
  hle <- from_data_frame(
    df,
    hesper_vars = c("hesper_drinking_water", "hesper_food"),
    enhanced = TRUE
  )
  expect_s7_class(hle, HesperListEnhanced)
  expect_equal(
    purrr::map_chr(hle@hesper_list, function(x) x@hesper_var),
    c("hesper_drinking_water", "hesper_food")
  )
  expect_equal(hle@other_list$region, c("A", "B"))
  expect_equal(hle@other_list$age, c(30, 40))
  expect_equal(
    purrr::map_lgl(hle@hesper_list, function(x) x@allow_missing),
    c(TRUE, TRUE)
  )
})

test_that("from_data_frame error if df is not a data.frame", {
  expect_error(
    from_data_frame(
      df = list(a = 1, b = 2),
      hesper_vars = c("hesper_drinking_water", "hesper_food")
    ),
    "Can't find method for `from_data_frame"
  )
})

test_that("from_data_frame errors when hesper_vars is invalid (empty or columns not in df)", {
  df <- data.frame(
    hesper_drinking_water = c("serious_problem", "no_serious_problem"),
    hesper_food = c("dnk", "pnta"),
    stringsAsFactors = FALSE
  )
  attr(df, "allow_missing") <- TRUE

  expect_error(
    from_data_frame(
      df,
      hesper_vars = character(0)
    ),
    "Assertion on 'hesper_vars' failed"
  )

  expect_error(
    from_data_frame(
      df,
      hesper_vars = c("hesper_drinking_water", "invalid_var")
    ),
    "Assertion on 'hesper_vars' failed"
  )
})

test_that("from_data_frame errors when allow_missing is not a single logical value", {
  df <- data.frame(
    hesper_drinking_water = c("serious_problem", "no_serious_problem"),
    hesper_food = c("dnk", "pnta"),
    stringsAsFactors = FALSE
  )
  attr(df, "allow_missing") <- TRUE

  expect_error(
    from_data_frame(
      df,
      hesper_vars = c("hesper_drinking_water", "hesper_food"),
      allow_missing = c(TRUE, FALSE)
    ),
    "Assertion on 'allow_missing' failed"
  )

  expect_error(
    from_data_frame(
      df,
      hesper_vars = c("hesper_drinking_water", "hesper_food"),
      allow_missing = "yes"
    ),
    "Assertion on 'allow_missing' failed"
  )
})

test_that("from_data_frame errors when other_vars is invalid (part of hesper_vars, not in remaining columns)", {
  df <- data.frame(
    hesper_drinking_water = c("serious_problem", "no_serious_problem"),
    hesper_food = c("dnk", "pnta"),
    region = c("A", "B"),
    age = c(30, 40),
    stringsAsFactors = FALSE
  )
  attr(df, "allow_missing") <- TRUE

  expect_error(
    from_data_frame(
      df,
      hesper_vars = c("hesper_drinking_water", "hesper_food"),
      other_vars = "invalid_var"
    ),
    "Assertion on 'other_vars' failed"
  )

  expect_error(
    from_data_frame(
      df,
      hesper_vars = c("hesper_drinking_water", "hesper_food"),
      other_vars = c("region", "hesper_food")
    ),
    "Assertion on 'other_vars' failed"
  )
})

test_that("from_data_frame get the right other_vars when not provided", {
  df <- data.frame(
    hesper_drinking_water = c("serious_problem", "no_serious_problem"),
    hesper_food = c("dnk", "pnta"),
    region = c("A", "B"),
    age = c(30, 40),
    stringsAsFactors = FALSE
  )
  attr(df, "allow_missing") <- TRUE
  hle <- from_data_frame(
    df,
    hesper_vars = c("hesper_drinking_water", "hesper_food"),
    enhanced = TRUE
  )
  expect_equal(names(hle@other_list), c("region", "age"))
})

test_that("from_data_frame errors when enhanced is not a single logical value", {
  df <- data.frame(
    hesper_drinking_water = c("serious_problem", "no_serious_problem"),
    hesper_food = c("dnk", "pnta"),
    stringsAsFactors = FALSE
  )
  attr(df, "allow_missing") <- TRUE

  expect_error(
    from_data_frame(
      df,
      hesper_vars = c("hesper_drinking_water", "hesper_food"),
      enhanced = c(TRUE, FALSE)
    ),
    "Assertion on 'enhanced' failed"
  )

  expect_error(
    from_data_frame(
      df,
      hesper_vars = c("hesper_drinking_water", "hesper_food"),
      enhanced = "yes"
    ),
    "Assertion on 'enhanced' failed"
  )
})
