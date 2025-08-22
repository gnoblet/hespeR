test_that("hesper_vars returns all allowed variables by default", {
  vars <- hesper_vars()
  expect_type(vars, "character")
  expect_length(vars, 29)
  expect_true(all(grepl("^hesper_", vars)))
})

test_that("hesper_vars validates input and returns only requested variables", {
  expect_equal(hesper_vars("hesper_food"), "hesper_food")
  expect_equal(
    sort(hesper_vars("hesper_food", "hesper_drinking_water")),
    sort(c("hesper_food", "hesper_drinking_water"))
  )
})

test_that("hesper_vars errors on duplicates", {
  expect_error(
    hesper_vars("hesper_food", "hesper_food"),
    "Duplicated variable names provided"
  )
})

test_that("hesper_vars errors on invalid names", {
  expect_error(hesper_vars("not_a_var"), "Invalid variable names provided")
})

test_that("hesper_opts returns all allowed options by default", {
  opts <- hesper_opts()
  expect_type(opts, "character")
  expect_length(opts, 5)
  expect_true(all(
    opts %in%
      c(
        "serious_problem",
        "no_serious_problem",
        "dnk",
        "pnta",
        "not_applicable"
      )
  ))
})

test_that("hesper_opts validates input and returns only requested options", {
  expect_equal(hesper_opts("serious_problem"), "serious_problem")
  expect_equal(
    sort(hesper_opts("serious_problem", "dnk")),
    sort(c("serious_problem", "dnk"))
  )
})

test_that("hesper_opts errors on duplicates", {
  expect_error(
    hesper_opts("serious_problem", "serious_problem"),
    "Duplicated options provided"
  )
})

test_that("hesper_opts errors on invalid options", {
  expect_error(hesper_opts("not_an_option"), "Invalid options provided")
})
