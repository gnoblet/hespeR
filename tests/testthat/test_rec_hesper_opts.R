# Tests for rec_hesper_opts function

test_that("rec_hesper_opts correctly recodes values", {
  # Simulate a data frame with two HESPER columns using old options
  df <- data.frame(
    hesper_drinking_water = c(
      "problem",
      "ok",
      "unknown",
      "refused",
      "not_applicable"
    ),
    hesper_food = c("ok", "problem", "unknown", "refused", "not_applicable"),
    stringsAsFactors = FALSE
  )
  hesper_vars <- c("hesper_drinking_water", "hesper_food")
  # Map old options to allowed HESPER options
  old_hesper_opts <- c(
    serious_problem = "problem",
    no_serious_problem = "ok",
    dnk = "unknown",
    pnta = "refused",
    not_applicable = "not_applicable"
  )

  # Should recode to allowed values
  df2 <- rec_hesper_opts(df, hesper_vars, old_hesper_opts)
  expect_equal(
    as.character(df2$hesper_drinking_water),
    c("serious_problem", "no_serious_problem", "dnk", "pnta", "not_applicable")
  )
  expect_equal(
    as.character(df2$hesper_food),
    c("no_serious_problem", "serious_problem", "dnk", "pnta", "not_applicable")
  )
})

test_that("rec_hesper_opts errors on invalid old_hesper_opts", {
  df <- data.frame(
    hesper_drinking_water = c("problem", "ok"),
    stringsAsFactors = FALSE
  )
  hesper_vars <- "hesper_drinking_water"
  # Missing one allowed option
  bad_opts <- c(
    serious_problem = "problem",
    no_serious_problem = "ok",
    dnk = "unknown",
    pnta = "refused"
    # not_applicable missing
  )
  expect_error(rec_hesper_opts(df, hesper_vars, bad_opts))
})

test_that("rec_hesper_opts errors if names of old_hesper_opts are not allowed", {
  df <- data.frame(
    hesper_drinking_water = c("problem", "ok"),
    stringsAsFactors = FALSE
  )
  hesper_vars <- "hesper_drinking_water"
  # Invalid name
  bad_opts <- c(
    serious_problem = "problem",
    no_serious_problem = "ok",
    dnk = "unknown",
    pnta = "refused",
    not_allowed = "n/a"
  )
  expect_error(
    rec_hesper_opts(df, hesper_vars, bad_opts)
  )
})

test_that("rec_hesper_opts errors if values in df are not in old_hesper_opts", {
  df <- data.frame(
    hesper_drinking_water = c("problem", "ok", "unexpected"),
    stringsAsFactors = FALSE
  )
  hesper_vars <- "hesper_drinking_water"
  old_hesper_opts <- c(
    serious_problem = "problem",
    no_serious_problem = "ok",
    dnk = "unknown",
    pnta = "refused",
    not_applicable = "n/a"
  )

  expect_error(
    rec_hesper_opts(df, hesper_vars, old_hesper_opts),
    regexp = "Invalid values"
  )
})
