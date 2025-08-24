test_that("rec_hesper_vars renames columns correctly", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  # Simulate hesper_vars() returning allowed names
  old_hesper_vars <- c(hesper_food = "a", hesper_drinking_water = "b")
  df2 <- rec_hesper_vars(df, old_hesper_vars)
  expect_equal(colnames(df2), c("hesper_food", "hesper_drinking_water", "c"))
  expect_equal(df2$hesper_food, 1:3)
  expect_equal(df2$hesper_drinking_water, 4:6)
})

test_that("rec_hesper_vars errors with invalid input", {
  df <- data.frame(a = 1:3, b = 4:6)
  assign("hesper_vars", function() c("hesper1", "hesper2"), envir = .GlobalEnv)
  expect_error(rec_hesper_vars(df, c(hesper1 = "x")), "subset")
  expect_error(rec_hesper_vars(df, c(notallowed = "a")), "old_hesper_vars")
  rm(hesper_vars, envir = .GlobalEnv)
})
