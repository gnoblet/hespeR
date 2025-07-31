# test_that("add_hesper_column works for HesperDefault and HesperEnhanced", {
#   hesper_levels <- c(
#     "serious_problem",
#     "no_serious_problem",
#     "dnk",
#     "pnta",
#     "not_applicable"
#   )
#   df <- HesperDefault(
#     data = list(hesper_drinking_water = c("serious_problem", "dnk")),
#     hesper_levels = hesper_levels
#   )
#   df2 <- add_hesper_column(df, "hesper_food", c("no_serious_problem", "pnta"))
#   expect_true("hesper_food" %in% names(df2@data))
#   # HesperEnhanced
#   df3 <- HesperEnhanced(
#     data = list(hesper_drinking_water = c("serious_problem", "dnk")),
#     hesper_levels = hesper_levels
#   )
#   df4 <- add_hesper_column(df3, "hesper_food", c("no_serious_problem", "pnta"))
#   expect_true("hesper_food" %in% names(df4@data))
# })

# test_that("apply_skip_logic sets NA correctly", {
#   hesper_levels <- c(
#     "serious_problem",
#     "no_serious_problem",
#     "dnk",
#     "pnta",
#     "not_applicable"
#   )
#   df <- HesperDefault(
#     data = list(
#       hesper_displaced = c("serious_problem", "dnk", "no_serious_problem"),
#       pop_group = c("refugees", "host", "idp")
#     ),
#     hesper_levels = hesper_levels
#   )
#   sl <- SL(
#     hesper_var = "hesper_displaced",
#     subset_var = "pop_group",
#     subset_vals = c("refugees", "idp")
#   )
#   df2 <- apply_skip_logic(df, sl)
#   expect_equal(
#     df2@data$hesper_displaced,
#     c("serious_problem", NA, "no_serious_problem")
#   )
# })
