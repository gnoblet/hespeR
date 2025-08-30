test_that("as_data_frame works for HesperVector", {
  hv <- HesperVector(
    hesper_var = hesper_vars()[1],
    hesper_vals = hesper_opts()[1:3],
    allow_missing = FALSE
  )
  df <- as_data_frame(hv, bins = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_equal(colnames(df), c("hesper_drinking_water"))
  expect_equal(df$hesper_drinking_water, hesper_opts()[1:3])
  expect_equal(attributes(df)$allow_missing, FALSE)
})

test_that("as_data_frame works with bins = FALSE", {
  hv <- hesper_vars()[1:2]
  ho1 <- hesper_opts()[1:3]
  ho2 <- hesper_opts()[3:5]

  hv1 <- HesperVector(
    hesper_var = hv[1],
    hesper_vals = ho1,
    allow_missing = FALSE
  )
  hv2 <- HesperVector(
    hesper_var = hv[2],
    hesper_vals = ho2,
    allow_missing = FALSE
  )
  hl <- HesperList(hesper_list = list(hv1, hv2))

  df <- as_data_frame(hl, bins = FALSE)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_equal(colnames(df), c('hesper_drinking_water', 'hesper_food'))
  expect_equal(attributes(df)$allow_missing, FALSE)
  expect_equal(
    df$hesper_drinking_water,
    c("serious_problem", "no_serious_problem", "dnk")
  )
  expect_equal(df$hesper_food, c("dnk", "pnta", "not_applicable"))
})

test_that("as_data_frame works with bins = TRUE (default)", {
  hv <- hesper_vars()[1:2]
  ho1 <- hesper_opts()[1:2] # Use fewer options for clearer testing
  ho2 <- hesper_opts()[2:3]

  hv1 <- HesperVector(
    hesper_var = hv[1],
    hesper_vals = ho1,
    allow_missing = FALSE
  )
  hv2 <- HesperVector(
    hesper_var = hv[2],
    hesper_vals = ho2,
    allow_missing = FALSE
  )
  hl <- HesperList(hesper_list = list(hv1, hv2))

  df <- as_data_frame(hl, bins = TRUE)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)

  # Should have original columns plus binary columns
  original_cols <- c('hesper_drinking_water', 'hesper_food')
  all_opts <- hesper_opts()
  binary_cols <- c(
    paste0("hesper_drinking_water.", all_opts),
    paste0("hesper_food.", all_opts)
  )
  expected_cols <- c(original_cols, binary_cols)

  expect_equal(colnames(df), expected_cols)
  expect_equal(attributes(df)$allow_missing, FALSE)

  # Check original columns
  expect_equal(
    df$hesper_drinking_water,
    c("serious_problem", "no_serious_problem")
  )
  expect_equal(df$hesper_food, c("no_serious_problem", "dnk"))

  # Check binary columns for first variable
  expect_equal(df$hesper_drinking_water.serious_problem, c(1L, 0L))
  expect_equal(df$hesper_drinking_water.no_serious_problem, c(0L, 1L))
  expect_equal(df$hesper_drinking_water.dnk, c(0L, 0L))
  expect_equal(df$hesper_drinking_water.pnta, c(0L, 0L))
  expect_equal(df$hesper_drinking_water.not_applicable, c(0L, 0L))

  # Check binary columns for second variable
  expect_equal(df$hesper_food.serious_problem, c(0L, 0L))
  expect_equal(df$hesper_food.no_serious_problem, c(1L, 0L))
  expect_equal(df$hesper_food.dnk, c(0L, 1L))
  expect_equal(df$hesper_food.pnta, c(0L, 0L))
  expect_equal(df$hesper_food.not_applicable, c(0L, 0L))
})

# test_that("from_data_frame works with data without binary columns", {
#   hv <- hesper_vars()[1:2]
#   ho1 <- hesper_opts()[1:3]
#   ho2 <- hesper_opts()[3:5]

#   hv1 <- HesperVector(
#     hesper_var = hv[1],
#     hesper_vals = ho1,
#     allow_missing = FALSE
#   )
#   hv2 <- HesperVector(
#     hesper_var = hv[2],
#     hesper_vals = ho2,
#     allow_missing = FALSE
#   )
#   hl <- HesperList(hesper_list = list(hv1, hv2))

#   df <- as_data_frame(hl, bins = FALSE)
#   hl2 <- from_data_frame(df)

#   expect_s7_class(hl2, HesperList)
#   expect_equal(
#     purrr::map_chr(hl2@hesper_list, \(x) x@hesper_var),
#     purrr::map_chr(hl@hesper_list, \(x) x@hesper_var)
#   )
#   expect_equal(
#     purrr::map(hl2@hesper_list, \(x) x@hesper_vals),
#     purrr::map(hl@hesper_list, \(x) x@hesper_vals)
#   )
#   expect_equal(
#     purrr::map_lgl(hl2@hesper_list, \(x) x@allow_missing),
#     purrr::map_lgl(hl@hesper_list, \(x) x@allow_missing)
#   )
# })

# test_that("from_data_frame errors when allow_missing is missing", {
#   df <- data.frame(
#     hesper_drinking_water = c("serious_problem", "dnk"),
#     hesper_food = c("no_serious_problem", "pnta")
#   )

#   expect_error(
#     from_data_frame(df),
#     "allow_missing must be provided or present as an attribute on df"
#   )
# })

# test_that("from_data_frame correctly ignores binary columns", {
#   hv <- hesper_vars()[1:2]
#   ho1 <- hesper_opts()[1:2]
#   ho2 <- hesper_opts()[2:3]

#   hv1 <- HesperVector(
#     hesper_var = hv[1],
#     hesper_vals = ho1,
#     allow_missing = TRUE
#   )
#   hv2 <- HesperVector(
#     hesper_var = hv[2],
#     hesper_vals = ho2,
#     allow_missing = TRUE
#   )
#   hl_original <- HesperList(hesper_list = list(hv1, hv2))

#   # Convert to data.frame with binary columns
#   df_with_bins <- as_data_frame(hl_original, bins = TRUE)

#   # Convert back to HesperList - should ignore binary columns
#   hl_restored <- from_data_frame(df_with_bins)

#   # Should only have 2 HesperVectors (original variables), not binary columns
#   expect_length(hl_restored@hesper_list, 2)

#   # Check that the restored object matches the original structure
#   expect_equal(
#     purrr::map_chr(hl_restored@hesper_list, \(x) x@hesper_var),
#     purrr::map_chr(hl_original@hesper_list, \(x) x@hesper_var)
#   )
#   expect_equal(
#     purrr::map(hl_restored@hesper_list, \(x) x@hesper_vals),
#     purrr::map(hl_original@hesper_list, \(x) x@hesper_vals)
#   )
#   expect_equal(
#     purrr::map_lgl(hl_restored@hesper_list, \(x) x@allow_missing),
#     purrr::map_lgl(hl_original@hesper_list, \(x) x@allow_missing)
#   )
# })

test_that("as_data_frame works for HesperListEnhanced with other_list", {
  hv <- hesper_vars()[1:2]
  ho1 <- hesper_opts()[1:2]
  ho2 <- hesper_opts()[2:3]

  hv1 <- HesperVector(
    hesper_var = hv[1],
    hesper_vals = ho1,
    allow_missing = FALSE
  )
  hv2 <- HesperVector(
    hesper_var = hv[2],
    hesper_vals = ho2,
    allow_missing = FALSE
  )
  other_list <- list(
    region = c("A", "B"),
    age = c(30, 40)
  )
  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2),
    SL = list(),
    other_list = other_list
  )

  df <- as_data_frame(hle, bins = FALSE)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_true(all(c("region", "age") %in% colnames(df)))
  expect_equal(df$region, c("A", "B"))
  expect_equal(df$age, c(30, 40))
})

test_that("as_data_frame works for HesperListEnhanced with priority_list", {
  hv <- hesper_vars()[1:2]
  ho1 <- hesper_opts()[1:2]
  ho2 <- hesper_opts()[2:3]

  hv1 <- HesperVector(
    hesper_var = hv[1],
    hesper_vals = ho1,
    allow_missing = FALSE
  )
  hv2 <- HesperVector(
    hesper_var = hv[2],
    hesper_vals = ho2,
    allow_missing = FALSE
  )
  priority_list <- list(
    HesperPriorities(
      top1 = c("hesper_drinking_water", NA),
      top2 = c(NA_character_, NA_character_),
      top3 = c(NA_character_, NA_character_),
      allow_missing = TRUE
    )
  )
  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2),
    SL = list(),
    priority_list = priority_list
  )

  df <- as_data_frame(hle, bins = FALSE)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_true(all(c("top1", "top2", "top3") %in% colnames(df)))
  expect_equal(df$top1, c("hesper_drinking_water", NA))
  expect_equal(df$top2, c(NA_character_, NA_character_))
  expect_equal(df$top3, c(NA_character_, NA_character_))
})

test_that("as_data_frame works for HesperListEnhanced for category_list", {
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

  cat1 <- HesperCategory(cat = "z_last", vars = c("hesper_shelter"))
  cat2 <- HesperCategory(
    cat = "a_first",
    vars = c("hesper_food", "hesper_drinking_water")
  )
  cat3 <- HesperCategory(cat = "m_middle", vars = c("hesper_health"))

  categories <- HesperCategories(category_list = list(cat1, cat2))

  hle <- HesperListEnhanced(
    hesper_list = list(hv1, hv2, hv3),
    category_list = list(categories)
  )

  hle_df <- as_data_frame(hle, bins = F)

  expect_equal(nrow(hle_df), 3)
  expect_true(all(c("cat.a_first", "cat.z_last") %in% colnames(hle_df)))
  expect_equal(
    hle_df$cat.a_first,
    c("serious_problem", "serious_problem", "serious_problem")
  )
  expect_equal(
    hle_df$cat.z_last,
    c("serious_problem", "serious_problem", "serious_problem")
  )

  hle_df <- as_data_frame(hle, bins = T)
  expect_equal(nrow(hle_df), 3)
  expect_true(all(
    c(
      "cat.a_first",
      "cat.z_last",
      "cat.a_first.serious_problem",
      "cat.a_first.no_serious_problem",
      "cat.a_first.dnk",
      "cat.a_first.pnta",
      "cat.a_first.not_applicable",
      "cat.z_last.serious_problem",
      "cat.z_last.no_serious_problem",
      "cat.z_last.dnk",
      "cat.z_last.pnta",
      "cat.z_last.not_applicable"
    ) %in%
      colnames(hle_df)
  ))
  expect_equal(
    hle_df$cat.a_first,
    c("serious_problem", "serious_problem", "serious_problem")
  )
  expect_equal(
    hle_df$cat.z_last,
    c("serious_problem", "serious_problem", "serious_problem")
  )
  expect_equal(hle_df$cat.a_first.serious_problem, c(1L, 1L, 1L))
  expect_equal(hle_df$cat.a_first.no_serious_problem, c(0L, 0L, 0L))
  expect_equal(hle_df$cat.a_first.dnk, c(0L, 0L, 0L))
  expect_equal(hle_df$cat.a_first.pnta, c(0L, 0L, 0L))
  expect_equal(hle_df$cat.a_first.not_applicable, c(0L, 0L, 0L))
  expect_equal(hle_df$cat.z_last.serious_problem, c(1L, 1L, 1L))
  expect_equal(hle_df$cat.z_last.no_serious_problem, c(0L, 0L, 0L))
  expect_equal(hle_df$cat.z_last.dnk, c(0L, 0L, 0L))
  expect_equal(hle_df$cat.z_last.pnta, c(0L, 0L, 0L))
  expect_equal(hle_df$cat.z_last.not_applicable, c(0L, 0L, 0L))
})
