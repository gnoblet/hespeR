# test-HesperEnhanced.R
# Dedicated tests for the HesperEnhanced S7 class

library(testthat)
library(hespeR)

# ---- Test Data Setup ----
hesper_vars <- c("hesper1", "hesper2")
hesper_opts <- c("Yes", "No")

df_valid <- data.frame(
  hesper1 = c("Yes", "No"),
  hesper2 = c("No", "Yes"),
  pop_group = c("Refugee", "Host"),
  site = c("A", "B"),
  stringsAsFactors = FALSE
)

df_invalid_hesper <- data.frame(
  hesper1 = c("Yes", "No"),
  hesper2 = c("No", "Maybe"), # "Maybe" is not a valid hesper option
  pop_group = c("Refugee", "Host"),
  site = c("A", "B"),
  stringsAsFactors = FALSE
)

# ---- SL Setup ----
sl_valid <- SL(
  hesper_var = "hesper1",
  subset_var = "pop_group",
  subset_vals = "Host"
)

sl_invalid_subset_var <- SL(
  hesper_var = "hesper1",
  subset_var = "not_in_df",
  subset_vals = "foo"
)

sl_duplicate <- list(
  SL(hesper_var = "hesper1", subset_var = "pop_group", subset_vals = "Host"),
  SL(hesper_var = "hesper1", subset_var = "pop_group", subset_vals = "Refugee")
)

sl_type_mismatch <- SL(
  hesper_var = "hesper1",
  subset_var = "site",
  subset_vals = 1 # site is character, but subset_vals is numeric
)

# ---- Tests ----

test_that("HesperEnhanced validates with valid df, extra_cols, and SL list", {
  he <- HesperEnhanced(
    df = df_valid,
    extra_cols = c("pop_group", "site"),
    sl_list = list(sl_valid)
  )
  expect_s3_class(he, "HesperEnhanced")
})

test_that("HesperEnhanced infers extra columns if not provided", {
  he <- HesperEnhanced(
    df = df_valid,
    sl_list = list(sl_valid)
  )
  expect_s3_class(he, "HesperEnhanced")
})

test_that("HesperEnhanced fails if extra_cols are missing from df", {
  expect_error(
    HesperEnhanced(
      df = df_valid,
      extra_cols = c("pop_group", "missing_col"),
      sl_list = list(sl_valid)
    ),
    "Missing extra columns"
  )
})

test_that("HesperEnhanced fails if SL list contains non-SL objects", {
  expect_error(
    HesperEnhanced(
      df = df_valid,
      extra_cols = c("pop_group", "site"),
      sl_list = list("not_an_SL")
    ),
    "All elements of sl_list must be S7 SL objects"
  )
})

test_that("HesperEnhanced fails if SL subset_var not in extra columns", {
  expect_error(
    HesperEnhanced(
      df = df_valid,
      extra_cols = c("pop_group", "site"),
      sl_list = list(sl_invalid_subset_var)
    ),
    "subset_var 'not_in_df' in SL not found in extra columns"
  )
})

test_that("HesperEnhanced fails if SL list has duplicate subset_var", {
  expect_error(
    HesperEnhanced(
      df = df_valid,
      extra_cols = c("pop_group", "site"),
      sl_list = sl_duplicate
    ),
    "Duplicate subset_var in SL list"
  )
})

test_that("HesperEnhanced fails if SL list has duplicate hesper_var", {
  # Create two SLs with same hesper_var but different subset_var
  sl_dup_hesper <- list(
    SL(hesper_var = "hesper1", subset_var = "pop_group", subset_vals = "Host"),
    SL(hesper_var = "hesper1", subset_var = "site", subset_vals = "A")
  )
  expect_error(
    HesperEnhanced(
      df = df_valid,
      extra_cols = c("pop_group", "site"),
      sl_list = sl_dup_hesper
    ),
    "Duplicate hesper_var in SL list"
  )
})

test_that("HesperEnhanced fails if SL subset_var type mismatches df column", {
  expect_error(
    HesperEnhanced(
      df = df_valid,
      extra_cols = c("pop_group", "site"),
      sl_list = list(sl_type_mismatch)
    ),
    "Type mismatch for subset_var 'site'"
  )
})

test_that("HesperEnhanced fails if HESPER columns contain invalid values", {
  expect_error(
    HesperEnhanced(
      df = df_invalid_hesper,
      extra_cols = c("pop_group", "site")
    ),
    "Invalid values in columns"
  )
})

test_that("HesperEnhanced works with factor extra columns and character subset_vals", {
  df_factor <- df_valid
  df_factor$pop_group <- factor(df_factor$pop_group)
  sl_factor <- SL(
    hesper_var = "hesper1",
    subset_var = "pop_group",
    subset_vals = "Host"
  )
  he <- HesperEnhanced(
    df = df_factor,
    extra_cols = c("pop_group", "site"),
    sl_list = list(sl_factor)
  )
  expect_s3_class(he, "HesperEnhanced")
})

# Add more edge case tests as needed!
