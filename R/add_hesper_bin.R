add_hesper_bin <- function(
  df,
  hesper_vars = c(
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
  ),
  hesper_serious_problem = "serious_problem",
  hesper_no_serious_problem = "no_serious_problem",
  hesper_dnk = "dnk",
  hesper_pnta = "pnta",
  hesper_na = "not_applicable",
  sv = TRUE,
  sv_l = list(
    displaced = list(
      hesper_vars = c("hesper_displaced"),
      subset_var = "pop_group",
      subset_vals = c("refugees", "idp")
    ),
    resp_gender_female = list(
      hesper_vars = c("hesper_clean_female"),
      subset_var = "resp_gender",
      subset_vals = c("female")
    )
  ),
  stop_if_subset_no_match = T,
  all_bin = F
) {
  #------ Checks

  # df is a dataframe
  checkmate::assertDataFrame(df)

  # df is not data.table, convert it
  if (!checkmate::testDataTable(df)) {
    rlang::warn("Converting df to data.table.")
    data.table::setDT(df)
  }

  # create a shallow copy of df
  df <- copy(df)

  # hesper_vars is a character vector
  checkmate::assertCharacter(hesper_vars, min.chars = 1, any.missing = FALSE)

  # warning from deviations from defaults
  if (length(hesper_vars) > 29) {
    rlang::warn(
      "More than 29 hesper_vars provided. Make sure you are using the correct variable names."
    )
  }
  if (length(hesper_vars) < 29) {
    rlang::warn(
      "Less than 29 hesper_vars provided. Make sure you are using the correct variable names."
    )
  }

  # hesper_vars are unique
  check_dupes(hesper_vars, "The following vars are duplicated: ")

  # hesper_vars are in df
  check_vars_in_df(df, hesper_vars)

  # hesper_vars are character vars in df
  check_vars_class_in_df(df, hesper_vars, "character")

  # *_problem are character scalar and cannot be NA or NULL
  checkmate::assertCharacter(
    hesper_serious_problem,
    len = 1,
    min.chars = 1,
    any.missing = FALSE
  )
  checkmate::assertCharacter(
    hesper_no_serious_problem,
    len = 1,
    min.chars = 1,
    any.missing = FALSE
  )
  checkmate::assertCharacter(
    hesper_dnk,
    len = 1,
    min.chars = 1,
    any.missing = FALSE
  )
  checkmate::assertCharacter(
    hesper_pnta,
    len = 1,
    min.chars = 1,
    any.missing = FALSE
  )
  checkmate::assertCharacter(
    hesper_na,
    len = 1,
    min.chars = 1,
    any.missing = FALSE
  )

  # hesper_vars only contains values in hesper_serious_problem, hesper_no_serious_problem, hesper_dnk, hesper_pnta and hesper_na and NA_skip
  check_vars_in_set(
    df,
    hesper_vars,
    c(
      hesper_serious_problem,
      hesper_no_serious_problem,
      hesper_dnk,
      hesper_pnta,
      hesper_na,
      "NA_skip"
    )
  )

  # sv is a logical scalar
  checkmate::assertLogical(sv, len = 1, any.missing = FALSE)

  if (sv) {
    # sv_l is a named list
    checkmate::assertList(sv_l, names = "unique")
    # sv_l items has three items only that are named hesper_vars, subset_var and subset_vals:
    check_sv_l(
      sv_l,
      df,
      hesper_vars,
      warn_subset_val_no_match = !stop_if_subset_no_match
    )
  }

  #------ Prepare intermediate variables

  hesper_applicable <- c(
    hesper_serious_problem,
    hesper_no_serious_problem,
    hesper_dnk,
    hesper_pnta
  )
  hesper_any <- c(hesper_serious_problem, hesper_no_serious_problem)
  hesper_exclude <- c(hesper_dnk, hesper_pnta, hesper_na)

  # warn if there is no occurence of hesper_applicable
  # wonder if we, instead, write down something to produce a quick quality checks
  # warn if there is no occurence of hesper_exclude

  #------ Finally, compose

  # Add number of serious problem, etc.
  df <- sum_vals_across(
    df,
    hesper_vars,
    hesper_serious_problem,
    "hesper_serious_problem_n"
  )
  df <- sum_vals_across(
    df,
    hesper_vars,
    hesper_no_serious_problem,
    "hesper_no_serious_problem_n"
  )
  df <- sum_vals_across(df, hesper_vars, hesper_serious_problem, "hesper_any_n")
  df <- sum_vals_across(
    df,
    hesper_vars,
    hesper_applicable,
    "hesper_applicable_n"
  )
  df <- sum_vals_across(df, hesper_vars, hesper_exclude, "hesper_exclude_n")
  df <- sum_vals_across(df, hesper_vars, hesper_dnk, "hesper_dnk_n")
  df <- sum_vals_across(df, hesper_vars, hesper_pnta, "hesper_pnta_n")

  # Expand bin if all_bin=T
  if (all_bin) {
    df <- expand_bin(df, hesper_vars)
  }

  ## This will expand binaries for all hesper items, for all choices (no added value to classic select one analysis?)
  ## Percentages calculated with these binaries would still be over subset excluding skipped respondents + data set to NA during cleaning

  # Iterate over subset
  # if (sv) {
  #   for (sv_el in sv_l) {
  #
  #     df <- recode_subset_to_missing(
  #       df,
  #       vars = sv_el$hesper_vars,
  #       subset_var = sv_el$subset_var,
  #       subset_vals = sv_el$subset_vals,
  #       suffix = "_subset"
  #     )
  #     ## From what I understand, this would clean the var to ensure the skip logic is respected (set NA if respondent is not in the subset)
  #     ## would be good to use this step to recode not as NA but as "skipped", so that then the overall prevalence is calculated over all respondents - data set to NA during cleaning?
  #     df <- expand_bin(
  #       df,
  #       vars = paste0(sv_el$hesper_vars, "_subset")
  #     )
  #     ## Again here, it would expand the binary for all hesper items, for all choices, for the subset only (after ensuring skip logic consistency)
  #   }
  # }

  ## Proposed change: use recode_subset_to_missing() using additional feature => recode to "NA_skip" instead of NA to distinguish from cleaning?
  ## Then use the cleaned data to calculate all prevalences using add_val_set_in_binaries()

  # 1. save original columns before skip logic cleaning with name suffix "_raw"
  hesper_vars_subset <- unlist(map(sv_l, ~ .x$hesper_vars))
  df[,
    paste0(hesper_vars_subset, "_raw") := .SD,
    .SDcols = c(hesper_vars_subset)
  ]

  # 2. clean skip logic
  if (sv) {
    for (sv_el in sv_l) {
      df <- recode_subset_to_missing(
        df,
        vars = sv_el$hesper_vars,
        subset_var = sv_el$subset_var,
        subset_vals = sv_el$subset_vals,
        suffix = NULL,
        missing_code = "NA_skip"
      )
    }
  }

  ## Based on data cleaned for subset with NA_skip when skip logic, compute binaries for the three metrics
  ## Could be done maybe through other function => basically a fcase mapping choices to 1, 0 and NA.

  ## Add HESPER binaries - global => prevalence of serious problem on all sample (not considering NA apart from NA_skip corresponding to skip logic from the tool)
  df <- df |>
    add_val_in_set_binaries(
      cols_character = hesper_vars,
      value_1 = c(hesper_serious_problem),
      value_0 = c(
        hesper_no_serious_problem,
        hesper_dnk,
        hesper_pnta,
        hesper_na,
        "NA_skip"
      ),
      replace = F,
      warn_if_no_match = !stop_if_subset_no_match,
      name_suffix = "binary",
      sep = "."
    )

  # Add HESPER binaries - subset => prevalence of serious problem on subset only, excluding skipped respondents and NA
  df <- df |>
    add_val_in_set_binaries(
      cols_character = hesper_vars,
      value_1 = c(hesper_serious_problem),
      value_0 = c(
        hesper_no_serious_problem,
        hesper_dnk,
        hesper_pnta,
        hesper_na
      ),
      replace = F,
      warn_if_no_match = !stop_if_subset_no_match,
      name_suffix = "binary_subset",
      sep = "."
    )

  # Add HESPER binaries - undefined => prevalence of undefined responses on subset only, excluding skipped respondents and NA
  df <- df |>
    add_val_in_set_binaries(
      cols_character = hesper_vars,
      value_1 = c(hesper_dnk, hesper_pnta, hesper_na),
      value_0 = c(hesper_serious_problem, hesper_no_serious_problem),
      replace = F,
      warn_if_no_match = !stop_if_subset_no_match,
      name_suffix = "binary_undefined",
      sep = "."
    )

  return(df)
}
