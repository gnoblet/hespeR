#' @title Align column names and choices in dataset and kobo tool to match standard HESPER names compatible with hespeR package
#'
#' @description Align column names and choices in dataset and kobo tool to match standard HESPER names compatible with hespeR package
#'
#' @param data A data frame with the HESPER data
#' @param hesper_serious_problem A character corresponding to the choice for serious problem
#' @param hesper_no_serious_problem A character corresponding to the choice for no serious problem
#' @param hesper_dnk A character corresponding to the choice for do not know
#' @param hesper_pnta A character corresponding to the choice for prefer not to answer
#' @param hesper_na A character corresponding to the choice for not applicable
#' @param hesper_drinking_water A character corresponding to the hesper item column name in dataset for drinking water
#' @param hesper_food A character corresponding to the hesper item column name in dataset for food
#' @param hesper_shelter A character corresponding to the hesper item column name in dataset for shelter
#' @param hesper_toilet A character corresponding to the hesper item column name in dataset for toilet
#' @param hesper_clean A character corresponding to the hesper item column name in dataset for clean (hygiene for whole household,  only for household level hesper)
#' @param hesper_clean_female A character corresponding to the hesper item column name in dataset for clean_female (menstrual hygiene for women specifically if household level, otherwise hygiene for women respondent if individual level hesper)
#' @param hesper_clothes_etc A character corresponding to the hesper item column name in dataset for clothes_etc (clothes, shoes, etc.)
#' @param hesper_income_livelihood A character corresponding to the hesper item column name in dataset for income_livelihood (income and livelihood)
#' @param hesper_health A character corresponding to the hesper item column name in dataset for physical health
#' @param hesper_health_care_male A character corresponding to the hesper item column name in dataset for health care for men
#' @param hesper_health_care_female A character corresponding to the hesper item column name in dataset for health care for women
#' @param hesper_distress A character corresponding to the hesper item column name in dataset for distress
#' @param hesper_safety A character corresponding to the hesper item column name in dataset for safety
#' @param hesper_education A character corresponding to the hesper item column name in dataset for education
#' @param hesper_care A character corresponding to the hesper item column name in dataset for care for other members of household
#' @param hesper_support A character corresponding to the hesper item column name in dataset for support to community members
#' @param hesper_separation A character corresponding to the hesper item column name in dataset for separation
#' @param hesper_displaced A character corresponding to the hesper item column name in dataset for displacement
#' @param hesper_information A character corresponding to the hesper item column name in dataset for information
#' @param hesper_aid A character corresponding to the hesper item column name in dataset for the way aid is provided
#' @param hesper_respect A character corresponding to the hesper item column name in dataset for respect
#' @param hesper_movement A character corresponding to the hesper item column name in dataset for lack of movement
#' @param hesper_time A character corresponding to the hesper item column name in dataset for too much free time
#' @param hesper_law A character corresponding to the hesper item column name in dataset for law and order in community
#' @param hesper_gbv A character corresponding to the hesper item column name in dataset for lack of safety for women in public spaces in community
#' @param hesper_drug A character corresponding to the hesper item column name in dataset for problem related to drug and alcohol use in community
#' @param hesper_mental_health A character corresponding to the hesper item column name in dataset for mental health in community
#' @param hesper_care_community A character corresponding to the hesper item column name in dataset for lack of care for others in community
#' @param hesper_other A character corresponding to the hesper item column name in dataset for other problem in community
#' @param hesper_clean_male A character corresponding to the hesper item column name in dataset for clean_male (hygiene for male respondent parameter, only for individual level hesper)
#' @param hesper_priority_first A character corresponding to the hesper item column name in dataset for priority_first (first priority among reported serious problems)
#' @param hesper_priority_second A character corresponding to the hesper item column name in dataset for priority_second (second priority among reported serious problems)
#' @param hesper_priority_third A character corresponding to the hesper item column name in dataset for priority_third (third priority among reported serious problems)
#' @param hesper_drinking_water_choice A character corresponding to the choice value for hesper priority corresponding to drinking water
#' @param hesper_food_choice A character corresponding to the choice value for hesper priority corresponding to food
#' @param hesper_shelter_choice A character corresponding to the choice value for hesper priority corresponding to shelter
#' @param hesper_toilet_choice A character corresponding to the choice value for hesper priority corresponding to toilet
#' @param hesper_clean_choice A character corresponding to the choice value for hesper priority corresponding to clean (hygiene for whole household, only for household level hesper)
#' @param hesper_clean_female_choice A character corresponding to the choice value for hesper priority corresponding to clean_female (menstrual hygiene for women only if household level hesper, otherwise hygiene for women respondent if individual level hesper)
#' @param hesper_clothes_etc_choice A character corresponding to the choice value for hesper priority corresponding to clothes_etc (clothes, shoes, etc.)
#' @param hesper_income_livelihood_choice A character corresponding to the choice value for hesper priority corresponding to income_livelihood (income and livelihood)
#' @param hesper_health_choice A character corresponding to the choice value for hesper priority corresponding to physical health
#' @param hesper_health_male_choice A character corresponding to the choice value for hesper priority corresponding to health care male
#' @param hesper_health_female_choice A character corresponding to the choice value for hesper priority corresponding to health care female
#' @param hesper_distress_choice A character corresponding to the choice value for hesper priority corresponding to distress
#' @param hesper_safety_choice A character corresponding to the choice value for hesper priority corresponding to safety
#' @param hesper_education_choice A character corresponding to the choice value for hesper priority corresponding to education
#' @param hesper_care_choice A character corresponding to the choice value for hesper priority corresponding to care for other members of household
#' @param hesper_support_choice A character corresponding to the choice value for hesper priority corresponding to support to community members
#' @param hesper_separation_choice A character corresponding to the choice value for hesper priority corresponding to separation
#' @param hesper_displaced_choice A character corresponding to the choice value for hesper priority corresponding to displacement
#' @param hesper_information_choice A character corresponding to the choice value for hesper priority corresponding to information
#' @param hesper_aid_choice A character corresponding to the choice value for hesper priority corresponding to the way aid is provided
#' @param hesper_respect_choice A character corresponding to the choice value for hesper priority corresponding to respect
#' @param hesper_movement_choice A character corresponding to the choice value for hesper priority corresponding to lack of movement
#' @param hesper_time_choice A character corresponding to the choice value for hesper priority corresponding to too much free time
#' @param hesper_law_choice A character corresponding to the choice value for hesper priority corresponding to law and order in community
#' @param hesper_gbv_choice A character corresponding to the choice value for hesper priority corresponding to lack of safety for women in public spaces
#' @param hesper_drug_choice A character corresponding to the choice value for hesper priority corresponding to problem related to drug and alcohol use
#' @param hesper_mental_health_choice A character corresponding to the choice value for hesper priority corresponding to mental health
#' @param hesper_care_community_choice A character corresponding to the choice value for hesper priority corresponding to lack of care for others in community
#' @param hesper_other_choice A character corresponding to the choice value for hesper priority corresponding to other problem
#' @param hesper_clean_male_choice A character corresponding to the choice value for hesper priority corresponding to clean_male (hygiene for male respondent parameter, only for individual level hesper)
#' @param sep A character corresponding to the separator used to separate parent from child choice binary columns in the colum names of dataset
#' @param kobo_survey A dataframe containing hesper kobo survey sheet as data.frame, set to NULL as default value
#' @param hesper_survey A dataframe containing hesper survey sheet as data.frame, set to NULL as default value
#'
#'
align_hesper_data <- function(
    data,

    hesper_serious_problem = "serious_problem",
    hesper_no_serious_problem = "no_serious_problem",
    hesper_dnk = "dnk",
    hesper_pnta = "pnta",
    hesper_na = "not_applicable",

    hesper_drinking_water = "hesper_drinking_water",
    hesper_food = "hesper_food",
    hesper_shelter = "hesper_shelter",
    hesper_toilet = "hesper_toilet",
    hesper_clean = "hesper_clean",
    hesper_clean_female = "hesper_clean_female",
    hesper_clothes_etc = "hesper_clothes_etc",
    hesper_income_livelihood = "hesper_income_livelihood",
    hesper_health = "hesper_health",
    hesper_health_care_male = "hesper_health_care_male",
    hesper_health_care_female = "hesper_health_care_female",
    hesper_distress = "hesper_distress",
    hesper_safety = "hesper_safety",
    hesper_education = "hesper_education",
    hesper_care = "hesper_care",
    hesper_support = "hesper_support",
    hesper_separation = "hesper_separation",
    hesper_displaced = "hesper_displaced",
    hesper_information = "hesper_information",
    hesper_aid = "hesper_aid",
    hesper_respect = "hesper_respect",
    hesper_movement = "hesper_movement",
    hesper_time = "hesper_time",
    hesper_law = "hesper_law",
    hesper_gbv = "hesper_gbv",
    hesper_drug = "hesper_drug",
    hesper_mental_health = "hesper_mental_health",
    hesper_care_community = "hesper_care_community",
    hesper_other = "hesper_other",
    hesper_clean_male = NULL, ## if individual level HESPER, standard name and item is hesper_clean_male, and replaces hesper_clean

    hesper_priority_first="hesper_priority_first",
    hesper_priority_second="hesper_priority_second",
    hesper_priority_third="hesper_priority_third",

    hesper_drinking_water_choice = "hesper_drinking_water",
    hesper_food_choice = "hesper_food",
    hesper_shelter_choice = "hesper_shelter",
    hesper_toilet_choice = "hesper_toilet",
    hesper_clean_choice = "hesper_clean",
    hesper_clean_female_choice = "hesper_clean_female",
    hesper_clothes_etc_choice = "hesper_clothes_etc",
    hesper_income_livelihood_choice = "hesper_income_livelihood",
    hesper_health_choice = "hesper_health",
    hesper_health_care_male_choice = "hesper_health_care_male",
    hesper_health_care_female_choice = "hesper_health_care_female",
    hesper_distress_choice = "hesper_distress",
    hesper_safety_choice = "hesper_safety",
    hesper_education_choice = "hesper_education",
    hesper_care_choice = "hesper_care",
    hesper_support_choice = "hesper_support",
    hesper_separation_choice = "hesper_separation",
    hesper_displaced_choice = "hesper_displaced",
    hesper_information_choice = "hesper_information",
    hesper_aid_choice = "hesper_aid",
    hesper_respect_choice = "hesper_respect",
    hesper_movement_choice = "hesper_movement",
    hesper_time_choice = "hesper_time",
    hesper_law_choice = "hesper_law",
    hesper_gbv_choice = "hesper_gbv",
    hesper_drug_choice = "hesper_drug",
    hesper_mental_health_choice = "hesper_mental_health",
    hesper_care_community_choice = "hesper_care_community",
    hesper_other_choice = "hesper_other",
    hesper_clean_male_choice = NULL,

    sep=".",

    kobo_survey=NULL,
    kobo_choices=NULL

){

  # data is a dataframe
  checkmate::assertDataFrame(data)

  # data is not data.table, convert it
  if (!checkmate::testDataTable(data)) {
    rlang::warn("Converting data to data.table.")
    data.table::setDT(data)
  }

  data <- copy(data) ## To enable running function multiple times

  ## get arguments names and organise
  hesper_arg_names = names(formals(sys.function()))[-1] ## excluding data
  hesper_arg_names_items_ch <- hesper_arg_names[grepl("(problem|dnk|pnta|na)$", hesper_arg_names)]
  hesper_arg_names_choices = hesper_arg_names[grepl("choice$", hesper_arg_names)]
  hesper_arg_names_prio = hesper_arg_names[grepl("priority_(first|second|third)$", hesper_arg_names)]
  hesper_arg_names_items = hesper_arg_names[grepl("^hesper_", hesper_arg_names, perl = TRUE) &
                                              !grepl("_(choice|priority_(first|second|third)|problem|dnk|pnta|na)$", hesper_arg_names, perl = TRUE)]

  ## get entered arguments
  call_args <- as.list(match.call())[-c(1,2)] # Exclude the function name and data
  all_args <- as.list(formals(sys.function())) # Retrieve all arguments and their default values
  combined_args <- modifyList(all_args, call_args) # Combine entered values and defaults (entered values override defaults)
  combined_args <- combined_args[!sapply(combined_args, is.null)]

  ## entered arguments items non null
  combined_args_items <- combined_args[c(hesper_arg_names_items)]
  combined_args_items <- combined_args_items[!sapply(combined_args_items, is.null)]

  ## entered arguments choices non null
  combined_args_choices <- combined_args[hesper_arg_names_choices]
  combined_args_choices <- combined_args_choices[!sapply(combined_args_choices, is.null)]

  ## entered arguments prio non null
  combined_args_prio <- combined_args[hesper_arg_names_prio]

  ## ensure that all combined_args corresponding to hesper_vars are in data
  check_vars_in_df(data, combined_args_items)

  ## ensure that all combined_args corresponding to hesper_choice are in data
  check_vars_in_set(data, unlist(unname(combined_args_prio)), combined_args_choices)

  # hesper_vars only contains values in hesper_serious_problem, hesper_no_serious_problem, hesper_dnk, hesper_pnta and hesper_na
  check_vars_in_set(data, unlist(unname(combined_args_items)), c(hesper_serious_problem, hesper_no_serious_problem, hesper_dnk, hesper_pnta, hesper_na))

  ## Rename column names of hesper items with standard values
  hesper_item_standard <- unlist(names(c(combined_args_items, combined_args_prio)))
  hesper_item_data <- unlist(unname(c(combined_args_items, combined_args_prio)))
  data <- data.table::setnames(data,
                               hesper_item_data,
                               hesper_item_standard)
  rlang::warn(glue::glue("The following columns have been renamed:\n\n",
                         paste0(hesper_item_data, " to ", hesper_item_standard, collapse="\n")))

  ## Replace old hesper item choices with standard "serious_problem" etc...
  pattern.replacement <- setNames(c("serious_problem", "no_serious_problem", "dnk", "pnta", "not_applicable"),
                                  c(hesper_serious_problem, hesper_no_serious_problem, hesper_dnk, hesper_pnta, hesper_na))
  ## subset hesper items in data only
  hesper_vars_in_data <- hesper_arg_names_items[hesper_arg_names_items %in% colnames(data)]
  data[, c(hesper_vars_in_data) := lapply(.SD,  \(x) str_replace_all(x, pattern.replacement)), .SDcols = hesper_vars_in_data]
  rlang::warn(glue::glue("The following columns have been aligned:\n",
                         paste0(hesper_vars_in_data, collapse="; "), "\n\nAccording to these choice values:\n\n",
                         paste0(names(pattern.replacement), " to ", unname(pattern.replacement), collapse="\n"),
                         "\n"))


  ## If any child column for top three starting with combined_args_prio and then ".", remove it
  if (any(grepl(paste0(paste0("^", unlist(names(combined_args_prio)), sep), collapse="|"), names(data)))){
    data <- data[, !grepl(paste0(paste0("^", unlist(names(combined_args_prio)), sep), collapse="|"), names(data)), with=F]
    rlang::warn(glue::glue("The following columns have been removed:\n\n",
                           paste0(names(data)[grepl(paste0(paste0("^", unlist(names(combined_args_prio)), sep), collapse="|"), names(data))], collapse="; "),
                           "\n"))
  }

  ## replace occurence in top 1 2 3 with standard names
  hesper_choice_standard <- gsub("_choice$", "", unlist(names(combined_args_choices)))
  hesper_choice_data <- unlist(unname(combined_args_choices))
  pattern.replacement <- setNames(hesper_choice_standard, paste0("^", hesper_choice_data, "$"))
  data[, c(hesper_arg_names_prio) := lapply(.SD,  \(x) str_replace_all(x, pattern.replacement)), .SDcols = hesper_arg_names_prio]
  rlang::warn(glue::glue("The following columns have been aligned:\n",
                         paste0(hesper_arg_names_prio, collapse="; "), "\n\nAccording to these new choice values:\n\n",
                         paste0(hesper_choice_data, " to ", hesper_choice_standard, collapse="\n"),
                         "\n"))


  ## if kobo survey and choices supplied, replace all old names with new names + old choices with new choices similarly in
  if(!is.null(kobo_survey) & !is.null(kobo_choices)){

    # kobo_survey & kobo_choices are dataframes
    checkmate::assertDataFrame(kobo_survey)
    checkmate::assertDataFrame(kobo_choices)

    # kobo_survey and kobo_choices is not data.table, convert it
    if (!checkmate::testDataTable(kobo_survey)) {
      rlang::warn("Converting kobo_survey to data.table.")
      data.table::setDT(kobo_survey)
    }
    if (!checkmate::testDataTable(kobo_choices)) {
      rlang::warn("Converting kobo_choices to data.table.")
      data.table::setDT(kobo_choices)
    }

    kobo_survey <- copy(kobo_survey)
    kobo_choices <- copy(kobo_choices)

    ## 1. clean tool names
    pattern.replacement <- setNames(hesper_item_standard, paste0("^", hesper_item_data, "$"))
    kobo_survey[, name:=str_replace_all(name, pattern.replacement)]
    rlang::warn(
      glue::glue("Cleaning names for hesper items and top three priorities in kobo tool choices\n",
                 "The name column has been aligned in kobo_survey according to these new values:\n\n",
                 paste0(hesper_item_data, " to ", hesper_item_data, collapse="\n"),
                 "\n")
    )

    ## 2. clean tool choices

    ## clean hesper top three priorities
    pattern.replacement <- setNames(hesper_choice_standard, paste0("^", hesper_choice_data, "$"))
    kobo_choices[, name:=str_replace_all(name, pattern.replacement)]
    rlang::warn(
      glue::glue("Cleaning choices for top three priorities in kobo tool choices\n",
                 "The name column has been aligned in kobo_choices according to these new values:\n\n",
                 paste0(hesper_choice_data, " to ", hesper_choice_standard, collapse="\n"),
                 "\n")
    )

    ## clean hesper serious problem / no serious problem etc...
    pattern.replacement <- setNames(c("serious_problem", "no_serious_problem", "dnk", "pnta", "not_applicable"),
                                    c(hesper_serious_problem, hesper_no_serious_problem, hesper_dnk, hesper_pnta, hesper_na))
    kobo_choices[, name:=str_replace_all(name, pattern.replacement)]
    rlang::warn(
      glue::glue("Cleaning choices for hesper items in kobo tool choices\n",
                 "The name column has been aligned in kobo_choices according to these new values:\n\n",
                 paste0(names(pattern.replacement), " to ", unname(pattern.replacement), collapse="\n"),
                 "\n")
    )

    return(list(data=data, kobo_survey=kobo_survey, kobo_choices=kobo_choices))} else {
      return(data)
  }


}
#
#
#
#
# Testing all of this
#
# kobo_survey=read_excel("../resources/REACH_MLI2402__tool.xlsx", "survey")
# kobo_choices=read_excel("../resources/REACH_MLI2402__tool.xlsx", "choices")
# ex_data=fread("../data/mli_msna_24_hh.csv")
#
# ## clean top three with NA if equal to "" with data.table syntax .SD
# col_prio <- c("hesper_most_serious_1", "hesper_most_serious_2", "hesper_most_serious_3")
# ex_data[, c(col_prio, "hesper_5_1_keep_clean") := lapply(.SD, function(x) ifelse(x=="", NA, x)), .SDcols = c(col_prio, "hesper_5_1_keep_clean")]
#
# test <- align_hesper_data(
#   ex_data,
#
#   hesper_serious_problem = "probleme_grave",
#   hesper_no_serious_problem = "pas_probleme",
#   hesper_dnk = "nsp",
#   hesper_pnta = "pnpr",
#   hesper_na = "pas_concerne",
#
#   hesper_drinking_water = "hesper_1_water",
#   hesper_food = "hesper_2_food",
#   hesper_shelter = "hesper_3_live_place",
#   hesper_toilet = "hesper_4_toilet",
#   hesper_clean = "hesper_5_keep_clean",
#   hesper_clean_female = "hesper_5_1_keep_clean",
#   hesper_clothes_etc = "hesper_6_dress",
#   hesper_income_livelihood = "hesper_7_revenu",
#   hesper_health = "hesper_8_phisical_health",
#   hesper_health_care_male = "hesper_9_health_care_men",
#   hesper_health_care_female = "hesper_9_health_care_women",
#   hesper_distress = "hesper_10_distress",
#   hesper_safety = "hesper_11_safety",
#   hesper_education = "hesper_12_education_for_child",
#   hesper_care = "hesper_13_family_care",
#   hesper_support = "hesper_14_support_from_other",
#   hesper_separation = "hesper_15_family_separation",
#   hesper_displaced = "hesper_16_being_displaced",
#   hesper_information = "hesper_17_information",
#   hesper_aid = "hesper_18_way_aid_provided",
#   hesper_respect = "hesper_19_respect",
#   hesper_movement = "hesper_20_moving_place",
#   hesper_time = "hesper_21_too_much_time",
#   hesper_law = NULL,
#   hesper_gbv = NULL,
#   hesper_drug = NULL,
#   hesper_mental_health = NULL,
#   hesper_care_community = NULL,
#   hesper_other = NULL,
#   hesper_clean_male=NULL,
#
#   hesper_priority_first="hesper_most_serious_1",
#   hesper_priority_second="hesper_most_serious_2",
#   hesper_priority_third="hesper_most_serious_3",
#
#   hesper_drinking_water_choice = "water",
#   hesper_food_choice = "food",
#   hesper_shelter_choice = "live_place",
#   hesper_toilet_choice = "toilet",
#   hesper_clean_choice = "clean",
#   hesper_clean_female_choice = "clean",
#   hesper_clothes_etc_choice = "dress",
#   hesper_income_livelihood_choice = "revenu",
#   hesper_health_choice = "phisical_health",
#   hesper_health_care_male_choice = "health_care_men",
#   hesper_health_care_female_choice = "health_care_women",
#   hesper_distress_choice = "distress",
#   hesper_safety_choice = "safety",
#   hesper_education_choice = "education_for_child",
#   hesper_care_choice = "family_care",
#   hesper_support_choice = "support_from_other",
#   hesper_separation_choice = "family_separation",
#   hesper_displaced_choice = "being_displaced",
#   hesper_information_choice = "information",
#   hesper_aid_choice = "way_aid_provided",
#   hesper_respect_choice = "respect",
#   hesper_movement_choice = "moving_place",
#   hesper_time_choice = "too_much_time",
#   hesper_law_choice = NULL,
#   hesper_gbv_choice = NULL,
#   hesper_drug_choice = NULL,
#   hesper_mental_health_choice = NULL,
#   hesper_care_community_choice = NULL,
#   hesper_other_choice = NULL,
#   hesper_clean_male_choice = NULL,
#
#   sep=".",
#
#   kobo_survey=kobo_survey,
#   kobo_choices=kobo_choices
# )
#
# hesper_drinking_water = "hesper_1_water"
# hesper_food = "hesper_2_food"
# hesper_shelter = "hesper_3_live_place"
# hesper_toilet = "hesper_4_toilet"
# hesper_clean = "hesper_5_keep_clean"
# hesper_clean_female = "hesper_5_1_keep_clean"
# hesper_clothes_etc = "hesper_6_dress"
# hesper_income_livelihood = "hesper_7_revenu"
# hesper_health = "hesper_8_phisical_health"
# hesper_health_care_male = "hesper_9_health_care_men"
# hesper_health_care_female = "hesper_9_health_care_women"
# hesper_distress = "hesper_10_distress"
# hesper_safety = "hesper_11_safety"
# hesper_education = "hesper_12_education_for_child"
# hesper_care = "hesper_13_family_care"
# hesper_support = "hesper_14_support_from_other"
# hesper_separation = "hesper_15_family_separation"
# hesper_displaced = "hesper_16_being_displaced"
# hesper_information = "hesper_17_information"
# hesper_aid = "hesper_18_way_aid_provided"
# hesper_respect = "hesper_19_respect"
# hesper_movement = "hesper_20_moving_place"
# hesper_time = "hesper_21_too_much_time"
# hesper_law = NULL
# hesper_gbv = NULL
# hesper_drug = NULL
# hesper_mental_health = NULL
# hesper_care_community = NULL
# hesper_other = NULL
# hesper_clean_male=NULL
#
# hesper_priority_first="hesper_most_serious_1"
# hesper_priority_second="hesper_most_serious_2"
# hesper_priority_third="hesper_most_serious_3"
#
# hesper_drinking_water_choice = "water"
# hesper_food_choice = "food"
# hesper_shelter_choice = "live_place"
# hesper_toilet_choice = "toilet"
# hesper_clean_choice = "clean"
# hesper_clean_female_choice = "clean"
# hesper_clothes_etc_choice = "dress"
# hesper_income_livelihood_choice = "revenu"
# hesper_health_choice = "phisical_health"
# hesper_health_care_male_choice = "health_care_men"
# hesper_health_care_female_choice = "health_care_women"
# hesper_distress_choice = "distress"
# hesper_safety_choice = "safety"
# hesper_education_choice = "education_for_child"
# hesper_care_choice = "family_care"
# hesper_support_choice = "support_from_other"
# hesper_separation_choice = "family_separation"
# hesper_displaced_choice = "being_displaced"
# hesper_information_choice = "information"
# hesper_aid_choice = "way_aid_provided"
# hesper_respect_choice = "respect"
# hesper_movement_choice = "moving_place"
# hesper_time_choice = "too_much_time"
# hesper_law_choice = NULL
# hesper_gbv_choice = NULL
# hesper_drug_choice = NULL
# hesper_mental_health_choice = NULL
# hesper_care_community_choice = NULL
# hesper_other_choice = NULL
# hesper_clean_male_choice = NULL
#
# hesper_serious_problem = "probleme_grave"
# hesper_no_serious_problem = "pas_probleme"
# hesper_dnk = "nsp"
# hesper_pnta = "pnpr"
# hesper_na = "pas_concerne"

## for testing only
# hesper_arg_names = names(formals(align_hesper_data))[-1] ## excluding data
# all_args <- as.list(formals(align_hesper_data)) # Retrieve all arguments and their default values
# args <- ls(pattern = "^hesper") %>% str_subset("df|hesper_dat|hesper_vars|_arg_|hesper_opts|problem|dnk|pnta|_na$", negate=T)
# args_val <- lapply(args, get)
# call_args <- setNames(args_val, args)

