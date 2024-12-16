#' @title function that creates different composite indicators from hesper items for each type of hesper items (at least one item as serious problem from the category, number of items as serious problem from the category, proportion of items as serious problem from the category, overall proportion of items as serious problem from the category compared to all other items)
#'
#' @description naming done using the following syntax: nb_hesper_items_{section_name}.{section_category}
#' @param df dataframe containing the data
#' @param list_group named list with all HESPER items column names organized by category
#' The argument must be a named list containing for each list element a vector with all columns names corresponding to the group category. 
#' The names of the list elements must be the name of the group category.  
#' @param choice_serious value that indicates a serious problem
#' @param choice_no_serious value that indicates no serious problem
#' @param choice_dnk value that indicates do not know
#' @param choice_pnta value that indicates decline to answer
#' @param choice_na value that indicates not applicable to household
#' 
#' @return dataframe with following new columns added: nb_hesper_items_{section_name}.{section_category} // prop_hesper_items_{section_name}.{section_category} // hesper_items_{section_name}.{section_category}
#' @details The function creates the following indicators:
#' nb_hesper_items_{section_name}.{section_category} : number of items with serious problem reported in the corresponding section category
#' prop_hesper_items_{section_name}.{section_category} : proportion of items with serious problem reported in the corresponding section category relative to all items of this section category
#' overall_prop_hesper_items_{section_name}.{section_category} : overall proportion of items with serious problem in the corresponding section category compared to all items reported as serious problem
#' at_least_one_hesper_item_{section_name}.{section_category} : binary column indicating if at least one item with serious problem is reported in the corresponding section category
#' @export
#' 
#' @examples
#' # 1. how to categorise hesper items according to sector
#' hesper_dat_out <- hesper_dat %>%
#'   add_hesper_cat(
#'     list_group = list(
#'       aap = c("hesper_information", 
#'               "hesper_aid"),
#'       education = c("hesper_education"),
#'       fsl = c("hesper_food"),
#'       health = c("hesper_health", 
#'                  "hesper_health_care_men", 
#'                   "hesper_health_care_women"),
#'       livelihood = c("hesper_income_livelihood"),
#'       protection = c("hesper_distress", 
#'                      "hesper_safety", 
#'                      "hesper_separation", 
#'                      "hesper_displaced", 
#'                      "hesper_respect",
#'                      "hesper_movement",
#'                      "hesper_law", 
#'                      "hesper_gbv",
#'                      "hesper_drug"),
#'       snfi = c("hesper_shelter", 
#'                "hesper_clothes_etc"),
#'       wash = c("hesper_drinking_water",
#'                "hesper_toilet", 
#'                "hesper_clean",
#'                "hesper_clean_women"),
#'       wellbeing = c("hesper_care",
#'                     "hesper_support", 
#'                     "hesper_time", 
#'                     "hesper_mental_health",
#'                     "hesper_care_community")
#'                     ),
#'    choice_serious = "serious_problem",
#'    choice_no_serious = "no_serious_problem",
#'    choice_dnk = "dnk",
#'    choice_pnta = "pnta",
#'    choice_na = "not_applicable"
#'  ) 
#' 
#' 
#' # 2. how to categorise hesper top three priority items according to sector
#'
#' hesper_dat_out <- hesper_dat %>%
#'   add_hesper_cat(
#'      list_group = list(
#'        aap = c("hesper_top_three_priorities.hesper_information", "hesper_top_three_priorities.hesper_aid"),
#'        displacement = c("hesper_top_three_priorities.hesper_displaced"),
#'        education = c("hesper_top_three_priorities.hesper_education"),
#'        fsl = c("hesper_top_three_priorities.hesper_food"),
#'        health = c("hesper_top_three_priorities.hesper_health", "hesper_top_three_priorities.hesper_health_care_men", "hesper_top_three_priorities.hesper_health_care_women"),
#'        livelihood = c("hesper_top_three_priorities.hesper_income_livelihood"),
#'        protection = c("hesper_top_three_priorities.hesper_distress", "hesper_top_three_priorities.hesper_safety", 
#'                       "hesper_top_three_priorities.hesper_separation", "hesper_top_three_priorities.hesper_respect", 
#'                       "hesper_top_three_priorities.hesper_movement", "hesper_top_three_priorities.hesper_law", 
#'                       "hesper_top_three_priorities.hesper_gbv", "hesper_top_three_priorities.hesper_drug"),
#'        snfi = c("hesper_top_three_priorities.hesper_shelter", "hesper_top_three_priorities.hesper_clothes_etc"),
#'        wash = c("hesper_top_three_priorities.hesper_drinking_water", "hesper_top_three_priorities.hesper_toilet",
#'                 "hesper_top_three_priorities.hesper_clean", "hesper_top_three_priorities.hesper_clean_women"),
#'        wellbeing = c("hesper_top_three_priorities.hesper_care", "hesper_top_three_priorities.hesper_support",
#'                      "hesper_top_three_priorities.hesper_time", "hesper_top_three_priorities.hesper_mental_health_care_community")
#'        other = c("hesper_top_three_priorities.hesper_other")
#'        ),
#'        choice_serious = 1,
#'        choice_no_serious = 0,
#'        choice_na = NA
#'      )
#'        

add_hesper_cat <- function(
    df,
    list_group = list(
      aap = c("hesper_information", 
              "hesper_aid"),
      education = c("hesper_education"),
      fsl = c("hesper_food"),
      health = c("hesper_health", 
                 "hesper_health_care_men", 
                 "hesper_health_care_women"),
      livelihood = c("hesper_income_livelihood"),
      protection = c("hesper_distress", 
                     "hesper_safety", 
                     "hesper_separation", 
                     "hesper_displaced", 
                     "hesper_respect",
                     "hesper_movement",
                     "hesper_law", 
                     "hesper_gbv",
                     "hesper_drug"),
      snfi = c("hesper_shelter", 
               "hesper_clothes_etc"),
      wash = c("hesper_drinking_water",
               "hesper_toilet", 
               "hesper_clean",
               "hesper_clean_women"),
      wellbeing = c("hesper_care",
                    "hesper_support", 
                    "hesper_time", 
                    "hesper_mental_health",
                    "hesper_care_community")
    ),
    choice_serious = "serious_problem",
    choice_no_serious = "no_serious_problem",
    choice_dnk = "dnk",
    choice_pnta = "pnta",
    choice_na = "not_applicable"
  ){
  
  ## access the name of the object passed as argument to "list_group"
  list_group_name <- deparse(substitute(list_group))
  list_group_cat <- names(list_group)
  
  ## check that all columns are present in dataframe and print the non matching columns
  col_hesper <- list_group %>% unname %>% unlist
  col_not_matching <- col_hesper %>% purrr::keep(!. %in% colnames(df))
  if(length(col_not_matching) > 0){warning(paste("The following columns are not present in the dataframe: ", col_not_matching, "\n"))}
  
  ## all response choices that should not be counted in the total nb of items
  choice_applicable <- c(choice_serious, choice_no_serious, choice_dnk, choice_pnta)
  choice_exclude <- c(NA, choice_na, choice_dnk, choice_pnta)
  
  ## Add a check if choice_serious and choice_no_serious never appears in any of df[, col_hesper]
  
  if(df |> dplyr::select(dplyr::any_of(col_hesper)) |> purrr::map(unique) |> purrr::map(~all(!. %in% c(choice_serious, choice_no_serious))) |> unlist %>% any){
    stop("The specified values for arguments choice_serious and choice_no_serious do not appear in the dataframe. Please check the values.")
  }
  
  ## for each group category, create the nb_hesper_items_{section_name}.{section_category} to capture number of serious problem recorded
  ## if there is only NAs across list_group[[cat]], the value should be set to NA 
  
  for (cat in list_group_cat){
    df <- dplyr::mutate(
      df, 
      all_serious_items := rowSums(dplyr::across(dplyr::any_of(unlist(unname(list_group))), ~ . %in% choice_serious), na.rm=T),
       n_valid := rowSums(dplyr::across(dplyr::any_of(list_group[[cat]]), ~ !. %in% choice_exclude)), !!paste0("nb_hesper_items_", list_group_name, ".", cat) := dplyr::case_when(
               n_valid == 0 ~ NA_real_, TRUE ~ rowSums(dplyr::across(dplyr::any_of(list_group[[cat]]), ~. %in% c(choice_serious)), na.rm=T)
             ),
      !!paste0("prop_hesper_items_", list_group_name, ".", cat) := !!sym(paste0("nb_hesper_items_", list_group_name, ".", cat)) / n_valid,
      !!paste0("overall_prop_hesper_", list_group_name, ".", cat) := !!sym(paste0("nb_hesper_items_", list_group_name, ".", cat)) / all_serious_items,
      !!paste0("at_least_one_hesper_item_", list_group_name, ".", cat) := dplyr:: case_when(
        n_valid == 0 ~ NA_real_,
        .default = rowSums(dplyr::across(dplyr::any_of(list_group[[cat]]), ~. %in% c(choice_serious)), na.rm=T) > 0
      )
    ) 
  }
  
  df <- dplyr::select(df, -n_valid)
  
  return(df)
}
