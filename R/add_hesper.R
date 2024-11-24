#' @title function that compute for each respondent the number of selected items, number of applicable items and collapse the three ordered priorities into one select multiple column
#'
#' @description The function computes the number of selected items for each respondent, the number of applicable items and collapse the three ordered priorities into one select multiple column
#' @param df dataframe containing the cleaned data
#' @param col_items vector of column names containing the hesper items. 
#' @param choice_serious character string representing the choice for serious problem
#' @param choice_no_serious character string representing the choice for no serious problem
#' @param choice_dnk character string representing the choice for do not know
#' @param choice_pnta character string representing the choice for decline to answer
#' @param choice_na character string representing the choice for not applicable to household
#' @param cols_priority vector of three column names containing the first, second and third priority hesper items  
#' @param col_gender character string representing the column name containing the respondent's gender
#' @param choice_male character string representing the choice corresponding to male respondent gender
#' @param choice_female character string representing the choice corresponding to female respondent gender
#' @param hesper_item_male vector of two choice names corresponding to the hesper items specific for male respondent 
#' @param hesper_item_femal vector of two choice names corresponding to the hesper items specific for female respodent
#' @param col_displacement character string representing the column name containing the respondent's displacement status
#' @param choice_displaced character string representing the choice corresponding to displaced respondent
#' @param choice_not_displaced character string representing the choice corresponding to not displaced respondent
#' @param col_hesper_subset character string representing the column name containing the respondent's hesper items that are applicable only to a gender/displacement status subset. should match column names in the dataset
#' @param hesper_item_displaced vector of two choices name coresponding to the hesper items specific for displaced respondent
#' @param hesper_item_not_displaced vector of two choices name coresponding to the hesper items specific for not displaced respondent
#' @param add_binaries logical value indicating whether to add binary variables for each hesper item
#' @param add_binaries_subset logical value indicating whether to add binary variables for each hesper item that are applicable to only a subset of the population
#' @param subset logical value indicating whether to add binary subset variables for all priority child columns that are applicable to only a subset of the population
#'
#' @return dataframe with the number of selected items, the number of applicable items and the three ordered priorities collapsed into one select multiple column

add_hesper_main <- function(df,
                            col_items = c("hesper_drinking_water", "hesper_food", "hesper_shelter", "hesper_toilet", "hesper_clean", "hesper_clean", "hesper_clothes_etc", "hesper_income_livelihood", "hesper_health", "hesper_health",
                                          "hesper_health", "hesper_distress", "hesper_safety", "hesper_education", "hesper_care", "hesper_support", "hesper_separation", "hesper_displaced", "hesper_information", "hesper_aid",
                                          "hesper_respect", "hesper_movement", "hesper_time", "hesper_law", "hesper_gbv", "hesper_drug", "hesper_mental_health", "hesper_care_community"),
                            choice_serious = "Serious_problem",
                            choice_no_serious = "No_serious_problem",
                            choice_dnk = "Do_not_know",
                            choice_pnta = "Decline_to_answer",
                            choice_na = "Not_applicable_to_household",
                            cols_priority = c("hesper_priority_first", "hesper_priority_second", "hesper_priority_third"),
                            col_name_hesper_top_three="hesper_top_three_priorities",
                            col_gender = "gender",
                            choice_male = "men",
                            choice_female = "women",
                            hesper_item_male = c("hesper_health_care_men"),
                            hesper_item_female = c("hesper_clean_women", "hesper_health_care_women"),
                            col_displacement = "pop_group",
                            choices_displaced = c("displaced"),
                            choices_non_displaced = c("host"), 
                            col_hesper_subset = c("hesper_health_care_men", "hesper_clean_women", "hesper_health_care_women", "hesper_displaced"),
                            hesper_item_displaced = c("hesper_displaced"),
                            hesper_item_non_displaced = NULL,
                            add_binaries = T,
                            add_binaries_subset = T,
                            subset = T
                            ){
  
  ## check that all columns are present in dataframe and print the non matching columns
  col_not_matching <- col_items %>% unname %>% unlist %>% purrr::keep(!. %in% colnames(df))
  if(length(col_not_matching) > 0){warning(paste("The following columns are not present in the dataframe: ", col_not_matching))}
  
  ## all response choices that should not be counted in the total nb of items
  choice_applicable <- c(choice_serious, choice_no_serious, choice_dnk, choice_pnta)
  choice_exclude <- c(NA, choice_na, choice_dnk, choice_pnta)
  
  ## compute total items selected
  df <- df %>%
    dplyr::mutate(nb_hesper.all = rowSums(dplyr::across(dplyr::all_of(col_items), ~. %in% choice_serious), na.rm=T),
           nb_hesper.applicable = rowSums(dplyr::across(dplyr::all_of(col_items), ~ . %in% choice_applicable), na.rm=T),
           nb_hesper.pnta = rowSums(dplyr::across(dplyr::all_of(col_items), ~ . %in% choice_pnta), na.rm=T),
           nb_hepser.dnk = rowSums(dplyr::across(dplyr::all_of(col_items), ~ . %in% choice_dnk), na.rm=T),
           prop_hesper.all = nb_hesper.all / nb_hesper.applicable)
  
  ## Add binary columns to record serious problem, 0 if no serious problem and NA otherwise for all items across col_items
  ## Column names is original names with _binary suffix appended
  
  if (add_binaries){
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(col_items), ~dplyr::case_when(. %in% choice_serious ~ 1, . %in% c(choice_dnk, choice_pnta) ~ NA_real_, TRUE ~ 0), .names = "{.col}.binary"))
  }
  
  if (add_binaries_subset){
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(col_hesper_subset), ~dplyr::case_when(. %in% choice_serious ~ 1, . %in% choice_no_serious ~ 0, TRUE ~ NA_real_), .names = "{.col}.binary_subset"))
  }
    
  ## collapse priority columns to have a select multiple column
  if (!is.null(cols_priority)){
    
    ## stop and display columns that are not in df
    if (sum(!cols_priority %in% colnames(df))>0) stop("The following columns are not present in the dataframe: ", cols_priority[!cols_priority %in% colnames(df)])
    
    ## unite the thre priority columns to have one select multiple hesper priorities
    df <- df %>%
      tidyr::unite(!!rlang::sym(col_name_hesper_top_three), dplyr::all_of(cols_priority), sep = " ", remove = F, na.rm = T) %>%
      dplyr::mutate(!!rlang::sym(col_name_hesper_top_three) := ifelse(!!rlang::sym(col_name_hesper_top_three)=="", NA, stringr::str_replace_all(!!rlang::sym(col_name_hesper_top_three), "^ | $", "")))
    
    ### expand parent column top three priorities and priority without subset
    df <- df %>% expand.select.multiple.vec(c(col_name_hesper_top_three, cols_priority))
    
    ### ensure that skip logic are respected
    if (subset){
      
      # mutate all subset binaries with _subset at the end before reworking them 
      col_hesper_male <- if (!any(is.null(hesper_item_male)|is.na(hesper_item_male))) paste0(col_name_hesper_top_three, ".", hesper_item_male, "_subset") else NULL
      col_hesper_female <- if (!any(is.null(hesper_item_female)|is.na(hesper_item_female))) paste0(col_name_hesper_top_three, ".", hesper_item_female, "_subset") else NULL
      col_hesper_displaced <- if (!any(is.null(hesper_item_displaced)|is.na(hesper_item_displaced))) paste0(col_name_hesper_top_three, ".", hesper_item_displaced, "_subset") else NULL 
      col_hesper_non_displaced <- if (!any(is.null(hesper_item_non_displaced)|is.na(hesper_item_non_displaced))) paste0(col_name_hesper_top_three, ".", hesper_item_non_displaced, "_subset") else NULL
      col_hesper_subset <- c(col_hesper_male, col_hesper_female, col_hesper_displaced, col_hesper_non_displaced)  
      col_hesper_item_subset <- col_hesper_subset %>% stringr::str_replace_all("_subset$", "")
      
      df <- df %>% dplyr::mutate(dplyr::across(any_of(col_hesper_item_subset), ~ . , .names = "{.col}_subset"))
      df <- df %>% dplyr::mutate(
        dplyr::across(any_of(col_hesper_male), ~ dplyr::case_when(!(!!rlang::sym(col_gender) == choice_male) ~ NA_real_, TRUE ~ .)),
        dplyr::across(any_of(col_hesper_female), ~ dplyr::case_when(!(!!rlang::sym(col_gender) == choice_female) ~ NA_real_, TRUE ~ .)),
        dplyr::across(any_of(col_hesper_displaced), ~ dplyr::case_when(!(!!rlang::sym(col_displacement) %in% choices_displaced) ~ NA_real_, TRUE ~ .)),
        dplyr::across(any_of(col_hesper_non_displaced), ~ dplyr::case_when(!(!!rlang::sym(col_displacement) %in% choices_non_displaced) ~ NA_real_, TRUE ~ .))
      )
      
      ## mutate all subset binaries for priority 1 / 2 / 3
      col_prio_male <- if (!any(is.null(hesper_item_male)|is.na(hesper_item_male))) purrr::map(cols_priority, ~ paste0(., ".", hesper_item_male, "_subset")) %>% unlist else NULL
      col_prio_female <- if (!any(is.null(hesper_item_female)|is.na(hesper_item_female))) purrr::map(cols_priority, ~ paste0(., ".", hesper_item_female, "_subset")) %>% unlist else NULL
      col_prio_displaced <- if (!any(is.null(hesper_item_displaced)|is.na(hesper_item_displaced))) purrr::map(cols_priority, ~ paste0(., ".", hesper_item_displaced, "_subset")) %>% unlist else NULL
      col_prio_non_displaced <- if (!any(is.null(hesper_item_non_displaced)|is.na(hesper_item_non_displaced))) purrr::map(cols_priority, ~ paste0(., ".", hesper_item_non_displaced, "_subset")) %>% unlist else NULL
      col_prio_subset <- c(col_prio_male, col_prio_female, col_prio_displaced)
      col_prio_item_subset <- col_prio_subset %>% stringr::str_replace_all("_subset$", "")
      
      df <- df %>% dplyr::mutate(dplyr::across(any_of(col_prio_item_subset), ~ . , .names = "{.col}_subset"))
      df <- df %>% dplyr::mutate(
        dplyr::across(any_of(col_prio_male), ~ dplyr::case_when(!(!!rlang::sym(col_gender) == choice_male) ~ NA_real_, TRUE ~ .)),
        dplyr::across(any_of(col_prio_female), ~ dplyr::case_when(!(!!rlang::sym(col_gender) == choice_female) ~ NA_real_, TRUE ~ .)),
        dplyr::across(any_of(col_prio_displaced), ~ dplyr::case_when(!(!!rlang::sym(col_displacement) %in% choices_displaced) ~ NA_real_, TRUE ~ .)),
        dplyr::across(any_of(col_prio_non_displaced), ~ dplyr::case_when(!(!!rlang::sym(col_displacement) %in% choices_non_displaced) ~ NA_real_, TRUE ~ .))
      )
      
    }
    
  }
  
  return(df)
  
}



#' @title function that creates the number of items with serious problem reported in a particular section and compute main other indicators needed
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

add_hesper_cat <- function(df,
                           list_group = section_wide,
                           choice_serious = "serious_problem",
                           choice_no_serious = "no_serious_problem",
                           choice_dnk = "dnk",
                           choice_pnta = "pnta",
                           choice_na = "not_applicable"
){
  
  ## access the name of the object passed as argument to "list_group"
  list_group_name <- deparse(substitute(list_group))
  list_group_cat <- names(list_group)
  
  # ## check that all columns are present in dataframe and print the non matching columns
  col_hesper <- list_group %>% unname %>% unlist
  col_not_matching <- col_hesper %>% purrr::keep(!. %in% colnames(df))
  if(length(col_not_matching) > 0){warning(paste("The following columns are not present in the dataframe: ", col_not_matching, "\n"))}
  
  ## all response choices that should not be counted in the total nb of items
  choice_applicable <- c(choice_serious, choice_no_serious, choice_dnk, choice_pnta)
  choice_exclude <- c(NA, choice_na, choice_dnk, choice_pnta)
  
  ## Add a check if choice_serious and choice_no_serious never appears in any of df[, col_hesper]
     
  if(df %>% dplyr::select(any_of(col_hesper)) %>% purrr::map(unique) %>% purrr::map(~all(!. %in% c(choice_serious, choice_no_serious))) %>% unlist %>% any){
    stop("The specified values for arguments choice_serious and choice_no_serious do not appear in the dataframe. Please check the values.")
  }
  
  ## for each group category, create the nb_hesper_items_{section_name}.{section_category} to capture number of serious problem recorded
  ## if there is only NAs across list_group[[cat]], the value should be set to NA 
  
  for (cat in list_group_cat){
    df <- df %>% 
      dplyr::mutate(all_serious_items := rowSums(dplyr::across(any_of(unlist(unname(list_group))), ~ . %in% choice_serious), na.rm=T),
             
             n_valid := rowSums(dplyr::across(any_of(list_group[[cat]]), ~ !. %in% choice_exclude)),
             
             !!paste0("nb_hesper_items_", list_group_name, ".", cat) := dplyr::case_when(
               n_valid == 0 ~ NA_real_, TRUE ~ rowSums(dplyr::across(any_of(list_group[[cat]]), ~. %in% c(choice_serious)), na.rm=T)
             ),
             
             !!paste0("prop_hesper_items_", list_group_name, ".", cat) := !!rlang::sym(paste0("nb_hesper_items_", list_group_name, ".", cat)) / n_valid,
             
             !!paste0("overall_prop_hesper_", list_group_name, ".", cat) := !!rlang::sym(paste0("nb_hesper_items_", list_group_name, ".", cat)) / all_serious_items,
             
             !!paste0("at_least_one_hesper_item_", list_group_name, ".", cat) := dplyr::case_when(
               n_valid == 0 ~ NA_real_, TRUE ~ rowSums(dplyr::across(any_of(list_group[[cat]]), ~. %in% c(choice_serious)), na.rm=T) > 0
             )
      ) 
  }
  
  df <- df %>% dplyr::select(-n_valid)
  
  return(df)
}
