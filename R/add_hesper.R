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
                            subset = T,
                            add_binaries_undefined = T
                            ){
  
  ## check that all columns are present in dataframe and print the non matching columns
  col_not_matching <- col_items %>% unname %>% unlist %>% purrr::keep(!. %in% colnames(df))
  if(length(col_not_matching) > 0){warning(paste("The following columns are not present in the dataframe: ", col_not_matching))}
  
  ## all response choices that should not be counted in the total nb of items
  choice_applicable <- c(choice_serious, choice_no_serious, choice_dnk, choice_pnta)
  choice_exclude <- c(NA, choice_na, choice_dnk, choice_pnta)
  
  ## compute total items selected
  df <- df %>%
    mutate(nb_hesper.all = rowSums(across(all_of(col_items), ~. %in% choice_serious), na.rm=T),
           nb_hesper.applicable = rowSums(across(all_of(col_items), ~ . %in% choice_applicable), na.rm=T),
           nb_hesper.undefined = rowSums(across(all_of(col_items), ~ . %in% c(choice_na, choice_dnk, choice_pnta)), na.rm=T),
           nb_hesper.pnta = rowSums(across(all_of(col_items), ~ . %in% choice_pnta), na.rm=T),
           nb_hepser.dnk = rowSums(across(all_of(col_items), ~ . %in% choice_dnk), na.rm=T)
           prop_hesper.all = nb_hesper.all / nb_hesper.applicable)
  
  ## Add binary columns to record serious problem, 0 if no serious problem and NA otherwise for all items across col_items
  ## Column names is original names with _binary suffix appended
  
  if (add_binaries){
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(col_items), ~dplyr::case_when(. %in% choice_serious ~ 1, . %in% c(choice_dnk, choice_pnta) ~ NA_real_, TRUE ~ 0), .names = "{.col}.binary"))
  }
  
  if (add_binaries_subset){
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::all_of(col_hesper_subset), ~dplyr::case_when(. %in% choice_serious ~ 1, . %in% choice_no_serious ~ 0, TRUE ~ NA_real_), .names = "{.col}.binary_subset"))
  }
  
  ## Add binary columns across all HESPER items recording if response is undefined
  if (add_binaries_undefined){
    df <- df %>% mutate(across(all_of(col_items), ~case_when(. %in% c(choice_na, choice_dnk, choice_pnta) ~ 1, TRUE ~ 0), .names = "{.col}.undefined"))  
  }
  
  ## collapse priority columns to have a select multiple column, create dummy binary child columns (with & without subset) & ensure that skip logic is respected for top three binaries
  if (!is.null(cols_priority)){
    
    ## stop and display columns that are not in df
    if (sum(!cols_priority %in% colnames(df))>0) stop("The following columns are not present in the dataframe: ", cols_priority[!cols_priority %in% colnames(df)])
    
    ## unite the thre priority columns to have one select multiple hesper priorities
    df <- df %>% add.top.three(new_col = col_name_hesper_top_three, cols_unite = cols_priority)

    ### expand parent column top three priorities and priority without accounting for subset
    df <- df %>% expand.select.multiple.vec(c(col_name_hesper_top_three, cols_priority))
    
    ### ensure that skip logic are respected
    ### to avoid having binaries with zero for items that are not applicable to the respondent 
    
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
      ## ensure that the binaries are not 0 if the item is not supposed to be asked to the respondent according to the tool
      col_prio_male <- if (!any(is.null(hesper_item_male)|is.na(hesper_item_male))) map(cols_priority, ~ paste0(., ".", hesper_item_male, "_subset")) %>% unlist else NULL
      col_prio_female <- if (!any(is.null(hesper_item_female)|is.na(hesper_item_female))) map(cols_priority, ~ paste0(., ".", hesper_item_female, "_subset")) %>% unlist else NULL
      col_prio_displaced <- if (!any(is.null(hesper_item_displaced)|is.na(hesper_item_displaced))) map(cols_priority, ~ paste0(., ".", hesper_item_displaced, "_subset")) %>% unlist else NULL
      col_prio_non_displaced <- if (!any(is.null(hesper_item_non_displaced)|is.na(hesper_item_non_displaced))) map(cols_priority, ~ paste0(., ".", hesper_item_non_displaced, "_subset")) %>% unlist else NULL
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

