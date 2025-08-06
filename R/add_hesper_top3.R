# #' @title function that compute for each respondent the number of selected items, number of applicable items and collapse the three ordered priorities into one select multiple column
# #'
# #' @description The function computes the number of selected items for each respondent, the number of applicable items and collapse the three ordered priorities into one select multiple column
# #' @typed df:
# #'   dataframe containing the cleaned data
# #' @typed col_items:
# #'   vector of column names containing the hesper items.
# #' @typed choice_serious:
# #'   character string representing the choice for serious problem
# #' @typed choice_no_serious:
# #'   character string representing the choice for no serious problem
# #' @typed choice_dnk:
# #'   character string representing the choice for do not know
# #' @typed choice_pnta:
# #'   character string representing the choice for decline to answer
# #' @typed choice_na:
# #'   character string representing the choice for not applicable to household
# #' @typed vars_priority:
# #'   vector of three column names containing the first, second and third priority hesper items
# #' @typed var_hesper_top_three:
# #'   character string representing the new column name containing the three ordered priorities collapsed into one select multiple column
# #' @typed sv_l:
# #'   list of named lists for each element should be named and containing in hesper_vars the list of hesper item(s) asked only to the subset, in subset_var the name of the column which defines the subset (for example population_group or respondent_gender) and in subset_vals the set of values contained in subset_var defining the relevant subset
# #' @typedreturn
# #'   dataframe with the number of selected items, the number of applicable items and the three ordered priorities collapsed into one select multiple column
# #'
# #' @export
# #'
# add_hesper_top3 <- function(
#   df,
#   vars_priority = c(
#     "hesper_priority_first",
#     "hesper_priority_second",
#     "hesper_priority_third"
#   ),
#   var_hesper_top_three = "hesper_top_three_priorities",
#   sv_l = list(
#     displaced = list(
#       hesper_vars = c("hesper_displaced"),
#       subset_var = "pop_group",
#       subset_vals = c("refugees", "idp")
#     ),
#     resp_gender_female = list(
#       hesper_vars = c("hesper_clean_female"),
#       subset_var = "resp_gender",
#       subset_vals = c("female")
#     )
#   )
# ) {
#   #------ Checks

#   # df is a dataframe
#   checkmate::assertDataFrame(df)

#   # df is not data.table, convert it
#   if (!checkmate::testDataTable(df)) {
#     rlang::warn("Converting df to data.table.")
#     data.table::setDT(df)
#   }

#   ## check that vars_priority is character vector
#   checkmate::assert_character(vars_priority)

#   # hesper_vars are unique
#   check_dupes(vars_priority, "The following vars are duplicated: ")

#   # check that there are three vars in vars_priority
#   if (length(vars_priority) != 3) {
#     rlang::abort("vars_priority should contain three variables names.")
#   }

#   # all vars are in df
#   check_vars_in_df(df, vars_priority)

#   # hesper_vars are character vars in df
#   check_vars_class_in_df(df, vars_priority, "character")

#   # sv_l is a named list
#   checkmate::assertList(sv_l, names = "unique")

#   # sv_l items has three items only that are named hesper_vars, subset_var and subset_vals:
#   check_sv_l(sv_l, df, 'hesper_vars')

#   # collapse priority columns to a select multiple column, create dummy binary child columns (with/without subset) + ensure respect skip logic for top three child col

#   ## unite the thre priority columns to have one select multiple hesper priorities
#   df <- add_top3(df, new_var = var_hesper_top_three, vars_unite = vars_priority)

#   ## expand parent column top three priorities and priority 1/2/3 without accounting for subset
#   df <- expand_bin(df, c(vars_priority, var_hesper_top_three))

#   ## For each subset clean the child columns ensure that skip logic are respected for top three to avoid having binaries with zero for items that are not applicable to the respondent
#   df <- clean_top_priorities_subset(
#     df,
#     col_prio = c(vars_priority, var_hesper_top_three),
#     sv_l_val = sv_l,
#     choice_suffix_subset = NULL
#   )

#   return(df)
# }
