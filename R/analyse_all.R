# analyse_all <- function(df, group_var=NULL, 
#                         var_select, 
#                         var_int,
#                         col_weight=NULL){
#   if (is.null(col_weight)) {
#     col_weight <- "weight"
#     df$weight <- 1
#   }
#   var <- c(var_select, var_int)
#   if (any(!var %in% colnames(df))) {
#     print(paste0("Colnames ", paste0(var[!var %in% colnames(df)], collapse="; ") , " not in dataset. Will be excluded from analysis"))
#     var_select <- var_select[var_select %in% colnames(df)]
#     var_int <- var_int[var_int %in% colnames(df)]
#   }
#   df <- df |> 
#     dplyr::group_by(!!!rlang::syms(group_var)) |> 
#     dplyr::summarise(dplyr::across(dplyr::all_of(var_select), list(prop=~weighted.mean(., w=!!rlang::sym(col_weight), na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.)))),
#               dplyr::across(dplyr::all_of(var_int), list(mean=~weighted.mean(., w=!!rlang::sym(col_weight), na.rm=T), median=~median(., na.rm=T), n=~sum(!is.na(.))))) |> 
#     tidyr::pivot_longer(where(is.numeric)) |> tidyr::separate(name, c("question", "choice.key"), sep = "\\.", remove = T) |> 
#     tidyr::separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) |> 
#     tidyr::mutate(fn=ifelse(is.na(choice), stringr::str_replace_all(question, ".*_", ""), fn), 
#            question=ifelse(is.na(choice), stringr::str_replace_all(question, "_[^_]*$", ""), question)) |> 
#     tidyr::pivot_wider(names_from = fn, values_from = value)
# }
