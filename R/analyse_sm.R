analyse_sm <- function(df, group_var=NULL, var, col_weight, type="quick"){
  if (type=="quick") {
    res <- analyse_sm_quick(df, group_var, var, col_weight)
  } else if (type=="ci") {
    res <- analyse_sm_ci(df, group_var, var, col_weight)
  } else {
    print("Type must be either 'quick' or 'ci'")
  }
  res <- res %>% add_key(group_var, type, delete.old.col=T)
}

analyse_sm_ci <- function(df, group_var=NULL, var, col_weight){
  if (any(!var %in% colnames(df))) {
    print(paste0("Colnames ", paste0(var[!var %in% colnames(df)], collapse="; ") , " not in dataset. Will be excluded from analysis"))
    var <- var[var %in% colnames(df)]
  }
  df <- df |>
    srvyr::as_survey_design(weights=!!rlang::sym(col_weight)) |> dplyr::group_by(!!!rlang::syms(group_var)) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(var), list(mean=~survey_mean(., vartype="ci", na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) |>
    dplyr::rename_with(.fn = ~stringr::str_replace_all(., c("_(?=(low|upp))"="\\/"))) |>
    tidyr::pivot_longer(where(is.numeric)) |> tidyr::separate(name, c("question", "choice.key"), sep = "\\.", remove = T) |>
    tidyr::separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) %>% tidyr::pivot_wider(names_from = fn, values_from = value)
}

analyse_sm_quick <- function(df, group_var=NULL, var, col_weight=NULL){
  if (is.null(col_weight)) {
    col_weight <- "weight"
    df$weight <- 1
  }
  if (any(!var %in% colnames(df))) {
    print(paste0("Colnames ", paste0(var[!var %in% colnames(df)], collapse="; ") , " not in dataset. Will be excluded from analysis"))
    var <- var[var %in% colnames(df)]
  }
  df <- df |> dplyr::group_by(!!!rlang::syms(group_var)) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(var), list(mean=~weighted.mean(., w=!!rlang::sym(col_weight), na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) |>
    tidyr::pivot_longer(where(is.numeric)) |> tidyr::separate(name, c("question", "choice.key"), sep = "\\.", remove = T) |>
    tidyr::separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) |> tidyr::pivot_wider(names_from = fn, values_from = value)
}
