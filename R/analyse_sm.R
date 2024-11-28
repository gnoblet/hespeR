analyse_sm <- function(df, group_var=NULL, var, col_weight, type="quick"){
  if (type=="quick") {
    analyse_sm_quick(df, group_var, var, col_weight)
  } else if (type=="ci") {
    analyse_sm_ci(df, group_var, var, col_weight)
  } else {
    print("Type must be either 'quick' or 'ci'")
  }
}

analyse_sm_ci <- function(df, group_var=NULL, var, col_weight){
  if (any(!var %in% colnames(df))) {
    print(paste0("Colnames ", paste0(var[!var %in% colnames(df)], collapse="; ") , " not in dataset. Will be excluded from analysis"))
    var <- var[var %in% colnames(df)]
  } 
  df <- df %>% 
    as_survey_design(weights=!!sym(col_weight)) %>% group_by(!!!syms(group_var)) %>%
    summarise(across(all_of(var), list(mean=~survey_mean(., vartype="ci", na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) %>%
    rename_with(.fn = ~str_replace_all(., c("_(?=(low|upp))"="\\/"))) %>% 
    pivot_longer(where(is.numeric)) %>% separate(name, c("question", "choice.key"), sep = "\\.", remove = T) %>%
    separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) %>% pivot_wider(names_from = fn, values_from = value)
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
  df <- df %>% group_by(!!!syms(group_var)) %>%
    summarise(across(all_of(var), list(mean=~weighted.mean(., w=!!sym(col_weight), na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) %>%
    pivot_longer(where(is.numeric)) %>% separate(name, c("question", "choice.key"), sep = "\\.", remove = T) %>%
    separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) %>% pivot_wider(names_from = fn, values_from = value)
}
