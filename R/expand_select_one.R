expand.select.one <- function(df, var, val.parent.na=NA, sep="."){
  unique <- df %>% pull(!!sym(var)) %>% unique %>% na.omit %>% as.character
  for (val in unique){
    bin.col <- paste0(var, sep, val)
    df <<- df %>% 
      mutate(!!sym(bin.col) := case_when(!!sym(var) %in% val.parent.na ~ NA_real_, 
                                         !!sym(var) == val ~ 1,
                                         TRUE ~ 0), .after=!!sym(var))
  }
  return(df)
}

expand.select.one.vec <- function(df, x=c(), ...){
  for (x in var) {df <- df %>% expand.select.one(var,...)}
  return(df)
}