expand.select.multiple <- function(df, var, val.parent.na=NA, sep="."){
  unique <- df %>% pull(!!sym(var)) %>% str_split(" ") %>% unlist %>% unique %>% na.omit %>% as.character
  unique <- unique[!unique %in% ""]
  ## use for loop as faster
  for (val in unique){
    bin.col <- paste0(var, sep, val)
    df <- df %>% 
      mutate(!!sym(bin.col) := case_when(!!sym(var) %in% val.parent.na ~ NA_real_, 
                                         str_detect(!!sym(var), paste0("(^| )", 
                                                                       str_replace_all(val, c("\\("="\\\\\\(", "\\)"="\\\\\\)", "\\'"="\\\\\\'", "\\/"="\\\\\\/")), 
                                                                       "($| )")) ~ 1,
                                         TRUE ~ 0), .after=!!sym(var))
  }
  return(df)
}

expand.select.multiple.vec <- function(df, x=c(),...){
  for (x in var){df <- df %>% expand.select.multiple(var,...)}
  return(df)
}
