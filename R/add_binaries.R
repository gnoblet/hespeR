#' Add as many binaries than input columns, taking the value 1 if value in each column is inside a set a values
#' 
#' Useful to create a set of dummy variables from a set of categorical variables if they are in a defined set of choices
#' For HESPER package, can be used to create binaries for all HESPER items select one columns, to either record if it's a serious problem, undefined, serious problem with subset
#' Additionnal argument can be used to add a suffix to the column names of the binary created

#' @param data A data.table or data.frame: The data to be processed.
#' @param cols_character A character vector: The name of the columns that will be used to create binary columns.
#' @param value_1 A character vector: Set of values for which binary columns will be populated with 1 if `cols_character` is inside this set.
#' @param value_0 A character vector: Set of values for which binary columns will be populated with 0 if `cols_character` is inside this set.
#' @param value_na A character vector: Set of values for which binary columns will be populated with NA if `cols_character` is inside this set. 
#' @param value_default A character vector: Set of values for which binary columns will be populated with otherwise. Will be overriden with 0 if value_0 = NULL and with NA if value_na = NULL
#' @param replace A logical: If TRUE, the original columns will be replaced by the binary columns. Default is FALSE.
#' @param name_suffix A character vector: The suffix to be added to the binary column names. Default is "binary".
#' @param sep A character vector: separator used to add name suffix to original column names
#'    
#'  
#'    
#'    
#'    
#'    
#'    

## example
source("../data-raw/hesper_dat.R")
hesper_vars <- c("hesper_drinking_water", "hesper_food", "hesper_shelter", "hesper_toilet", "hesper_clean", "hesper_clothes_etc", "hesper_income_livelihood",
                 "hesper_health", "hesper_distress", "hesper_safety", "hesper_education", "hesper_care", "hesper_support", "hesper_separation", "hesper_displaced", "hesper_information", "hesper_aid",
                 "hesper_respect", "hesper_movement", "hesper_time", "hesper_law", "hesper_gbv", "hesper_drug", "hesper_mental_health", "hesper_care_community")

data=hesper_dat
cols_character = hesper_vars
value_1= c("serious_problem")
value_na = c("undefined", "dnk", "pnta")
value_default = NULL
replace=F
name_suffix="binary"
sep="_"


## Add binaries populated with NA if undefeined, dnk or pnta, 1 if serious_problem and 0 otherwise
hesper_dat <- hesper_dat %>% 
  add_binaries(cols_character = hesper_vars, 
               value_1 = c("serious_problem"),
               value_na = c("undefined", "dnk", "pnta"),
               value_default = NULL,
               replace = F, 
               name_suffix = "binary", 
               sep = "_")

add_binaries <- function(
  data = df,
  cols_character,
  value_1,
  value_0=NULL,
  value_na=NA,
  replace = F,
  name_suffix="binary",
  sep=".",
  value_default=NA_integer_
) {
  
  ## if replace=T, force arguments name_suffix to "" and sep to ""
  if (replace) {
    name_suffix <- ""
    sep <- ""
  }
    
  ## if checkmate:: datatable not true, transform in DT
  if (!checkmate::testDataTable(data)){
    data <- data.table::as.data.table(data)
  }
  
  ## check that cols_character are in data, or stop and print colnames
  if (!all(cols_character %in% colnames(data))) {
    col_not_in_data <- cols_character %>% setdiff(colnames(data))
    stop(paste0("The following columns are not in the data: ", paste(col_not_in_data, collapse = ", ")))
  }  
  
  ## checkmate that cols_character are character
  checkmate::assert_character(cols_character)
  
  ## if value_0=NULL ensure that default is overridden to 0 
  if (is.null(value_0)){
    value_default <- 0
    warning("value_0 is NULL, default value is overridden to 0")
  }
  
  ## if value_na=NULL ensure that default is overridden to NA_integer_
  if (is.null(value_na)){
    value_default <- NA_integer_
    warning("value_na is NULL, default value is overridden to NA_integer_")
  }
  
  data[, (paste0(cols_character, sep, name_suffix)) := 
         lapply(.SD, \(x)
                fcase(
                  x %in% value_1, as.double(1),              # Assign 1 if x is in the set of values in vector `value_1`
                  x %in% value_0, as.double(0),              # Assign 0 if x is in the set of values in vector `value_0`
                  x %in% value_na, as.double(NA),    # Assign NA if x is in the set of values in vector `value_na` (NA_integer by default) 
                  default = as.double(value_default)          # Default value of NA for any other case, can be changed in parameters
                )
         ), .SDcols = cols_character]
  
  return(data)
  
}    
