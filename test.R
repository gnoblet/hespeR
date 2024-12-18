source("data-raw/hesper_dat.R")


# Sample data
df <- data.table(
  col1 = sample(c("yes", "no", NA), 10, replace = TRUE),
  col2 = sample(c("yes", "no", NA), 10, replace = TRUE)
)
 
# Parameters
cols_character <- c("col1", "col2")
value_in <- "yes"
sep <- "."
name_suffix <- "binary"
 
df <- df %>%
  mutate(
    across(all_of(cols_character),
           ~ case_when(
             . %in% value_in ~ 1,
             !is.na(.) ~ 0,
             TRUE ~ NA_real_
           ),
           .names = "{.col}{sep}{name_suffix}"
    ))
 

# Apply transformation
df[, (paste0(cols_character, sep, name_suffix)) := lapply(.SD, \(x) 
  fcase(
    x %in% value_in, 1L,              # Assign 1 if value matches `value_in`
    !is.na(x), 0L,                   # Assign 0 if not NA and not matching `value_in`
    default = NA_integer_            # Default to NA for missing values
  )
), .SDcols = cols_character]

df <- expand_bin(df = data, vars = cols_character, split_by = " ", bin_sep = ".", drop_undefined = NULL, value_in = "yes", value_in_suffix = "binary")
