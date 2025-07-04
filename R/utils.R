warn_replace <- function(df, vars) {
  # Remove from df the names.bin binary columns if they exist, and warn for replacement
  vars_in_lgl <- vars %in% colnames(df)
  if (any(vars_in_lgl)) {
    vars_in <- vars[vars_in_lgl]
    rlang::warn(paste0(
      "Variable(s) ",
      paste(vars_in, collapse = ", "),
      " already present in df. They will be replaced."
    ))
  }
}

warn_removal <- function(df, vars) {
  # Remove from df the names.bin binary columns if they exist, and warn for removal
  vars_in_lgl <- vars %in% colnames(df)
  if (any(vars_in_lgl)) {
    vars_in <- vars[vars_in_lgl]
    rlang::warn(paste0(
      "Variable(s) ",
      paste(vars_in, collapse = ", "),
      " will be removed."
    ))
  }
}


colors_reach <- c(
  main_red = "#EE5859",
  main_lt_grey = "#C7C8CA",
  main_beige = "#D2CBB8",
  iroise_1 = "#DFECEF",
  iroise_2 = "#B1D7E0",
  iroise_3 = "#699DA3",
  iroise_4 = "#236A7A",
  iroise_5 = "#0C3842",
  white = "#FFFFFF",
  black = "#000000",
  main_grey = "#58585A",
  red_main_1 = "#AE2829",
  red_main_2 = "#D05E5F",
  red_main_3 = "#DB9797",
  red_main_4 = "#EBC7C8",
  red_main_5 = "#FAF2F2",
  red_alt_1 = "#792a2e",
  red_alt_2 = "#c0474a",
  red_alt_3 = "#ee5859",
  red_alt_4 = "#f49695",
  red_alt_5 = "#f8d6d6",
  red_alt_na = "#f8f4f4",
  lt_grey_1 = "#C6C6C6",
  lt_grey_2 = "#818183",
  grey3 = "#E3E3E3",
  dk_grey = "#464647",
  two_dots_1 = "#706441",
  two_dots_2 = "#56b4e9",
  two_dots_flashy_1 = "gold1",
  two_dots_flashy_2 = "blue2",
  three_dots_1 = "aquamarine2",
  three_dots_2 = "cornflowerblue",
  three_dots_3 = "brown1",
  orpink = "#f8aa9b",
  pink = "#f5a6a7",
  lt_pink = "#F9C6C7",
  hot_pink = "#ef6d6f",
  mddk_red = "#bf4749",
  dk_red = "#782c2e",
  orange = "#F69E61",
  lt_green = "#B0CFAC",
  green = "#84A181",
  dk_green = "#526450",
  red_less_4_1 = "#f6e3e3",
  red_less_4_2 = "#f3b5b6",
  red_less_4_3 = "#ee5a59",
  red_less_4_4 = "#9d393c",
  red_5_1 = "#f6e3e3",
  red_5_2 = "#f3b5b6",
  red_5_3 = "#ee5a59",
  red_5_4 = "#c0474a",
  red_5_5 = "#792a2e",
  red_less_7_1 = "#f8f4f4",
  red_less_7_2 = "#f8d6d6",
  red_less_7_3 = "#f49695",
  red_less_7_4 = "#ee5a59",
  red_less_7_5 = "#c0474a",
  red_less_7_6 = "#792a2e",
  red_less_7_7 = "#471119",
  green_2_1 = "#cce5c9",
  green_2_2 = "#55a065",
  green_3_1 = "#e6f2e0",
  green_3_2 = "#7ebf85",
  green_3_3 = "#2d8246",
  green_4_1 = "#e6f2e1",
  green_4_2 = "#b0d3ab",
  green_4_3 = "#4bab5e",
  green_4_4 = "#0c592e",
  green_5_1 = "#e6f2e1",
  green_5_2 = "#b0d3ab",
  green_5_3 = "#6bb26a",
  green_5_4 = "#229346",
  green_5_5 = "#0c592e",
  green_6_1 = "#e6f2e0",
  green_6_2 = "#b0d3ab",
  green_6_3 = "#75c376",
  green_6_4 = "#086d38",
  green_6_5 = "#0c592e",
  green_6_6 = "#0d4420",
  green_7_1 = "#fafafa",
  green_7_2 = "#e6f2e0",
  green_7_3 = "#b0d3ab",
  green_7_4 = "#75c376",
  green_7_5 = "#40ab5d",
  green_7_6 = "#086d38",
  green_7_7 = "#0d4420",
  artichoke_2_1 = "#b6c8b1",
  artichoke_2_2 = "#53755f",
  artichoke_3_1 = "#e4f1db",
  artichoke_3_2 = "#89a087",
  artichoke_3_3 = "#455843",
  artichoke_4_1 = "#e4f1db",
  artichoke_4_2 = "#b5ceb2",
  artichoke_4_3 = "#89a087",
  artichoke_4_4 = "#465944",
  artichoke_5_1 = "#e4f1db",
  artichoke_5_2 = "#b5ceb2",
  artichoke_5_3 = "#89a087",
  artichoke_5_4 = "#60755f",
  artichoke_5_5 = "#465944",
  artichoke_6_1 = "#fafafa",
  artichoke_6_2 = "#e4f1db",
  artichoke_6_3 = "#b5ceb2",
  artichoke_6_4 = "#89a087",
  artichoke_6_5 = "#60755f",
  artichoke_6_6 = "#455843",
  artichoke_7_1 = "#fafafa",
  artichoke_7_2 = "#e4f1db",
  artichoke_7_3 = "#b5ceb2",
  artichoke_7_4 = "#9fb89c",
  artichoke_7_5 = "#89a087",
  artichoke_7_6 = "#60755f",
  artichoke_7_7 = "#455843",
  blue_2_1 = "#7cb6c4",
  blue_2_2 = "#286877 ",
  blue_3_1 = "#b9d7de",
  blue_3_2 = "#5ca4b4",
  blue_3_3 = "#286877",
  blue_4_1 = "#dfecef",
  blue_4_2 = "#8fc1cc",
  blue_4_3 = "#3f96aa",
  blue_4_4 = "#286877",
  blue_5_1 = "#dfecef",
  blue_5_2 = "#8fc1cc",
  blue_5_3 = "#3f96aa",
  blue_5_4 = "#256a7a",
  blue_5_5 = "#0c3842",
  blue_6_1 = "#f4fbfe",
  blue_6_2 = "#cfe4e9",
  blue_6_3 = "#77b2bf",
  blue_6_4 = "#4096aa",
  blue_6_5 = "#256a7a",
  blue_6_6 = "#0c3842",
  blue_7_1 = "#f4fbfe",
  blue_7_2 = "#b3d5de",
  blue_7_3 = "#77b2bf",
  blue_7_4 = "#4096aa",
  blue_7_5 = "#27768a",
  blue_7_6 = "#0c596b",
  blue_7_7 = "#0c3842"
)

# ## quick utils function to run weighted analysis
# analyse_ci <- function(df, group_var=NULL, var, col_weight, col_strata=NULL){
#   if (any(!var %in% colnames(df))) {
#     print(paste0("Colnames ", paste0(var[!var %in% colnames(df)], collapse="; ") , " not in dataset. Will be excluded from analysis"))
#     var <- var[var %in% colnames(df)]
#   }

#   df <- df |>
#     srvyr::as_survey_design(weights=!!rlang::sym(col_weight), strata=!!rlang::sym(col_strata)) |> srvyr::group_by(!!!syms(group_var))

# <<<<<<< HEAD
#   df <- df %>%
#     srvyr::summarise(srvyr::across(srvyr::all_of(var), list(mean=~srvyr::survey_mean(., vartype="ci", na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) %>%
#     dplyr::rename_with(.fn = ~stringr::str_replace_all(., c("_(?=(low|upp))"="\\/"))) %>%
#     tidyr::pivot_longer(where(is.numeric)) %>% tidyr::separate(name, c("question", "choice.key"), sep = "\\.", remove = T) %>%
#     tidyr::separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) %>% tidyr::pivot_wider(names_from = fn, values_from = value) %>%
#     add_key(group_var)
# }
#
# analyse <- function(df, group_var=NULL, var, col_weight, col_strata=NULL){
#   if (any(!var %in% colnames(df))) {
#     print(paste0("Colnames ", paste0(var[!var %in% colnames(df)], collapse="; ") , " not in dataset. Will be excluded from analysis"))
#     var <- var[var %in% colnames(df)]
#   }
#   df <- df %>% dplyr::group_by(!!!syms(group_var)) %>%
#     dplyr::summarise(dplyr::across(dplyr::all_of(var), list(mean=~weighted.mean(., w=!!rlang::sym(col_weight), na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) %>%
#     tidyr::pivot_longer(where(is.numeric)) %>% tidyr::separate(name, c("question", "choice.key"), sep = "\\.", remove = T) %>%
#     tidyr::separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) %>% tidyr::pivot_wider(names_from = fn, values_from = value) %>%
#     add_key(group_var)
# }
# =======
#   df <- df |>
#     srvyr::summarise(srvyr::across(dplyr::all_of(var), list(mean=~srvyr::survey_mean(., vartype="ci", na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) |>
#     dplyr::rename_with(.fn = ~stringr::str_replace_all(., c("_(?=(low|upp))"="\\/"))) |>
#     tidyr::pivot_longer(where(is.numeric)) |> tidyr::separate(name, c("question", "choice.key"), sep = "\\.", remove = T) |>
#     tidyr::separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) |> tidyr::pivot_wider(names_from = fn, values_from = value)
# }

# analyse <- function(df, group_var=NULL, var, col_weight, col_strata=NULL){
#   if (any(!var %in% colnames(df))) {
#     print(paste0("Colnames ", paste0(var[!var %in% colnames(df)], collapse="; ") , " not in dataset. Will be excluded from analysis"))
#     var <- var[var %in% colnames(df)]
#   }
#   df <- df |> dplyr::group_by(!!!syms(group_var)) |>
#     dplyr::summarise(dplyr::across(dplyr::all_of(var), list(mean=~weighted.mean(., w=!!rlang::sym(col_weight), na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) |>
#     tidyr::pivot_longer(where(is.numeric)) |> tidyr::separate(name, c("question", "choice.key"), sep = "\\.", remove = T) |>
#     tidyr::separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) |> tidyr::pivot_wider(names_from = fn, values_from = value)
# }
# >>>>>>> 8977479f8a3e972b03a3d1fadb5d83396b1a40a6

# mark_significance <- function(df=sum.hoh.gender, treshold=0.01){
#   df |> dplyr::group_by(country, question, choice) |>
#     dplyr::mutate(no_overlap=ifelse(max(`mean/low`)>min(`mean/upp`) & mean(mean)>treshold, "*", "")) %>%
#     dplyr::arrange(country, question, choice)
# }

# first_up <- function(x) {
#   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
#   return(x)
# }
