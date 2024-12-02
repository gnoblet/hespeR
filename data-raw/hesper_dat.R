set.seed(1234)

# Variables and options 

## hesper
hesper_vars <- c("hesper_drinking_water", "hesper_food", "hesper_shelter", "hesper_toilet", "hesper_clean", "hesper_clean", "hesper_clothes_etc", "hesper_income_livelihood", "hesper_health", "hesper_health",
"hesper_health", "hesper_distress", "hesper_safety", "hesper_education", "hesper_care", "hesper_support", "hesper_separation", "hesper_displaced", "hesper_information", "hesper_aid",
"hesper_respect", "hesper_movement", "hesper_time", "hesper_law", "hesper_gbv", "hesper_drug", "hesper_mental_health", "hesper_care_community")
hesper_opts <- c("serious_problem", "no_serious_problem", "dnk", "pnta")

# hoh gender and respondent gender
gender_vars <- c("hoh_gender", "resp_gender")
gender_opts <- c("female", "male", "other", "dnk", "pnta")

# displacement
dis_vars <- c("dis")
dis_opts <- c("idp", "hosts", "refugees", "undefined", "other")

# Create dataframe sampling each group of vars by opts, no NA allowed
l <- list(
  hesper = list(vars = hesper_vars, opts = hesper_opts),
  gender = list(vars = gender_vars, opts = gender_opts),
  dis = list(vars = dis_vars, opts = dis_opts)
)

# Prepare datasets
hesper_df <- lapply(hesper_vars, \(x) x = sample(hesper_opts, 10000, replace = TRUE)) |> 
  setNames(hesper_vars) |> 
  as.data.frame()
## generate top 1/2/3 priorities
hesper_df <- hesper_df %>%
  mutate(across(all_of(hesper_vars), ~ case_when(. %in% "serious_problem" ~ cur_column()), .names="{.col}_name")) %>%
  unite(col = "all.selected", paste0(hesper_vars, "_name"), na.rm = T, sep = " ", remove=T) %>%
  mutate(hesper_priorities = str_split(all.selected, " ") %>% 
           map_chr(~str_c(sample(.x, size = min(3, length(.x))), collapse = " "))) %>%
  mutate(hesper_priority_first = map(hesper_priorities, ~unlist(str_split(., " "))[1]) %>% unlist,
         hesper_priority_second = map(hesper_priorities, ~unlist(str_split(., " "))[2]) %>% unlist,
         hesper_priority_third = map(hesper_priorities, ~unlist(str_split(., " "))[3]) %>% unlist) 

gender_df <- lapply(gender_vars, \(x) x = sample(gender_opts, 10000, replace = TRUE)) |> 
  setNames(gender_vars) |> 
  as.data.frame()
dis_df <- lapply(dis_vars, \(x) x = sample(dis_opts, 10000, replace = TRUE)) |> 
  setNames(dis_vars) |> 
  as.data.frame()

# Bind all and add key id
hesper_dat <- cbind(hesper_df, gender_df, dis_df) 
library(data.table)
setDT(hesper_dat)
hesper_dat[, uuid := paste0("uuid_", .I)]

usethis::use_data(hesper_dat, overwrite = TRUE)
