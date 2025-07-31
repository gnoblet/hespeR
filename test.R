library(hespeR)
hespeR::HesperDefault()

source("data-raw/hesper_dat.R")


# Sample data
df <- data.table(
  col1 = sample(c("yes", "no", NA), 10, replace = TRUE),
  col2 = sample(c("yes", "no", NA), 10, replace = TRUE)
)

hesper_dat |> head()


hesper_vars = c(
  "hesper_drinking_water",
  "hesper_food",
  "hesper_shelter",
  "hesper_toilet",
  "hesper_clean",
  "hesper_clean_female",
  "hesper_clothes_etc",
  "hesper_income_livelihood",
  "hesper_health",
  "hesper_health_care_male",
  "hesper_health_care_female",
  "hesper_distress",
  "hesper_safety",
  "hesper_education",
  "hesper_care",
  "hesper_support",
  "hesper_separation",
  "hesper_displaced",
  "hesper_information",
  "hesper_aid",
  "hesper_respect",
  "hesper_movement",
  "hesper_time",
  "hesper_law",
  "hesper_gbv",
  "hesper_drug",
  "hesper_mental_health",
  "hesper_care_community",
  "hesper_other"
)

tt <- add_hesper_bin(hesper_dat)


library(dplyr)
tt <- recode_subset_to_missing(
  hesper_dat,
  "hesper_law",
  "pop_group",
  c("refugees", "idps"),
  suffix = "subset"
)
tt |>
  select(hesper_law, hesper_law_subset, pop_group)


tt <- sum_vals_across(hesper_dat, hesper_vars, "serious_problem", "new")
