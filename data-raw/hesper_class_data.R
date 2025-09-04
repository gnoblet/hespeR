set.seed(1234)

# Hesper vars and options
hesper_vars <- c(
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
hesper_opts <- c(
  "serious_problem",
  "no_serious_problem",
  "dnk",
  "pnta",
  "not_applicable"
)

# Other list vars and options
gender_vars <- c("hoh_gender", "resp_gender")
gender_opts <- c("female", "male", "other", "dnk", "pnta")
dis_vars <- c("pop_group")
dis_opts <- c("idp", "hosts", "refugees", "undefined", "other")

# Create dataframe sampling each group of vars by opts, no NA allowed
l <- list(
  hesper = list(vars = hesper_vars, opts = hesper_opts),
  gender = list(vars = gender_vars, opts = gender_opts),
  dis = list(vars = dis_vars, opts = dis_opts)
)

# Prepare datasets
hesper_df <- purrr::map(hesper_vars, \(x) {
  x = sample(hesper_opts, 10000, replace = TRUE)
}) |>
  purrr::set_names(hesper_vars) |>
  as.data.frame()

gender_df <- purrr::map(gender_vars, \(x) {
  x = sample(gender_opts, 10000, replace = TRUE)
}) |>
  purrr::set_names(gender_vars) |>
  as.data.frame()
dis_df <- purrr::map(dis_vars, \(x) {
  x = sample(dis_opts, 10000, replace = TRUE)
}) |>
  purrr::set_names(dis_vars) |>
  as.data.frame()

# Bind all and add key id
hesper_dat <- purrr::list_cbind(list(gender_df, dis_df, hesper_df))
library(data.table)
setDT(hesper_dat)
hesper_dat[, uuid := .I]

# Sample priorities out of the existing serious problems
hesper_dat[,
  'hesper_priorities' := {
    # Get serious problems var names
    probs <- names(.SD)[.SD == "serious_problem"]
    # If not empty, sample
    if (length(probs)) {
      paste(sample(probs, min(3, length(probs))), collapse = " ")
    } else {
      NA_character_
    }
  },
  by = "uuid",
  .SDcols = hesper_vars
]

# Split by " " and add to the priority columns
hesper_dat[,
  c(
    "hesper_priority_first",
    "hesper_priority_second",
    "hesper_priority_third"
  ) := tstrsplit(hesper_priorities, " ", fixed = TRUE, fill = NA_character_)
]

# Save
usethis::use_data(hesper_dat, overwrite = TRUE)
