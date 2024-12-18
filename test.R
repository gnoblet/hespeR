source("data-raw/hesper_dat.R")


# Sample data
df <- data.table(
  col1 = sample(c("yes", "no", NA), 10, replace = TRUE),
  col2 = sample(c("yes", "no", NA), 10, replace = TRUE)
)

hesper_dat |> head()

tt <- add_hesper_bin(hesper_dat)
