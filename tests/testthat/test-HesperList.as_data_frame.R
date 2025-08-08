test_that("as_data_frame and from_data_frame works for HesperList", {
  hv <- hesper_vars()[1:2]
  ho1 <- hesper_opts()[1:3]
  ho2 <- hesper_opts()[3:5]

  hv1 <- HesperVector(
    hesper_var = hv[1],
    hesper_vals = ho1,
    allow_missing = FALSE
  )
  hv2 <- HesperVector(
    hesper_var = hv[2],
    hesper_vals = ho2,
    allow_missing = FALSE
  )
  hl <- HesperList(hesper_list = list(hv1, hv2))

  df <- as_data_frame(hl)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_equal(colnames(df), c('hesper_drinking_water', 'hesper_food'))
  expect_equal(attributes(df)$allow_missing, c(FALSE, FALSE))
  expect_equal(
    df$hesper_drinking_water,
    c("serious_problem", "no_serious_problem", "dnk")
  )
  expect_equal(df$hesper_food, c("dnk", "pnta", "not_applicable"))

  hl2 <- from_data_frame(df)

  expect_s7_class(hl2, HesperList)
  expect_equal(
    purrr::map_chr(hl2@hesper_list, \(x) x@hesper_var),
    purrr::map_chr(hl@hesper_list, \(x) x@hesper_var)
  )
  expect_equal(
    purrr::map(hl2@hesper_list, \(x) x@hesper_vals),
    purrr::map(hl@hesper_list, \(x) x@hesper_vals)
  )
  expect_equal(
    purrr::map_lgl(hl2@hesper_list, \(x) x@allow_missing),
    purrr::map_lgl(hl@hesper_list, \(x) x@allow_missing)
  )
})
