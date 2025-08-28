# Tests for create_binary_vectors function

test_that("create_binary_vectors works with basic character vectors", {
  vec_chr <- c("a", "b", "a", "c")
  ref_chr <- c("a", "b", "c")

  result <- create_binary_vectors(vec_chr, ref_chr)

  expect_type(result, "list")
  expect_named(result, c("a", "b", "c"))
  expect_equal(result$a, c(1L, 0L, 1L, 0L))
  expect_equal(result$b, c(0L, 1L, 0L, 0L))
  expect_equal(result$c, c(0L, 0L, 0L, 1L))
})

test_that("create_binary_vectors handles missing values", {
  vec_chr <- c("a", "b", NA, "c")
  ref_chr <- c("a", "b", "c")

  result <- create_binary_vectors(vec_chr, ref_chr)

  expect_equal(result$a, c(1L, 0L, NA_integer_, 0L))
  expect_equal(result$b, c(0L, 1L, NA_integer_, 0L))
  expect_equal(result$c, c(0L, 0L, NA_integer_, 1L))
})

test_that("create_binary_vectors handles empty reference values", {
  vec_chr <- c("a", "b", "c")
  ref_chr <- character(0)

  expect_error(result <- create_binary_vectors(vec_chr, ref_chr))
})

test_that("create_binary_vectors handles single element vectors", {
  vec_chr <- "a"
  ref_chr <- c("a", "b")

  result <- create_binary_vectors(vec_chr, ref_chr)

  expect_equal(result$a, 1L)
  expect_equal(result$b, 0L)
})

test_that("create_binary_vectors handles no matches", {
  vec_chr <- c("x", "y", "z")
  ref_chr <- c("a", "b", "c")

  result <- create_binary_vectors(vec_chr, ref_chr)

  expect_equal(result$a, c(0L, 0L, 0L))
  expect_equal(result$b, c(0L, 0L, 0L))
  expect_equal(result$c, c(0L, 0L, 0L))
})

test_that("create_binary_vectors preserves vector length", {
  vec_chr <- c("a", "b", "a", "c", "b", "a")
  ref_chr <- c("a", "b")

  result <- create_binary_vectors(vec_chr, ref_chr)

  expect_length(result$a, 6)
  expect_length(result$b, 6)
  expect_equal(result$a, c(1L, 0L, 1L, 0L, 0L, 1L))
  expect_equal(result$b, c(0L, 1L, 0L, 0L, 1L, 0L))
})

test_that("create_binary_vectors works with duplicate reference values", {
  vec_chr <- c("a", "b", "a")
  ref_chr <- c("a", "a", "b")

  result <- create_binary_vectors(vec_chr, ref_chr)

  expect_named(result, c("a", "a", "b"))
  expect_length(result, 3)
  # Both "a" entries should have the same binary vector
  expect_equal(result[[1]], c(1L, 0L, 1L))
  expect_equal(result[[2]], c(1L, 0L, 1L))
  expect_equal(result$b, c(0L, 1L, 0L))
})
