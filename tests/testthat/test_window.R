library(mmpr)
context("Sliding window API")

test_that("test window_bounds", {
  expect_equal(window_bounds(1, 10), c(-3, 6))
  expect_equal(window_bounds(5, 10), c(1, 10))
  expect_equal(window_bounds(5, 10, align="center"), c(1, 10))
  expect_equal(window_bounds(5, 10, align="right"), c(-4, 5))
  expect_equal(window_bounds(5, 10, align="left"), c(5, 14))
  expect_equal(window_bounds(5, 10, align="Left"), c(NA, NA))
  expect_equal(window_bounds(5, 10, align=""), c(NA, NA))
})

test_that("test create_window", {
  # if we make a partial centered window of 10 points around 1,
  # we expect 5 points
  expect_equal(nrow(create_window(1, 10, 1:50)), 6)
  # if we make a partial centered window of 10 points around 5,
  # we expect 9 points
  expect_equal(nrow(create_window(5, 10, 1:50)), 10)
  # if we make a partial centered window of 10 points around 6,
  # we expect 10 points
  expect_equal(nrow(create_window(6, 10, 1:50)), 10)
  # if we make a partial centered window of 20 points around 1,
  # we expect 10 points
  expect_equal(nrow(create_window(1, 20, 1:50)), 11)
  # if we make a non-partial centered window of 20 points around 1,
  # we expect a data.frame with 0 points
  expect_equal(nrow(create_window(1, 20, 1:50, partial=F)), 0)
  expect_equal(nrow(create_window(9, 20, 1:50, partial=F)), 0)
  expect_equal(nrow(create_window(10, 20, 1:50, partial=F)), 20)
  expect_equal(nrow(create_window(1, 20, 1:50, partial=F, align="left")), 20)
  expect_equal(nrow(create_window(31, 20, 1:50, partial=F, align="left")), 20)
  expect_equal(nrow(create_window(32, 20, 1:50, partial=F, align="left")), 0)
  expect_equal(nrow(create_window(50, 20, 1:50, partial=F, align="right")), 20)
  expect_equal(nrow(create_window(20, 20, 1:50, partial=F, align="right")), 20)
  expect_equal(nrow(create_window(19, 20, 1:50, partial=F, align="right")), 0)
})

test_that("test create_windows", {
  # test parameters
  w.size <- 10
  df.test <- 1:100
  expect_equal(
    nrow(create_windows(df.test, w_size = w.size)),
    length(df.test)*w.size - sum(1:(w.size %/% 2)) - w.size
  )
  expect_equal(
    nrow(create_windows(df.test, w_size = w.size, align="right")),
    length(df.test)*w.size - sum(1:(w.size-1))
  )
  expect_equal(
    nrow(create_windows(df.test, ~idx, w_size = w.size, align="left")),
    length(df.test)*w.size - sum(1:(w.size-1))
  )
})

test_that("test create_windows",{
  # test parameters
  w.size <- 10
  df.test <- 1:100
  expect_equal(ncol(create_windows(df.test, w_size=w.size)), 2)
  expect_type(create_windows(df.test, w_size=w.size), "list")
})

test_that("test nest_windows",{
  # test parameters
  w.size <- 10
  df.test <- 1:100
  expect_equal(ncol(nest_windows(df.test, w_size=w.size)), 2)
  expect_equal(nrow(nest_windows(df.test, w_size=w.size)$data[[1]]), 6)
  expect_equal(nrow(nest_windows(df.test, w_size=w.size)$data[[5]]), 10)
  library(purrr)
  expect_equal(
    sum(
      map_dbl(nest_windows(df.test, w_size=w.size)$data, nrow)
    ),
    length(df.test)*w.size - sum(1:(w.size %/% 2)) - w.size
  )
  expect_equal(
    sum(
      map_dbl(nest_windows(df.test, w_size=w.size, align="right")$data, nrow)
    ),
    length(df.test)*w.size - sum(1:(w.size-1))
  )
  expect_equal(
    sum(
      map_dbl(nest_windows(df.test, w_size=w.size, align="left")$data, nrow)
    ),
    length(df.test)*w.size - sum(1:(w.size-1))
  )
})
