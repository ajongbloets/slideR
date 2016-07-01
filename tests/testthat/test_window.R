library(slider)
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
  # if we make a centered window of 10 points around 1 and it may be partial,
  # we expect 5 points, we provide a vector, so expect a vector
  expect_equal(length(create_window(1, 10, 1:50)), 6)
  # if we make a centered window of 10 points around 5 and it may be partial,
  # we expect 9 points
  expect_equal(length(create_window(5, 10, 1:50)), 10)
  # if we make a centered window of 10 points around 6 and it may be partial,
  # we expect 10 points
  expect_equal(length(create_window(6, 10, 1:50)), 10)
  # if we make a centered window of 20 points around 1 and it may be partial,
  # we expect 10 points
  expect_equal(length(create_window(1, 20, 1:50)), 11)
  # if we make a centered window of 20 points around 1 and do not allow partial,
  # we expect a vector with 0 points
  expect_equal(length(create_window(1, 20, 1:50, partial=F)), 0)
  expect_equal(length(create_window(9, 20, 1:50, partial=F)), 0)
  expect_equal(length(create_window(10, 20, 1:50, partial=F)), 20)
  expect_equal(length(create_window(1, 20, 1:50, partial=F, align="left")), 20)
  expect_equal(length(create_window(31, 20, 1:50, partial=F, align="left")), 20)
  expect_equal(length(create_window(32, 20, 1:50, partial=F, align="left")), 0)
  expect_equal(length(create_window(50, 20, 1:50, partial=F, align="right")), 20)
  expect_equal(length(create_window(20, 20, 1:50, partial=F, align="right")), 20)
  expect_equal(length(create_window(19, 20, 1:50, partial=F, align="right")), 0)
})

test_that("test create_windows",{
  # test parameters
  w.size <- 10
  df.test <- 1:100
  # check type and columns
  expect_equal(ncol(create_windows(df.test, w_size=w.size)), 3)
  expect_type(create_windows(df.test, w_size=w.size), "list")
  # check nesting
  expect_equal(length(create_windows(df.test, w_size=w.size)$data[[1]]), 6)
  expect_equal(length(create_windows(df.test, w_size=w.size)$data[[5]]), 10)
  library(purrr)
  df.windows <- create_windows(df.test, w_size=w.size)
  # test for a centered sliding window
  expect_equal(
    sum(map_dbl(df.windows$data, length)),
    length(df.test)*w.size - sum(1:(w.size %/% 2)) - w.size
  )
  # test for a right aligned sliding window
  df.windows <- create_windows(df.test, w_size=w.size, align="right")
  expect_equal(
    sum(map_dbl(df.windows$data, length)),
    length(df.test)*w.size - sum(1:(w.size-1))
  )
  # test for a left aligned sliding window
  df.windows <- create_windows(df.test, w_size=w.size, align="left")
  expect_equal(
    sum(map_dbl(df.windows$data, length)),
    length(df.test)*w.size - sum(1:(w.size-1))
  )

})
