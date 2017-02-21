library(slider)
context("Partition API")

test_that("test window_bounds", {
  expect_equal(window_boundaries(1, 10), c(-3, 6))
  expect_equal(window_boundaries(5, 10), c(1, 10))
  expect_equal(window_boundaries(5, 10, align="center"), c(1, 10))
  expect_equal(window_boundaries(5, 10, align="right"), c(-4, 5))
  expect_equal(window_boundaries(5, 10, align="left"), c(5, 14))
  expect_equal(window_boundaries(5, 10, align="Left"), c(NA, NA))
  expect_equal(window_boundaries(5, 10, align=""), c(NA, NA))
})

test_that("test partition_window", {
  # if we make a centered window of 10 points around 1 and it may be partial,
  # we expect 5 points, we provide a vector, so expect a vector
  df <- partition_window(1, 10, 1:50)
  expect_equal(length(df), 6)
  # if we make a centered window of 10 points around 5 and it may be partial,
  # we expect 9 points
  df <- partition_window(5, 10, 1:50)
  expect_equal(length(df), 10)
  # if we make a centered window of 10 points around 6 and it may be partial,
  # we expect 10 points
  df <- partition_window(6, 10, 1:50)
  expect_equal(length(df), 10)
  # if we make a centered window of 20 points around 1 and it may be partial,
  # we expect 10 points
  df <- partition_window(1, 20, 1:50)
  expect_equal(length(df), 11)
  # if we make a centered window of 20 points around 1 and do not allow partial,
  # we expect a vector with 0 points
  df <- partition_window(1, 20, 1:50, partial=F)
  expect_equal(length(df), 0)
  df <- partition_window(9, 20, 1:50, partial=F)
  expect_equal(length(df), 0)
  df <- partition_window(10, 20, 1:50, partial=F)
  expect_equal(length(df), 20)
  df <- partition_window(1, 20, 1:50, partial=F, align="left")
  expect_equal(length(df), 20)
  df <- partition_window(31, 20, 1:50, partial=F, align="left")
  expect_equal(length(df), 20)
  df <- partition_window(32, 20, 1:50, partial=F, align="left")
  expect_equal(length(df), 0)
  df <- partition_window(50, 20, 1:50, partial=F, align="right")
  expect_equal(length(df), 20)
  df <- partition_window(20, 20, 1:50, partial=F, align="right")
  expect_equal(length(df), 20)
  df <- partition_window(19, 20, 1:50, partial=F, align="right")
  expect_equal(length(df), 0)
})
