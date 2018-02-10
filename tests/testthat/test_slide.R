library(slider)
context("Sliding window API")

# load global
library(purrr)
v.test <- 1:100
df.test <- data.frame(x=v.test, y=c("a", "b"))

test_that("test slide_window vector",{
  # test parameters
  w.size <- 10
  # check type and columns
  df <- slide_window(v.test, w_size=w.size)
  expect_equal(ncol(df), 3)
  expect_false(is.data.frame(df$data[[1]]))
  expect_type(df, "list")
  # check nesting
  expect_equal(length(df$data[[1]]), 6)
  expect_equal(length(df$data[[5]]), 10)
  # test for a centered sliding window
  expect_equal(
    sum(map_dbl(df$data, length)),
    length(v.test)*w.size - sum(1:(w.size %/% 2)) - w.size
  )
  # test for a right aligned sliding window
  df <- slide_window(v.test, w_size=w.size, align="right")
  expect_equal(
    sum(map_dbl(df$data, length)),
    length(v.test)*w.size - sum(1:(w.size-1))
  )
  # test for a left aligned sliding window
  df <- slide_window(v.test, w_size=w.size, align="left")
  expect_equal(
    sum(map_dbl(df$data, length)),
    length(v.test)*w.size - sum(1:(w.size-1))
  )
})

test_that("test slide_window data.frame", {
  # window size
  w.size <- 10
  # test data.frame with string
  df <- slide_window(df.test, key="x", w_size=w.size)
  expect_equal(ncol(df), 3)
  expect_true(is.data.frame(df$data[[1]]))
  expect_equal(ncol(df$data[[1]]), 2)

  # test data.frame with
  df <- slide_window(df.test, key=x, w_size=w.size)
  expect_equal(ncol(df), 3)
  expect_true(is.data.frame(df$data[[1]]))
  expect_equal(ncol(df$data[[1]]), 2)
})

test_that("test slide_windows vector", {
  # window sizes
  w.sizes <- c(5, 10, 15, 20)
  # create windows
  df <- slide_windows(v.test, w_sizes=w.sizes, partial=F)
  expect_equal(unique(df$w_size), w.sizes)
})

test_that("test slide_windows data.frame", {
  # window sizes
  w.sizes <- c(5, 10, 15, 20)
  # create windows
  df <- slide_windows(df.test, key="x", w_sizes=w.sizes, partial=F)
  expect_equal(unique(df$w_size), w.sizes)
})

test_that("test apply_slide_window vector", {
  # window size
  w.size <- 10

  df <- apply_slide_window(
    v.test, mean, w_size=w.size, partial=F
  )

  expect_true(".out" %in% colnames(df))

  df <- apply_slide_window(
    v.test, mean, w_size=w.size, partial=F, .to="bla"
  )
  expect_true("bla" %in% colnames(df))

  df <- apply_slide_window(
    v.test, mean, w_size=w.size, partial=F, .keep_data = F
  )

  expect_false("data" %in% colnames(df))

})

test_that("test apply_slide_window data.frame", {
  w.sizes <- c(5, 10 ,15, 20)

  df <- apply_slide_windows(
    df.test, ~mean(.$x), key="x", w_sizes=w.sizes, partial=F
  )

  expect_true(".out" %in% colnames(df))
  expect_equal(unique(df$w_size), w.sizes)
  expect_equal(mean(df$data[[1]]$x), df$.out[[1]])

})



## DO NOT RUN IN TEST
if (FALSE) {
  # install.packages("microbenchmark")
  library(microbenchmark)
  library(tidyr)
  library(zoo)
  # simple mean calculation using a vector
  w.size <- 10
  df.test <- 1:500
  microbenchmark(
    unlist(map(create_windows(df.test, w_size=w.size, partial=F)$data, mean)),
    rollapply(df.test, w.size, (mean)),
    rollmean(df.test, w.size)
  )
  # linear model fitting
  # w.size <- 20
  # df.test <- beaver1 %>% mutate(time_h = time %/% 100 + 24*(day - day[1]) + (time %% 100)/60)
  # microbenchmark(
  #   create_windows(df.test, key="time_h", w_size=w.size, partial=F) %>%
  #     mutate(model = map(data, ~lm(temp ~ time_h, data=.))),
  #   mmpr::roll_window
  # )
}
