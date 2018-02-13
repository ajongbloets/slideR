library(slider)
context("slideR utilities")

d <- data.frame(v1=1:2, v2=9:8)

Dtest <- function(d, x="v1", y="z"){
  result <- d

  x <- enquo(x) %>% resolve_quosure()
  y <- enquo(y) %>% resolve_quosure()

  if (!is.null(x) && !is.null(y)) {
    result <- d %>% mutate(!!quo_name(y) := UQ(x))
  }

  result
}

test_that("test resolve.quosure with strings",{
  df.r <- Dtest(d, "v1", "z")
  expect_equal(colnames(df.r), c("v1", "v2", "z") )
  expect_equal(df.r$v1, df.r$z)
  df.r <- Dtest(d, "v1", "f")
  expect_equal(colnames(df.r), c("v1", "v2", "f") )
  expect_equal(df.r$v1, df.r$f)
  df.r <- Dtest(d, "v2", "f")
  expect_equal(colnames(df.r), c("v1", "v2", "f") )
  expect_equal(df.r$v2, df.r$f)
})

test_that("test resolve.quosure with x being a variable", {
  df.r <- Dtest(d, v1)
  expect_equal(colnames(df.r), c("v1", "v2", "z") )
  expect_equal(df.r$v1, df.r$z)
  df.r <- Dtest(d, v2)
  expect_equal(colnames(df.r), c("v1", "v2", "z") )
  expect_equal(df.r$v2, df.r$z)
})

test_that("test resolve.quosure with x and y being a variable", {
  df.r <- Dtest(d, v1, z)
  expect_equal(colnames(df.r), c("v1", "v2", "z") )
  expect_equal(df.r$v1, df.r$z)
  df.r <- Dtest(d, v2, z)
  expect_equal(colnames(df.r), c("v1", "v2", "z") )
  expect_equal(df.r$v2, df.r$z)
})

test_that("test resolve_quosure with NULL", {

  df.r <- Dtest(d, NULL, NULL)
  expect_equal(colnames(df.r), c("v1", "v2"))

})
