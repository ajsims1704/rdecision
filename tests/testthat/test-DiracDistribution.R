test_that("illegal initializations are rejected", {
  expect_error(DiracDistribution$new("42"), class="const_not_numeric")
})

test_that("it has correct distribution name", {
  lue <- DiracDistribution$new(42.0)
  expect_identical(lue$distribution(), "Const(42)")  
})

test_that("const values are returned", {
  x <- DiracDistribution$new(42.0)
  expect_identical(x$mean(), 42.0)
  expect_identical(x$SD(), 0.0)
  expect_identical(x$mode(), 42.0)
  expect_error(
    x$quantile(probs = c(0.25, NA, 0.75)), class = "probs_not_defined"
  )
  expect_error(
    x$quantile(probs = c(0.25, "A", 0.75)), class = "probs_not_numeric"
  )
  expect_error(x$quantile(
    probs = c(-0.25, 0.75)), class = "probs_out_of_range"
  )
  expect_identical(x$quantile(probs = 0.22), 42.0)
})

test_that("random sampling is correct", {
  x <- DiracDistribution$new(42.0)
  x$sample()
  rv <- x$r()
  expect_identical(rv, 42.0)
  x$sample(TRUE)
  expect_identical(rv, 42.0)
})
