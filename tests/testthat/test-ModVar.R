
test_that("illegal arguments are rejected", {
  expect_error(ModVar$new(42, "GBP"), class="description_not_string")
  expect_error(ModVar$new(TRUE, "GBP"), class="description_not_string")
  expect_error(ModVar$new("x", 42), class="units_not_string")
  expect_error(ModVar$new("x", FALSE), class="units_not_string")
})

test_that("ModVar description is saved", {
  # same environment
  x <- ModVar$new("x", "GBP")
  expect_equal(x$description(), "x")
  # child environment
  f <- function(){
    expect_equal(x$description(), "x")
  }
  f()
  # parent environment
  g <- function(){
    y <- ModVar$new("y", "GBP")
    return(y)
  }
  yy <- g()
  expect_equal(yy$description(), "y")
})

test_that("illegal arguments to probs are rejected", {
  x <- ModVar$new("x", "GBP")
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(x$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(x$quantile(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(x$quantile(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(x$quantile(probs), class="probs_out_of_range")
})

test_that("illegal arguments to r are rejected", {
  x <- ModVar$new("x", "GBP")
  expect_silent(x$r(42))  
  expect_error(x$r(NA), class="n_not_defined")
  expect_error(x$r(0), class="n_out_of_range")
})
