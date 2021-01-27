
test_that("illegal arguments are rejected", {
  expect_error(ModVar$new(42, "GBP"), class="description_not_string")
  expect_error(ModVar$new(TRUE, "GBP"), class="description_not_string")
  expect_error(ModVar$new("x", 42), class="units_not_string")
  expect_error(ModVar$new("x", FALSE), class="units_not_string")
})

test_that("properties are set correctly", {
  v <- ModVar$new("dummy", "m")
  expect_true(is.na(v$is_probabilistic()))
  expect_false(v$is_expression())
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

test_that("random sampler checks and returns correct types", {
  
})

test_that("stub quantile function checks inputs and has correct output", {
  x <- ModVar$new("x", "GBP")
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(x$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(x$quantile(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(x$quantile(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(x$quantile(probs), class="probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_equal(length(x$quantile(probs)),3)
})

