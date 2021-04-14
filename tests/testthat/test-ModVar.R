
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
  expect_identical(x$description(), "x")
  # child environment
  f <- function(){
    expect_identical(x$description(), "x")
  }
  f()
  # parent environment
  g <- function(){
    y <- ModVar$new("y", "GBP")
    return(y)
  }
  yy <- g()
  expect_identical(yy$description(), "y")
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
  expect_length(x$quantile(probs),3)
})

test_that("stub functions return NA", {
  x <- ModVar$new("x", "GBP")
  expect_true(is.na(x$distribution()))
  expect_true(is.na(x$mode()))
  expect_true(is.na(x$SD()))
})

test_that("set checks its argument", {
  x <- ModVar$new("x", "GBP")
  expect_error(x$set(42), class="what_not_character")
  expect_error(x$set(TRUE), class="what_not_character")
  expect_error(x$set("arodnm"), class="what_not_supported")
  expect_error(x$set("value"), class="invalid_val")
  expect_error(x$set("value", "b42"), class="invalid_val")
  expect_silent(x$set("expected"))
  expect_silent(x$set())
  expect_silent(x$set("value", 42))
})

test_that("get is initialized to NA for base class", {
  x <- ModVar$new("x", "GBP")
  expect_true(is.na(x$get()))
})

