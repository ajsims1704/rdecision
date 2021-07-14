
test_that("illegal initializations are rejected", {
  expect_silent(ConstModVar$new("const", "GBP", 42.5))
  expect_error(ConstModVar$new(42,42,42), class="description_not_string")
  expect_error(ConstModVar$new("const",42,42), class="units_not_string")
  expect_error(ConstModVar$new("const","GBP","42"), class="const_not_numeric")
})

test_that("properties are correct", {
  lue <- ConstModVar$new("lue", "GBP", 42)
  expect_false(lue$is_expression())
  expect_false(lue$is_probabilistic())
})

test_that("it has correct distribution name", {
  lue <- ConstModVar$new("lue", "GBP", 42)
  expect_identical(lue$distribution(), "Const(42)")  
})

test_that("const values are returned", {
  x <- ConstModVar$new("const", "GBP", 42)
  expect_equal(x$mean(),42)
  expect_equal(x$SD(),0)
  expect_equal(x$mode(),42)
  expect_error(x$quantile(probs=c(0.25,NA,0.75)), class="probs_not_defined")
  expect_error(x$quantile(probs=c(0.25,"A",0.75)), class="probs_not_numeric")
  expect_error(x$quantile(probs=c(-0.25,0.75)), class="probs_out_of_range")
  expect_equal(x$quantile(probs=c(0.22)),42)
})

test_that("set and get function as expected", {
  x <- ConstModVar$new("y", "GBP", 42)
  expect_intol(x$get(), 42, 0.01)
  expect_error(x$set(TRUE), class="what_not_character")
  expect_error(x$set("red"), class="what_not_supported")
  expect_silent(x$set())
  expect_silent(x$set("expected"))
  expect_intol(x$get(), 42, 0.01)
  S <- vector(mode="numeric", length=1000)
  for (i in 1:1000) {
    x$set()
    S[i] <- x$get() 
  }  
  expect_intol(mean(S), 42, 0.1)
  expect_intol(sd(S), 0, 0.01)
})

test_that("set('value') works as expected", {
  x <- ConstModVar$new("x", "GBP", 42)
  expect_equal(x$get(), 42)
  x$set("value", 7)
  expect_equal(x$get(), 7)
  expect_equal(x$mean(), 7)
})

