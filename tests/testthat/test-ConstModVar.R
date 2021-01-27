
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
  expect_equal(lue$distribution(), "Const(42)")  
})

test_that("const values are returned", {
  x <- ConstModVar$new("const", "GBP", 42)
  expect_equal(x$mean(),42)
  expect_equal(x$SD(),0)
  expect_equal(x$mode(),42)
  expect_equal(x$quantile(probs=c(0.22)),42)
})

test_that("set and get function as expected", {
  x <- ConstModVar$new("y", "GBP", 42)
  expect_true(is.na(x$get()))
  expect_error(x$set("red"), class="expected_not_logical")
  expect_silent(x$set())
  expect_silent(x$set(TRUE))
  expect_equal(x$get(), 42, tolerance=0.01)
  S <- vector(mode="numeric", length=1000)
  for (i in 1:1000) {
    x$set()
    S[i] <- x$get() 
  }  
  expect_equal(mean(S), 42, tolerance=0.001)
  expect_equal(sd(S), 0, tolerance=0.001)
})
