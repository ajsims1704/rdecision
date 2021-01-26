
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