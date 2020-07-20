
test_that("illegal initializations are rejected", {
  expect_silent(ConstModVar$new("const", "GBP", 42.5))
  expect_error(ConstModVar$new(42,42,42), class="description_not_string")
  expect_error(ConstModVar$new("const",42,42), class="units_not_string")
  expect_error(ConstModVar$new("const","GBP","42"), class="const_not_numeric")
})

test_that("const values are returned", {
  x <- ConstModVar$new("const", "GBP", 42)
  expect_equal(x$value(),42)
  expect_equal(x$value("pe"),42)
  expect_equal(x$value("mean"),42)
  expect_equal(x$quantile(probs=c(0.22)),42)
})