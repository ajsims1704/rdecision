test_that("illegal initializations are rejected", {
  expect_error(DiracDistribution$new("42"), class="const_not_numeric")
})

test_that("it has correct distribution name", {
  lue <- DiracDistribution$new(42)
  expect_identical(lue$distribution(), "Const(42)")  
})

test_that("const values are returned", {
  x <- DiracDistribution$new(42)
  expect_equal(x$mean(),42)
  expect_equal(x$SD(),0)
  expect_equal(x$mode(),42)
  expect_error(x$quantile(probs=c(0.25,NA,0.75)), class="probs_not_defined")
  expect_error(x$quantile(probs=c(0.25,"A",0.75)), class="probs_not_numeric")
  expect_error(x$quantile(probs=c(-0.25,0.75)), class="probs_out_of_range")
  expect_equal(x$quantile(probs=c(0.22)),42)
})

test_that("random sampling is correct", {
  x <- DiracDistribution$new(42)
  x$sample()
  rv <- x$r()
  expect_equal(rv, 42)
  x$sample(TRUE)
  expect_equal(rv, 42)
})
