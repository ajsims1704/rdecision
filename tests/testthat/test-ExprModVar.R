
test_that("ExprModVars can include simple expressions" , {
  x <- 2
  y <- 3
  z <- ExprModVar$new(description="z", units="Z", expr=rlang::quo(x+y), envir=environment())
  expect_equal(z$getDistribution(), "x + y")
  expect_equal(z$value(), 5)
})
