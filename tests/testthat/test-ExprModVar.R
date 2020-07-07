
test_that("ExprModVar obeys scoping rules" , {
  # operands in a function environment (test_that)
  x <- 2
  y <- 3
  z <- ExprModVar$new("z", "Z", expr=quote(x+y), envir=environment())
  expect_equal(z$getDistribution(), "x + y")
  expect_equal(z$value(), 5)
  # operands in different function environments
  f = function() {
    y <- 4
    z <- ExprModVar$new("z", "Z", expr=quote(x+y), envir=environment())
    expect_equal(z$value(), 6)
  }
  f()
  # ExprModVar can be passed as an object
  x <- 20
  y <- 30
  z <- ExprModVar$new("z", "Z", expr=quote(x+y), envir=environment())
  g <- function(mv) {
    x <- 200
    y <- 300
    expect_equal(mv$value(),50)
  }
  g(z)
})
