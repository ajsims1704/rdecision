
test_that("ExprModVar obeys scoping rules" , {
  # operands in a function environment (test_that)
  x <- 2
  y <- 3
  z <- ExprModVar$new("z", "Z", quo=rlang::quo(x+y))
  expect_equal(z$distribution(), "x + y")
  expect_equal(z$value(), 5)
  # operands in different function environments
  f = function() {
    y <- 4
    z <- ExprModVar$new("z", "Z", quo=rlang::quo(x+y))
    expect_equal(z$value(), 6)
  }
  f()
  # ExprModVar can be passed as an object
  x <- 20
  y <- 30
  z <- ExprModVar$new("z", "Z", quo=rlang::quo(x+y))
  g <- function(mv) {
    x <- 200
    y <- 300
    expect_equal(mv$value(),50)
  }
  g(z)
})

test_that("Point estimates are preserved in expressions", {
  x <- NormModVar$new("SN", "m", mu=0, sigma=1)
  y <- ExprModVar$new("z","m^2",rlang::quo(x^2))
  expect_equal(y$value(),0)
  expect_equal(y$mean(),0)
})
