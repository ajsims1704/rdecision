
test_that("properties are set correctly", {
  x <- 2
  y <- 3
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x+y))
  expect_true(z$is_expression())
  expect_equal(z$distribution(), "x + y")
  expect_false(z$is_probabilistic())
  #
  y <- ConstModVar$new("y", "GBP", 42)
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x+y))
  expect_true(z$is_expression())
  expect_equal(z$distribution(), "x + y")
  expect_false(z$is_probabilistic())
  #
  y <- NormModVar$new("y", "GBP", mu=0, sigma=1)
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x+y))
  expect_true(z$is_expression())
  expect_equal(z$distribution(), "x + y")
  expect_true(z$is_probabilistic())
})

test_that("illegal sample sizes for estimating parameters are rejected", {
  x <- 3
  y <- NormModVar$new("y", "GBP", mu=0, sigma=1)
  expect_error(ExprModVar$new("z", "GBP", quo=rlang::quo(x*y), "100"), 
               class="n_not_numeric")
  expect_error(ExprModVar$new("z", "GBP", quo=rlang::quo(x*y), 3), 
               class="n_too_small")
  expect_error(ExprModVar$new("z", "GBP", quo=rlang::quo(x*y), 999.5), 
               class="n_too_small")
  expect_silent(ExprModVar$new("z", "GBP", quo=rlang::quo(x*y), 10000))
})

test_that("ExprModVar obeys scoping rules" , {
  # operands in a function environment (test_that)
  x <- 2
  y <- 3
  z <- ExprModVar$new("z", "Z", quo=rlang::quo(x+y))
  expect_equal(z$distribution(), "x + y")
  expect_equal(z$mean(), 5)
  # operands in different function environments
  f = function() {
    y <- 4
    z <- ExprModVar$new("z", "Z", quo=rlang::quo(x+y))
    expect_equal(z$mean(), 6)
  }
  f()
  # ExprModVar can be passed as an object
  x <- 20
  y <- 30
  z <- ExprModVar$new("z", "Z", quo=rlang::quo(x+y))
  g <- function(mv) {
    x <- 200
    y <- 300
    expect_equal(mv$mean(),50)
  }
  g(z)
})

test_that("set and get function as expected", {
  x <- 2
  y <- NormModVar$new("y", "GBP", mu=0, sigma=1)
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x*y))
  expect_true(is.na(z$get()))
  expect_error(z$set("red"), class="expected_not_logical")
  expect_silent(z$set())
  expect_silent(z$set(TRUE))
  expect_equal(z$get(), 0, tolerance=0.15)
  S <- vector(mode="numeric", length=1000)
  for (i in 1:1000) {
    z$set()
    S[i] <- z$get() 
  }  
  expect_equal(mean(S), 0, tolerance=0.1)
  expect_equal(sd(S), 2, tolerance=0.1)
})

test_that("expression chi square from SN is correct", {
  # x = N(0,1), y = x^2 = Chisq(k=1)
  x <- NormModVar$new("SN", "m", mu=0, sigma=1)
  y <- ExprModVar$new("z","m^2",rlang::quo(x^2))
  expect_equal(y$mean(),1, tolerance=0.2)  # mean is k
  expect_true(is.na(y$mode()))  # mode is undefined for ExprModVar
  expect_equal(y$SD(), sqrt(2), tolerance=0.2) # variance is 2k
})

