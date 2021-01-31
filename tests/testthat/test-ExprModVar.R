
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

test_that("operands are identified correctly", {
  # simple case
  x <- 2
  y <- NormModVar$new("y", "GBP", mu=0, sigma=1)
  z <- ConstModVar$new("z", "GPB", 42)
  e <- ExprModVar$new("e", "GBP", quo=rlang::quo(x*y + z))
  mv <- e$operands()
  expect_equal(length(mv), 2)  # y and z
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_equal(d[order(d)], c("y", "z"))
  # nested case, with repeats
  e1 <- ExprModVar$new("e1", "GBP", quo=rlang::quo(x*y + z))
  e2 <- ExprModVar$new("e2", "GBP", quo=rlang::quo(z+3))
  e3 <- ExprModVar$new("e3", "GBP", quo=rlang::quo(e1+e2))
  mv <- e3$operands()
  expect_equal(length(mv), 4)  # y, z, e1, e2
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_equal(d[order(d)], c("e1", "e2", "y", "z"))
})

test_that("set and get function as expected", {
  # check initialization
  x <- 2
  y <- NormModVar$new("y", "GBP", mu=0, sigma=1)
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x*y))
  expect_false(is.na(z$get()))
  # check illegal input
  expect_error(z$set("red"), class="expected_not_logical")
  # check that set() is ignored for ExprModVar
  expect_silent(z$set())
  expect_equal(z$get(),0)
  # check that set() for operands affects get() for the expression
  y$set(TRUE)
  expect_equal(z$get(), 0, tolerance=0.01)
  S <- vector(mode="numeric", length=1000)
  for (i in 1:1000) {
    y$set()
    S[i] <- z$get() 
  } 
  expect_equal(mean(S), 0, tolerance=0.2)
  expect_equal(sd(S), 2, tolerance=0.2)
})

test_that("illegal sample sizes for estimating parameters are rejected", {
  x <- 3
  y <- NormModVar$new("y", "GBP", mu=0, sigma=1)
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x*y))
  expect_error(z$mu_hat("100"), class="nest_not_numeric")
  expect_error(z$mu_hat(3), class="nest_too_small")
  expect_error(z$mu_hat(999.5), class="nest_too_small")
  expect_silent(z$mu_hat())
  expect_silent(z$mu_hat(10000))
  expect_error(z$sigma_hat("100"), class="nest_not_numeric")
  expect_error(z$sigma_hat(3), class="nest_too_small")
  expect_error(z$sigma_hat(999.5), class="nest_too_small")
  expect_silent(z$sigma_hat(10000))
})

test_that("scoping rules for mu_hat in nested expressions are obeyed", {
  x <- NormModVar$new("SN", "m", mu=0, sigma=1)
  y <- ExprModVar$new("SN2","m", rlang::quo(2*x))
  b <- ExprModVar$new("z","m^2",rlang::quo(x*y))
  b$mu_hat()
  expect_silent(b$mu_hat())
})

test_that("expression chi square from SN is correct", {
  # x = N(0,1), y = x^2 = Chisq(k=1)
  x <- NormModVar$new("SN", "m", mu=0, sigma=1)
  y <- ExprModVar$new("z","m^2",rlang::quo(x^2))
  expect_equal(y$mean(), 0)  # true mean is k=1, expression at mean inputs is 0
  expect_equal(y$mu_hat(), 1, tolerance=0.2)  # true mean is k=1
  expect_true(is.na(y$mode()))  # mode is undefined for ExprModVar
  expect_true(is.na(y$SD()))  # SD is undefined for ExprModVar
  expect_equal(y$sigma_hat(), sqrt(2), tolerance=0.2) # variance is 2k
})

