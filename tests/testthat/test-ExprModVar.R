
test_that("properties are set correctly", {
  x <- 2
  y <- 3
  expect_error(ExprModVar$new("z", "GBP", x+y), class = "quo_not_quosure")
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x+y))
  expect_true(z$is_expression())
  expect_identical(z$distribution(), "x + y")
  expect_false(z$is_probabilistic())
  #
  y <- ConstModVar$new("y", "GBP", 42)
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x+y))
  expect_true(z$is_expression())
  expect_identical(z$distribution(), "x + y")
  expect_false(z$is_probabilistic())
  #
  y <- NormModVar$new("y", "GBP", mu=0, sigma=1)
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x+y))
  expect_true(z$is_expression())
  expect_identical(z$distribution(), "x + y")
  expect_true(z$is_probabilistic())
})

test_that("ExprModVar obeys scoping rules" , {
  # operands in a function environment (test_that)
  x <- 2
  y <- 3
  z <- ExprModVar$new("z", "Z", quo=rlang::quo(x+y))
  expect_identical(z$distribution(), "x + y")
  expect_intol(z$mean(), 5, 0.1)
  # operands in different function environments
  f = function() {
    y <- 4
    z <- ExprModVar$new("z", "Z", quo=rlang::quo(x+y))
    expect_intol(z$mean(), 6, 0.1)
  }
  f()
  # ExprModVar can be passed as an object
  x <- 20
  y <- 30
  z <- ExprModVar$new("z", "Z", quo=rlang::quo(x+y))
  g <- function(mv) {
    x <- 200
    y <- 300
    expect_intol(mv$mean(),50,1)
  }
  g(z)
})

test_that("stub quantile function checks inputs and has correct output", {
  x <- 2
  y <- 3
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x+y))
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(z$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(z$quantile(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(z$quantile(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(z$quantile(probs), class="probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_length(z$quantile(probs),3)
})

test_that("operands are identified correctly", {
  # simple case
  x <- 2
  y <- NormModVar$new("y", "GBP", mu=0, sigma=1)
  z <- ConstModVar$new("z", "GPB", 42)
  e <- ExprModVar$new("e", "GBP", quo=rlang::quo(x*y + z))
  mv <- e$operands()
  expect_length(mv, 2)  # y and z
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_setequal(d, c("y", "z"))
  # nested case, with repeats
  e1 <- ExprModVar$new("e1", "GBP", quo=rlang::quo(x*y + z))
  e2 <- ExprModVar$new("e2", "GBP", quo=rlang::quo(z+3))
  e3 <- ExprModVar$new("e3", "GBP", quo=rlang::quo(e1+e2))
  mv <- e3$operands()
  expect_length(mv, 4)  # y, z, e1, e2
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_setequal(d, c("e1", "e2", "y", "z"))
})

test_that("set and get function as expected", {
  # check initialization
  x <- 2
  y <- NormModVar$new("y", "GBP", mu=0, sigma=1)
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x*y))
  expect_false(is.na(z$get()))
  # check illegal input
  expect_error(z$set(TRUE), class="what_not_character")
  expect_error(z$set("red"), class="what_not_supported")
  # check that set() is ignored for ExprModVar
  expect_silent(z$set())
  expect_equal(z$get(),0)
  # check that set() for operands affects get() for the expression
  y$set("expected")
  expect_intol(z$get(), 0, 0.01)
  n <- 1000
  S <- vector(mode="numeric", length=n)
  for (i in 1:n) {
    y$set()
    S[i] <- z$get() 
  } 
  # 99.9% confidence limits; expected 0.1% test failure rate; skip for CRAN
  skip_on_cran()
  expect_samplemean(S, mu=0, sigma=2)
  expect_sampleSD(S, sigma=2)
})

test_that("modified expressions are created correctly", {
  alpha <- 1
  beta <- 9
  p <- BetaModVar$new("P(success)", "P", alpha=alpha, beta=beta)
  q <- ExprModVar$new("P(failure)", "P", rlang::quo(1-p))
  # check externally added method
  expect_error(q$add_method(42), class="method_not_character")
  q.mean <- q$add_method("mean()")
  expect_intol(
    eval(rlang::quo_get_expr(q.mean), envir=rlang::quo_get_env(q.mean)),
    0.9,
    0.05
  )
  expect_intol(q$mean(), 0.9, 0.05)
  # check that pre-prepared r() method is present
  rbeta <- q$r(1)
  expect_true((rbeta>=0) && (rbeta <= 1))
  # check internally added methods; 99.9% confidence limits assuming CLT; expect
  # 0.1% test failure rate; skip for CRAN
  n <- 1000
  samp <- q$r(n)
  expect_length(samp, n)
  skip_on_cran()
  mu <- 1-(alpha / (alpha+beta))
  sigma <- sqrt(alpha*beta / ((alpha+beta)^2 * (alpha+beta+1)))
  expect_samplemean(samp, mu, sigma)
  expect_sampleSD(samp, sigma)
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

test_that("quantile estimation checks inputs and has correct output", {
  p <- BetaModVar$new("P(success)", "P", alpha=1, beta=9)
  q <- ExprModVar$new("P(failure)", "P", rlang::quo(1-p))
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(q$q_hat(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(q$q_hat(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(q$q_hat(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(q$q_hat(probs), class="probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_equal(length(q$q_hat(probs)),3)
  expect_error(q$q_hat(probs,"1000"), class="nest_not_numeric")
  expect_error(q$q_hat(probs,42), class="nest_too_small")
})

test_that("scoping rules for mu_hat in nested expressions are obeyed", {
  x <- NormModVar$new("SN", "m", mu=0, sigma=1)
  y <- ExprModVar$new("SN2","m", rlang::quo(2*x))
  b <- ExprModVar$new("z","m^2",rlang::quo(x*y))
  expect_silent(b$mu_hat())
})

test_that("expression chi square from SN is correct", {
  # x = N(0,1), y = x^2 = Chisq(k=1)
  x <- NormModVar$new("SN", "m", mu=0, sigma=1)
  y <- ExprModVar$new("z","m^2",rlang::quo(x^2))
  expect_equal(y$mean(), 0)  # true mean is k=1, expression at mean inputs is 0
  expect_true(is.na(y$mode()))  # mode is undefined for ExprModVar
  expect_true(is.na(y$SD()))  # SD is undefined for ExprModVar
  skip_on_cran()
  mu <- 1 # true mean is k=1
  sigma <- sqrt(2) # variance is 2k
  samp <- y$r(1000)
  expect_samplemean(samp, mu, sigma)
  expect_sampleSD(samp, sigma)
  samp <- rchisq(n=1000, df=1)
  expect_samplemean(samp, mu, sigma)
  expect_sampleSD(samp, sigma)
})

test_that("skip on cran", {
  skip_on_cran()
  expect_true(FALSE)
})

