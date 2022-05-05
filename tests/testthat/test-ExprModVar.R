
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
  # deeper nesting
  e4 <- ExprModVar$new("e4", "", quo=rlang::quo(2*e3))
  mv <- e4$operands()
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_setequal(d, c("e1", "e2", "e3", "y", "z"))
  # nesting without recursion
  mv <- e3$operands(recursive=FALSE)
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_setequal(d, c("e1", "e2"))
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
  # skip remaining tests on CRAN because they rely on sampling
  skip_on_cran()
  # check quantile, estimated from empirical distribution
  z$set("q97.5")
  v <- z$get()
  expect_true(v>3.5)
  # check mean is within 3 std errors
  z$set("expected")
  se <- 2/sqrt(1000)
  expect_intol(z$get(),0,3*se)
  # check that set() for operands affects get() for the expression
  y$set("q97.5")
  z$set("current")
  expect_true(z$get()>3.5)
  y$set("expected")
  expect_intol(z$get(), 0, 3*se)
  # check that a random sample from z is from a SN*2, despite setting y 
  n <- 1000
  S <- vector(mode="numeric", length=n)
  for (i in 1:n) {
    y$set("q97.5")
    z$set("random")
    S[i] <- z$get() 
  } 
  # 99.9% confidence limits; expected 0.1% test failure rate; skip for CRAN
  ht <- ks.test(S, rnorm(n,mean=0,sd=2))
  expect_true(ht$p.value>0.001)
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
  # check that random sampling is supported
  q$set("random")
  rbeta <- q$get()
  expect_true((rbeta>=0) && (rbeta <= 1))
  # check internally added methods; 99.9% confidence limits assuming CLT; expect
  # 0.1% test failure rate; skip for CRAN
  n <- 1000
  samp <- sapply(1:n, FUN=function(i) {
    q$set("random")
    rv <- q$get()
    return(rv)
  })
  expect_length(samp, n)
  skip_on_cran()
  ht <- ks.test(samp, rbeta(n,shape1=beta,shape2=alpha))
  expect_true(ht$p.value > 0.001)
})

test_that("illegal sample sizes for estimating parameters are rejected", {
  x <- 3
  y <- NormModVar$new("y", "GBP", mu=0, sigma=1)
  z <- ExprModVar$new("z", "GBP", quo=rlang::quo(x*y))
  expect_error(
    ExprModVar$new("z", "GBP", quo=rlang::quo(x*y), "100"), 
    class="nemp_not_numeric"
  )
  expect_error(
    ExprModVar$new("z", "GBP", quo=rlang::quo(x*y), 100),
    class="nemp_too_small"
  )
  expect_error(
    ExprModVar$new("z", "GBP", quo=rlang::quo(x*y), 999.5), 
    class="nemp_too_small"
  )
  expect_silent(
    ExprModVar$new("z", "GBP", quo=rlang::quo(x*y))
  )
  expect_silent(
    ExprModVar$new("z", "GBP", quo=rlang::quo(x*y), 10000)
  )
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
})

test_that("nested expressions are evaluated correctly", {
  # skip test on cran because it involves sampling
  skip_on_cran()
  # standard normal and an expression with an identity operator
  sn1 <- NormModVar$new("sn1", "", mu=0, sigma=1)
  s1 <- ExprModVar$new("s1", "", rlang::quo(1*sn1))
  # standard error
  se <- sqrt(2)/sqrt(1000)
  # check that all 3 products have a mean of 1 (chisq with 1 dof)   
  p1 <- ExprModVar$new("p1", "", rlang::quo(sn1*sn1))
  expect_intol(p1$mu_hat(), 1, 3*se)
  p2 <- ExprModVar$new("p2", "", rlang::quo(s1*s1))
  expect_intol(p2$mu_hat(), 1, 3*se)
  p3 <- ExprModVar$new("p3", "", rlang::quo(s1*sn1))
  expect_intol(p3$mu_hat(), 1, 3*se)
})

test_that("autocorrelation in nested expressions is preserved", {
  # skip test on cran because it involves sampling
  skip_on_cran()
  # create expressions to wrap standard normals
  sn1 <- NormModVar$new("sn1", "", mu=0, sigma=1)
  s1 <- ExprModVar$new("s1", "", rlang::quo(1*sn1))
  sn2 <- NormModVar$new("sn2", "", mu=0, sigma=1)
  s2 <- ExprModVar$new("s2", "", rlang::quo(1*sn2))
  x <- ExprModVar$new("x", "", rlang::quo(1*s1))
  y <- ExprModVar$new("y", "", rlang::quo(1*s2))
  # create nested correlated and nested uncorrelated expressions
  zc <- ExprModVar$new("zc", "", rlang::quo(x*s1))
  zu <- ExprModVar$new("zu", "", rlang::quo(y*s1))
  # compute approx standard error of the mean, for tolerance
  se <- sqrt(2)/sqrt(1000)
  # zc is a chi-squared with 1 dof
  expect_intol(zc$mu_hat(), 1, 3*se)
  # zu is a modified Bessel function with mean 0 and sd 1
  expect_intol(zu$mu_hat(), 0, 3*se)
})

test_that("expression chi square from SN is correct", {
  # skip on cran because tests involve sampling
  skip_on_cran()
  # x ~ N(0,1), y = x^2 ~ Chisq(k=1)
  k <- 1
  x <- NormModVar$new("x", "", mu=0, sigma=1)
  y <- ExprModVar$new("y","",rlang::quo(x^2))
  # standard error
  se <- sqrt(2*k)/sqrt(1000)
  expect_equal(y$mean(), 0)          # product of operand means is 0
  expect_intol(y$mu_hat(), k, 3*se)  # true mean is k=1
  expect_true(is.na(y$mode()))  # mode is undefined for ExprModVar
  median <- k*(1-2/(9*k))^3
  expect_intol(y$q_hat(p=0.5), median, 3*se)
  # generate a distribution and check it
  n <- 1000
  samp <- sapply(1:n, FUN=function(i) {
    y$set("random")
    rv <- y$get()
    return(rv)
  })
  ht <- ks.test(samp, rchisq(n, df=1))
  expect_true(ht$p.value>0.001)
})

test_that("one Dirichlet matches a Beta and an expression", {
  # skip on cran because tests inovolve sampling
  skip_on_cran()
  # p follows Beta(1,9) and q is 1-p
  alpha <- 1
  beta <- 9
  p <- BetaModVar$new("P(success)", "P", alpha=alpha, beta=beta)
  q <- ExprModVar$new("P(failure)", "P", rlang::quo(1-p))
  # p and q are both derived from Dir(1,9) distribution
  D <- DirichletDistribution$new(alpha=c(1,9))
  p.d <- ModVar$new("P(success)", "P", D=D, k=as.integer(1))
  q.d <- ModVar$new("P(failure)", "P", D=D, k=as.integer(2))
  # approx standard error
  sd <- sqrt((alpha*beta)/((alpha+beta)^2 * (alpha+beta+1)))
  se <- sd / sqrt(1000)
  # compare means
  expect_equal(p$mean(), p.d$mean())
  expect_intol(q$mean(), q.d$mean(), 3*se)
  # compare quantiles for p 
  probs<- c(0.025, 0.975)
  expect_setequal(unname(p$quantile(probs)), unname(p.d$quantile(probs)))
  # quantiles defined for q.d but not q 
  expect_true(all(is.na(q$quantile(probs))))
  expect_true(all(!is.na(q.d$quantile(probs))))
})
