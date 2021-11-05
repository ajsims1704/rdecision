# tests for class EmpiricalDistribution

test_that("illegal initializations are rejected", {
  x <- c(1,2,'3',4,5)
  expect_error(EmpiricalDistribution$new(x), class="x_not_numeric")
  x <- c(1,2,NA,4,5)
  expect_error(EmpiricalDistribution$new(x), class="x_not_supported")
  x <- c(1,2,3,4,5)
  expect_error(EmpiricalDistribution$new(x), class="x_too_small")
  x <- seq(from=1,to=1000)
  expect_silent(EmpiricalDistribution$new(x))
})

test_that("distribution name is correct", {
  x <- seq(from=1,to=1000)
  e <- EmpiricalDistribution$new(x)
  expect_identical(e$distribution(), "Emp")
})

test_that("mean, mode, sd and quantiles are returned correctly", {
  n3 <- 334
  x <- c(rep(0,n3),rep(0.5,n3),rep(1,n3))
  e <- EmpiricalDistribution$new(x)
  expect_intol(e$mean(), 0.5, 0.01)
  v <- (n3*(-0.5)^2 + n3*0 + n3*(0.5)^2)/(3*n3-1)
  expect_intol(e$SD(), sqrt(v), 0.01)
  expect_true(is.na(e$mode()))
  probs <- c(0.025, 0.5, 0.975)
  q <- e$quantile(probs)
  expect_intol(q[1], 0, 0.01)
  expect_intol(q[2], 0.5, 0.01)
  expect_intol(q[3], 1, 0.01)
})
 
test_that("quantile function checks inputs and has correct output", {
  x <- seq(from=1,to=1000)
  e <- EmpiricalDistribution$new(x)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(e$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(e$quantile(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(e$quantile(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(e$quantile(probs), class="probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_length(e$quantile(probs),3)
})
 
test_that("random sampling from a supplied Gamma distribution", {
  # create the gamma distribution
  k <- 9
  theta <- 0.5
  n <- 1000
  x <- stats::rgamma(n,shape=k,scale=theta)
  # build the empirical distribution
  e <- EmpiricalDistribution$new(x)
  # random sampling
  samp <- sapply(1:n, FUN=function(i) {
    e$sample()
    rv <- e$r()
    return(rv)
  })
  expect_length(samp, n)
  # 99.9% confidence limits; expected test failure rate is 0.1%;
  # skip for CRAN
  skip_on_cran()
  ht <- ks.test(samp, stats::rgamma(n,shape=k,scale=theta))
  expect_true(ht$p.value > 0.001)
})
