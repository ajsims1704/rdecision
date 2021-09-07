
test_that("illegal initializations are rejected", {
  expect_silent(NormalDistribution$new(0, 1))
  expect_error(NormalDistribution$new("0",1), class="mu_not_numeric")
  expect_error(NormalDistribution$new(0,"1"), class="sigma_not_numeric")
})

test_that("distribution name is correct", {
  sn <- NormalDistribution$new(0, 1)
  expect_identical(sn$distribution(), "N(0,1)")  
  n <- NormalDistribution$new(42, 1)
  expect_identical(n$distribution(), "N(42,1)")  
})

test_that("quantile function checks inputs", {
  x <- NormalDistribution$new(0, 1)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(x$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(x$quantile(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(x$quantile(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(x$quantile(probs), class="probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_length(x$quantile(probs),3)
})

test_that("pe, mean, sd and quantiles are returned correctly", {
  sn <- NormalDistribution$new(0, 1)
  expect_intol(sn$mean(), 0, 0.01)
  expect_intol(sn$SD(), 1, 0.01)
  probs <- c(0.025, 0.975)
  q <- sn$quantile(probs)
  expect_intol(q[1], -1.96, 0.05)
  expect_intol(q[2],  1.96, 0.05)
})

test_that("random sampling is from a Normal distribution", {
  mu <- 0
  sigma <- 1
  sn <- NormalDistribution$new(mu, sigma)
  # mean
  sn$sample(TRUE)
  expect_equal(sn$r(), 0)
  # sample
  n <- 1000
  samp <- sapply(1:n, FUN=function(i) {
    sn$sample()
    rv <- sn$r()
    return(rv)
  })
  expect_length(samp, n)
  # check sample mean and sd are within 99.9% CI based on CLT; this is exact
  # for a normal, and is expected to fail for 0.1% of tests; skip for CRAN
  skip_on_cran()
  ht <- ks.test(samp, rnorm(n,mean=mu,sd=sigma))
  expect_true(ht$p.value > 0.001)
})

