# tests for class EmpiricalDistribution

test_that("illegal initializations are rejected", {
  x <- c(1.0, 2.0, "3", 4.0, 5.0)
  expect_error(EmpiricalDistribution$new(x), class = "x_not_numeric")
  x <- c(1.0, 2.0, NA_real_, 4.0, 5.0)
  expect_error(EmpiricalDistribution$new(x), class = "x_not_supported")
  x <- vector(mode = "numeric", length = 0L)
  expect_error(EmpiricalDistribution$new(x), class = "x_too_small")
  x <- 42L
  expect_silent(EmpiricalDistribution$new(x))
  expect_error(
    EmpiricalDistribution$new(x, 42L), 
    class = "interpolate.sample_not_supported"
  )
  x <- seq(from = 1.0, to = 1000.0)
  expect_silent(EmpiricalDistribution$new(x))
})

test_that("distribution name is correct", {
  x <- seq(from = 1.0, to = 1000.0)
  e <- EmpiricalDistribution$new(x)
  expect_identical(e$distribution(), "Emp")
})

test_that("mean, mode, sd and quantiles are returned correctly", {
  n3 <- 334L
  x <- c(rep(0.0, n3), rep(0.5, n3), rep(1.0, n3))
  e <- EmpiricalDistribution$new(x)
  expect_intol(e$mean(), 0.5, 0.01)
  v <- (n3 * (-0.5) ^ 2L + n3 * 0.0 + n3 * (0.5) ^2L) / (3L * n3 - 1L)
  expect_intol(e$SD(), sqrt(v), 0.01)
  expect_true(is.na(e$mode()))
  probs <- c(0.025, 0.5, 0.975)
  q <- e$quantile(probs)
  expect_intol(q[[1L]], 0.0, 0.01)
  expect_intol(q[[2L]], 0.5, 0.01)
  expect_intol(q[[3L]], 1.0, 0.01)
})
 
test_that("quantile function checks inputs and has correct output", {
  x <- seq(from = 1.0, to = 1000.0)
  e <- EmpiricalDistribution$new(x)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(e$quantile(probs))
  probs <- c(0.1, NA_real_, 0.5)
  expect_error(e$quantile(probs), class = "probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(e$quantile(probs), class = "probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(e$quantile(probs), class = "probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_length(e$quantile(probs), 3L)
})

test_that("random draw sampling is from the supplied distribution", {
  x <- seq(5L, 15L)
  # build the empirical distribution
  e <- EmpiricalDistribution$new(x, interpolate.sample = FALSE)
  # random sampling
  n <- 20L
  samp <- vapply(seq_len(n), FUN.VALUE = 1.0, FUN = function(i) {
    e$sample()
    rv <- e$r()
    return(rv)
  })
  expect_length(samp, n)
  # test all values are from x
  expect_true(all(samp %in% x))
})

test_that("random interpolated sampling is from a supplied Gamma dist", {
  # create the gamma distribution
  k <- 9.0
  theta <- 0.5
  n <- 1000L
  x <- stats::rgamma(n, shape = k, scale = theta)
  # build the empirical distribution
  e <- EmpiricalDistribution$new(x)
  # random sampling
  samp <- vapply(seq_len(n), FUN.VALUE = 1.0, FUN = function(i) {
    e$sample()
    rv <- e$r()
    return(rv)
  })
  expect_length(samp, n)
  # 99.9% confidence limits; expected test failure rate is 0.1%;
  # skip for CRAN
  skip_on_cran()
  ht <- stats::ks.test(samp, stats::rgamma(n, shape = k, scale = theta))
  expect_gt(ht$p.value, 0.001)
})
