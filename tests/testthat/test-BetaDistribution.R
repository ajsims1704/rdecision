test_that("illegal initializations are rejected", {
  a <- 2L
  b <- 5L
  expect_silent(BetaDistribution$new(a, b))
  expect_error(BetaDistribution$new("9", b), class = "alpha_not_numeric")
  expect_error(BetaDistribution$new(a, "0.5"), class = "beta_not_numeric")
  expect_error(BetaDistribution$new(-1L, b), class = "alpha_not_supported")
  expect_error(BetaDistribution$new(a, 0L), class = "beta_not_supported")
})

test_that("the correct distribution name is created", {
  a <- 2L
  b <- 5L
  B <- BetaDistribution$new(a, b)
  expect_identical(B$distribution(), "Be(2,5)")
})

test_that("mean, mode, sd and quantiles are returned correctly", {
  alpha <- 2L
  beta <- 5L
  b <- BetaDistribution$new(alpha, beta)
  m <- alpha / (alpha + beta)
  v <- (alpha * beta) / ((alpha + beta + 1L) * (alpha + beta) ^ 2L)
  o <- (alpha - 1L) / (alpha + beta - 2L)
  expect_intol(b$mean(), m, 0.01)
  expect_intol(b$SD(), sqrt(v), 0.01)
  expect_intol(b$mode(), o, 0.01)
  probs <- c(0.025, 0.975)
  q <- b$quantile(probs)
  expect_intol(q[[1L]], 0.043, 0.01)
  expect_intol(q[[2L]], 0.641, 0.01)
})

test_that("quantile function checks inputs and has correct output", {
  alpha <- 2L
  beta <- 5L
  b <- BetaDistribution$new(alpha, beta)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(b$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(b$quantile(probs), class = "probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(b$quantile(probs), class = "probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(b$quantile(probs), class = "probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  q <- b$quantile(probs)
  expect_length(q, 3L)
  expect_setequal(names(q), c("0.1", "0.2", "0.5"))
})

test_that("Extreme mode values are defined", {
  alpha <- 1L
  beta <- 1L
  b <- BetaDistribution$new(alpha, beta)
  expect_equal(b$mode(), 0.5)
  alpha <- 0.5
  beta <- 0.5
  b <- BetaDistribution$new(alpha, beta)
  expect_true(is.na(b$mode()))
  alpha <- 0.5
  beta <- 1.5
  b <- BetaDistribution$new(alpha, beta)
  expect_intol(b$mode(), 0.0, 0.01)
  alpha <- 1.5
  beta <- 0.5
  b <- BetaDistribution$new(alpha, beta)
  expect_intol(b$mode(), 1.0, 0.01)
})

test_that("random sampling is from a Beta distribution", {
  # create distribution
  alpha <- 2L
  beta <- 5L
  b <- BetaDistribution$new(alpha, beta)
  # sample mean
  b$sample(TRUE)
  expect_intol(b$r(), 2.0 / 7.0, 0.01)
  # random sampling
  n <- 1000L
  osamp <- vapply(seq(n), FUN.VALUE = 1.0, FUN = function(i) {
    b$sample()
    rv <- b$r()
    return(rv)
  })
  expect_length(osamp, n)
  # 99.9% confidence limits; expected test failure rate is 0.1%,
  # skip for CRAN
  skip_on_cran()
  esamp <- rbeta(n, shape1 = alpha, shape2 = beta)
  ht <- ks.test(osamp, esamp)
  expect_gt(ht$p.value, 0.001)
})
