
test_that("illegal initializations are rejected", {
  a <- 2L
  b <- 5L
  expect_silent(BetaModVar$new("beta", "GBP", a, b))
  expect_error(BetaModVar$new(42L, 42L, a, b), class = "description_not_string")
  expect_error(BetaModVar$new("beta", 42L, a, b), class = "units_not_string")
  expect_error(
    BetaModVar$new("beta", "GBP", "9", b),
    class = "alpha_not_numeric"
  )
  expect_error(
    BetaModVar$new("beta", "GBP", a, "0.5"),
    class = "beta_not_numeric"
  )
  expect_error(
    BetaModVar$new("beta", "GBP", -1L, b),
    class = "alpha_not_supported"
  )
  expect_error(
    BetaModVar$new("beta", "GBP", a, 0L),
    class = "beta_not_supported"
  )
})

test_that("properties are correct", {
  a <- 2L
  b <- 5L
  B <- BetaModVar$new("beta", "GBP", a, b)
  expect_false(B$is_expression())
  expect_true(B$is_probabilistic())
})

test_that("modvar has correct distribution name", {
  a <- 2L
  b <- 5L
  B <- BetaModVar$new("beta", "GBP", a, b)
  expect_identical(B$distribution(), "Be(2,5)")
})

test_that("get() is initialized correctly", {
  a <- 2L
  b <- 5L
  B <- BetaModVar$new("beta", "GBP", a, b)
  expect_intol(B$get(), a / (a + b), 0.02)
})

test_that("set(current) works as intended", {
  a <- 2L
  b <- 5L
  B <- BetaModVar$new("beta", "GBP", a, b)
  B$set("random")
  x <- B$get()
  B$set("current")
  y <- B$get()
  expect_identical(y, x)
})

test_that("set(value) works as intended", {
  a <- 2L
  b <- 5L
  B <- BetaModVar$new("beta", "GBP", a, b)
  B$set("random")
  x <- B$get()
  expect_true((x >= 0.0 & x <= 1.0))
  B$set("value", 42.0)
  expect_identical(B$get(), 42.0)
})

test_that("mean, mode, sd and quantiles are returned correctly", {
  alpha <- 2L
  beta <- 5L
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
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
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(b$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(b$quantile(probs), class = "probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(b$quantile(probs), class = "probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(b$quantile(probs), class = "probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_length(b$quantile(probs), 3L)
})

test_that("Extreme mode values are defined", {
  alpha <- 1L
  beta <- 1L
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  expect_equal(b$mode(), 0.5)
  alpha <- 0.5
  beta <- 0.5
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  expect_true(is.na(b$mode()))
  alpha <- 0.5
  beta <- 1.5
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  expect_identical(b$mode(), 0.0)
  alpha <- 1.5
  beta <- 0.5
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  expect_equal(b$mode(), 1.0)
})

test_that("random sampling is from a Beta distribution", {
  alpha <- 2L
  beta <- 5L
  n <- 1000L
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  osamp <- vapply(seq_len(n), FUN.VALUE = 1.0, FUN = function(i) {
    b$set("random")
    rv <- b$get()
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
