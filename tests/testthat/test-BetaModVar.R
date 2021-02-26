
test_that("illegal initializations are rejected", {
  a <- 2
  b <- 5
  expect_silent(BetaModVar$new("beta","GBP",a,b))
  expect_error(BetaModVar$new(42,42,a,b), class="description_not_string")
  expect_error(BetaModVar$new("beta",42,a,b), class="units_not_string")
  expect_error(BetaModVar$new("beta","GBP","9",b), class="alpha_not_numeric")
  expect_error(BetaModVar$new("beta","GBP",a,"0.5"), class="beta_not_numeric")
  expect_error(BetaModVar$new("beta","GBP",-1,b), class="alpha_not_supported")
  expect_error(BetaModVar$new("beta","GBP",a,0), class="beta_not_supported")
})

test_that("properties are correct", {
  a <- 2
  b <- 5
  B <- BetaModVar$new("beta", "GBP", a, b)
  expect_false(B$is_expression())
  expect_true(B$is_probabilistic())
})

test_that("modvar has correct distribution name", {
  a <- 2
  b <- 5
  B <- BetaModVar$new("beta", "GBP", a, b)
  expect_equal(B$distribution(), "Be(2,5)")
})

test_that("get() is initialized correctly", {
  a <- 2
  b <- 5
  B <- BetaModVar$new("beta", "GBP", a, b)
  expect_equal(B$get(), a/(a+b), tolerance=0.01)
})

test_that("set(current) works as intended", {
  a <- 2
  b <- 5
  B <- BetaModVar$new("beta", "GBP", a, b)
  B$set("random")
  x <- B$get()
  B$set("current")
  expect_equal(x, B$get())
})

test_that("mean, mode, sd and quantiles are returned correctly", {
  alpha <- 2
  beta <- 5
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  expect_equal(b$mean(), alpha/(alpha+beta), tolerance=0.01)
  var <- (alpha*beta)/((alpha+beta+1)*(alpha+beta)^2)
  expect_equal(b$SD(), sqrt(var), tolerance=0.01)
  expect_equal(b$mode(), (alpha-1)/(alpha+beta-2))
  probs <- c(0.025, 0.975)
  q <- b$quantile(probs)
  expect_equal(q[1], 0.043, tolerance=0.001)
  expect_equal(q[2], 0.641, tolerance=0.001)
})

test_that("quantile function checks inputs and has correct output", {
  alpha <- 2
  beta <- 5
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(b$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(b$quantile(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(b$quantile(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(b$quantile(probs), class="probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_equal(length(b$quantile(probs)),3)
})

test_that("Extreme mode values are defined", {
  alpha <- 1
  beta <- 1
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  expect_equal(b$mode(), 0.5)
  alpha <- 0.5
  beta <- 0.5
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  expect_true(is.na(b$mode()))
  alpha <- 0.5
  beta <- 1.5
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  expect_equal(b$mode(), 0)
  alpha <- 1.5
  beta <- 0.5
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  expect_equal(b$mode(), 1)
})

test_that("random sampling is from a Beta distribution", {
  alpha <- 2
  beta <- 5
  b <- BetaModVar$new("beta", "GBP", alpha, beta)
  samp <- b$r(1000)
  expect_equal(length(samp), 1000)
  expect_equal(mean(samp), alpha/(alpha+beta), tolerance=0.1)
  var <- (alpha*beta)/((alpha+beta+1)*(alpha+beta)^2)
  expect_equal(sd(samp), sqrt(var), tolerance=0.1)
})
