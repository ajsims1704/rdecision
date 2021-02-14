
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

# test_that("mean, mode, sd and quantiles are returned correctly", {
#   k <- 9
#   theta <- 0.5
#   g <- GammaModVar$new("gamma", "GBP", k, theta)
#   expect_equal(g$mean(), k*theta, tolerance=0.01)
#   expect_equal(g$SD(), sqrt(k)*theta, tolerance=0.01)
#   expect_equal(g$mode(), (k-1)*theta)
#   probs <- c(0.025, 0.975)
#   q <- g$quantile(probs)
#   expect_equal(round(q[1],2), 2.06, tolerance=0.01)
#   expect_equal(round(q[2],2), 7.88, tolerance=0.01)
# })
# 
# test_that("random sampling is from a Gamma distribution", {
#   k <- 9
#   theta <- 0.5
#   g <- GammaModVar$new("gamma", "GBP", k, theta)
#   samp <- g$r(1000)
#   expect_equal(length(samp), 1000)
#   expect_equal(mean(samp), 4.5, tolerance=0.1)
#   expect_equal(sd(samp), 1.5, tolerance=0.1)
# })
