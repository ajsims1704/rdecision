
test_that("illegal initializations are rejected", {
  k <- 9
  theta <- 0.5
  expect_silent(GammaModVar$new("gamma","GBP",k,theta))
  expect_error(GammaModVar$new(42,42,k,theta), 
               class="description_not_string")
  expect_error(GammaModVar$new("gamma",42,k,theta), 
               class="units_not_string")
  expect_error(GammaModVar$new("gamma","GBP","9",theta), 
               class="shape_not_numeric")
  expect_error(GammaModVar$new("gamma","GBP",k,"0.5"), 
               class="scale_not_numeric")
  expect_error(GammaModVar$new("gamma","GBP",-1,theta), 
               class="shape_not_supported")
  expect_error(GammaModVar$new("gamma","GBP",k,0), 
               class="scale_not_supported")
})

test_that("properties are correct", {
  k <- 9
  theta <- 0.5
  g <- GammaModVar$new("gamma", "GBP", k, theta)
  expect_false(g$is_expression())
  expect_true(g$is_probabilistic())
})

test_that("modvar has correct distribution name", {
  k <- 9
  theta <- 0.5
  g <- GammaModVar$new("gamma", "GBP", k, theta)
  expect_identical(g$distribution(), "Ga(9,0.5)")
})

test_that("get() is initialized correctly", {
  k <- 9
  theta <- 0.5
  g <- GammaModVar$new("gamma", "GBP", k, theta)
  expect_intol(g$get(), k*theta, 0.01)
})

test_that("mean, mode, sd and quantiles are returned correctly", {
  k <- 9
  theta <- 0.5
  g <- GammaModVar$new("gamma", "GBP", k, theta)
  expect_intol(g$mean(), k*theta, 0.01)
  expect_intol(g$SD(), sqrt(k)*theta, 0.01)
  expect_intol(g$mode(), (k-1)*theta, 0.01)
  probs <- c(0.025, 0.975)
  q <- g$quantile(probs)
  expect_intol(q[1], 2.06, 0.01)
  expect_intol(q[2], 7.88, 0.01)
})

test_that("stub quantile function checks inputs and has correct output", {
  k <- 9
  theta <- 0.5
  g <- GammaModVar$new("gamma", "GBP", k, theta)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(g$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(g$quantile(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(g$quantile(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(g$quantile(probs), class="probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_length(g$quantile(probs),3)
})

test_that("random sampling is from a Gamma distribution", {
  k <- 9
  theta <- 0.5
  n <- 1000
  g <- GammaModVar$new("gamma", "GBP", k, theta)
  samp <- g$r(n)
  expect_length(samp, n)
  # 99.9% empirical confidence limits; expected test failure rate is 0.1%;
  # skip for CRAN
  skip_on_cran()
  ci <- gamma.sampleCI(k, theta, n)
  expect_between(mean(samp), ci$mean.CI[1], ci$mean.CI[2])
  expect_between(sd(samp), ci$sd.CI[1], ci$sd.CI[2])
})
