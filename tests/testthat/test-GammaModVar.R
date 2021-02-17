
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
  expect_equal(g$distribution(), "Ga(9,0.5)")
})

test_that("get() is initialized correctly", {
  k <- 9
  theta <- 0.5
  g <- GammaModVar$new("gamma", "GBP", k, theta)
  expect_equal(g$get(), k*theta, tolerance=0.01)
})

test_that("mean, mode, sd and quantiles are returned correctly", {
  k <- 9
  theta <- 0.5
  g <- GammaModVar$new("gamma", "GBP", k, theta)
  expect_equal(g$mean(), k*theta, tolerance=0.01)
  expect_equal(g$SD(), sqrt(k)*theta, tolerance=0.01)
  expect_equal(g$mode(), (k-1)*theta)
  probs <- c(0.025, 0.975)
  q <- g$quantile(probs)
  expect_equal(q[1], 2.06, tolerance=0.01)
  expect_equal(q[2], 7.88, tolerance=0.01)
})

test_that("random sampling is from a Gamma distribution", {
  k <- 9
  theta <- 0.5
  g <- GammaModVar$new("gamma", "GBP", k, theta)
  samp <- g$r(1000)
  expect_equal(length(samp), 1000)
  expect_equal(mean(samp), 4.5, tolerance=0.1)
  expect_equal(sd(samp), 1.5, tolerance=0.1)
})
