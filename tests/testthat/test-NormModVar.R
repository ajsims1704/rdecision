
test_that("illegal initializations are rejected", {
  expect_silent(NormModVar$new("norm", "GBP", 0, 1))
  expect_error(NormModVar$new(42,42,0,1), class="description_not_string")
  expect_error(NormModVar$new("norm",42,0,1), class="units_not_string")
  expect_error(NormModVar$new("norm","GBP","0",1), class="mu_not_numeric")
  expect_error(NormModVar$new("norm","GBP",0,"1"), class="sigma_not_numeric")
})

test_that("properties are correct", {
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  expect_false(sn$is_expression())
  expect_true(sn$is_probabilistic())
})

test_that("modvar has correct distribution name", {
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  expect_identical(sn$distribution(), "N(0,1)")  
  n <- NormModVar$new("n", "GBP", 42, 1)
  expect_identical(n$distribution(), "N(42,1)")  
})

test_that("quantile function checks inputs", {
  x <- NormModVar$new("x", "GBP", 0, 1)
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
  sn <- NormModVar$new("sn", "GBP", 0, 1)
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
  sn <- NormModVar$new("sn", "GBP", mu, sigma)
  n <- 1000
  samp <- sn$r(n)
  expect_length(samp, n)
  # check sample mean and sd are within 99.9% CI based on CLT; this is exact
  # for a normal, and is expected to fail for 0.1% of tests; skip for CRAN
  skip_on_cran()
  expect_normsample(samp, mu, sigma)
})

test_that("First call to get() returns mean", {
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  expect_intol(sn$get(), 0, 0.01)  
})

test_that("variable passing and persistency of get/set are correct", {
  f <- function(mv) {
    expect_equal(mv$get(), 0)
    mv$set("q2.5")
  }
  g <- function(mv) {
    expect_intol(mv$get(), 0, 0.01)
  }
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  f(sn)
  expect_false(sn$get()==0)
  sn$set("expected")
  g(sn)
})
