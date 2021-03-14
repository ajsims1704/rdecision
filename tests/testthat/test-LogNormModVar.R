
test_that("illegal initializations are rejected", {
  expect_silent(LogNormModVar$new("lognorm", "GBP", 1, 1))
  expect_error(LogNormModVar$new(42,42,1,1), class="description_not_string")
  expect_error(LogNormModVar$new("lognorm",42,1,1), class="units_not_string")
  expect_error(LogNormModVar$new("lognorm","GBP","1",1), class="p1_not_numeric")
  expect_error(LogNormModVar$new("lognorm","GBP",1,"1"), class="p2_not_numeric")
  expect_error(LogNormModVar$new("lognorm","GBP",1,1,"LN8"), 
               class="parametrization_not_supported")
})

test_that("properties are correct", {
  ln <- LogNormModVar$new("lognorm", "GBP", 1, 1)
  expect_false(ln$is_expression())
  expect_true(ln$is_probabilistic())
})

test_that("modvar has correct distribution name", {
  ln <- LogNormModVar$new("lognorm", "GBP", 1, 1)
  expect_identical(ln$distribution(), "LN(1,1)")
  ln <- LogNormModVar$new("lognorm", "GBP", 1, 1, "LN2")
  expect_identical(ln$distribution(), "LN(1,1)")
})

test_that("mean, mode, sd and quantiles are returned correctly", {
  mu <- 0
  sigma <- 0.25
  ln <- LogNormModVar$new("ln", "GBP", p1=mu, p2=sigma, "LN1")
  expect_intol(ln$mean(), exp(mu+(sigma^2)/2), 0.01)
  sd <- sqrt( (exp(sigma^2)-1)*(exp(2*mu+sigma^2)) )
  expect_intol(ln$SD(), sd, 0.01)
  expect_intol(ln$mode(), exp(mu-sigma^2), 0.01)
  # quantiles
  expect_intol(ln$quantile(c(0.5)), exp(mu), 0.01)
  erf.inv <- function(x){qnorm((x+1)/2)/sqrt(2)}
  ln.quant <- function(mu,sigma,p){exp(mu + sqrt(2*sigma^2)*erf.inv(2*p-1))}
  expect_intol(ln$quantile(c(0.25)), ln.quant(mu,sigma,0.25), 0.01)
  expect_intol(ln$quantile(c(0.75)), ln.quant(mu,sigma,0.75), 0.01)
})

test_that("quantile function checks inputs and has correct output", {
  mu <- 0
  sigma <- 0.25
  ln <- LogNormModVar$new("ln", "GBP", p1=mu, p2=sigma, "LN1")
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(ln$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(ln$quantile(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(ln$quantile(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(ln$quantile(probs), class="probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_equal(length(ln$quantile(probs)),3)
})

test_that("get() is initialized correctly", {
  mu <- 0
  sigma <- 0.25
  ln <- LogNormModVar$new("ln", "GBP", p1=mu, p2=sigma, "LN1")
  expect_intol(ln$get(), exp(mu+(sigma^2)/2), 0.01)
})

test_that("random sampling is from a log normal distribution", {
  mu <- 0
  sigma <- 0.25
  n <- 1000
  ln <- LogNormModVar$new("ln", "GBP", mu, sigma, "LN1")
  samp <- ln$r(n)
  expect_length(samp, n)
  # expect sample mean and sd to fall within 99.9% CI assuming CLT; test
  # expected to fail 0.1% of the time, exclude from CRAN
  skip_on_cran()
  m.pop <- exp(mu + 0.5*sigma^2)
  sd.pop <- sqrt( (exp(sigma^2)-1)*(exp(2*mu+sigma^2)) )
  expect_normsample(samp, m.pop, sd.pop)
})

test_that("parametrizations are linked", {
  mu <- 0
  sigma <- 0.25
  # 1
  ln1 <- LogNormModVar$new("ln1", "GBP", mu, sigma, "LN1")
  # 2
  ln2 <- LogNormModVar$new("ln2", "GBP", mu, sigma^2, "LN2")
  expect_intol(ln1$mean(), ln2$mean(), 0.01)
  expect_intol(ln1$mode(), ln2$mode(), 0.01)
  # 3
  ln3 <- LogNormModVar$new("ln3", "GBP", exp(mu), sigma, "LN3")
  expect_intol(ln1$mean(), ln3$mean(), 0.01)
  expect_intol(ln1$mode(), ln3$mode(), 0.01)
  # 4
  ln4 <- LogNormModVar$new("ln4", "GBP", exp(mu), sqrt(exp(sigma^2)-1), "LN4")
  expect_intol(ln1$mean(), ln4$mean(), 0.01)
  expect_intol(ln1$mode(), ln4$mode(), 0.01)
  # 5
  ln5 <- LogNormModVar$new("ln5", "GBP", mu, 1/sigma^2, "LN5")
  expect_intol(ln1$mean(), ln5$mean(), 0.01)
  expect_intol(ln1$mode(), ln5$mode(), 0.01)
  # 6
  ln6 <- LogNormModVar$new("ln6", "GBP", exp(mu), exp(sigma), "LN6")
  expect_intol(ln1$mean(), ln6$mean(), 0.01)
  expect_intol(ln1$mode(), ln6$mode(), 0.01)
  # 7
  ln7 <- LogNormModVar$new(
    "ln7", "GBP", exp(mu+(sigma^2)/2), 
    exp(mu+(sigma^2)/2)*sqrt(exp(sigma^2)-1), "LN7"
  )
  expect_intol(ln1$mean(), ln7$mean(), 0.01)
  expect_intol(ln1$mode(), ln7$mode(), 0.01)
})

