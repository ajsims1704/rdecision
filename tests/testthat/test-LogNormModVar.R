
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
  expect_equal(ln$distribution(), "LN(1,1)")
  ln <- LogNormModVar$new("lognorm", "GBP", 1, 1, "LN2")
  expect_equal(ln$distribution(), "LN(1,1)")
})

test_that("mean, mode, sd and quantiles are returned correctly", {
  mu <- 0
  sigma <- 0.25
  ln <- LogNormModVar$new("ln", "GBP", p1=mu, p2=sigma, "LN1")
  expect_equal(ln$mean(), exp(mu+(sigma^2)/2))
  sd <- sqrt( (exp(sigma^2)-1)*(exp(2*mu+sigma^2)) )
  expect_equal(ln$SD(), sd)
  expect_equal(ln$mode(), exp(mu-sigma^2))
  # quantiles
  expect_equal(ln$quantile(c(0.5)), exp(mu))
  erf.inv <- function(x){qnorm((x+1)/2)/sqrt(2)}
  ln.quant <- function(mu,sigma,p){exp(mu + sqrt(2*sigma^2)*erf.inv(2*p-1))}
  expect_equal(ln$quantile(c(0.25)), ln.quant(mu,sigma,0.25))
  expect_equal(ln$quantile(c(0.75)), ln.quant(mu,sigma,0.75))
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
  expect_equal(ln$get(), exp(mu+(sigma^2)/2))
})

test_that("random sampling is from a log normal distribution", {
  mu <- 0
  sigma <- 0.25
  ln <- LogNormModVar$new("ln", "GBP", mu, sigma)
  samp <- ln$r(1000)
  expect_equal(length(samp), 1000)
  expect_equal(mean(samp), 1, tolerance=0.2)
  sd <- sqrt( (exp(sigma^2)-1)*(exp(2*mu+sigma^2)) )
  expect_equal(sd(samp), sd, tolerance=0.2)
})

test_that("parametrizations are linked", {
  mu <- 0
  sigma <- 0.25
  ln1 <- LogNormModVar$new("ln1", "GBP", mu, sigma, "LN1")
  # 2
  ln2 <- LogNormModVar$new("ln2", "GBP", mu, sigma^2, "LN2")
  expect_equal(ln1$mean(), ln2$mean(), tolerance=0.01)
  expect_equal(ln1$mode(), ln2$mode(), tolerance=0.01)
  # 3
  ln3 <- LogNormModVar$new("ln3", "GBP", exp(mu), sigma, "LN3")
  expect_equal(ln1$mean(), ln3$mean(), tolerance=0.01)
  expect_equal(ln1$mode(), ln3$mode(), tolerance=0.01)
  # 4
  ln4 <- LogNormModVar$new("ln4", "GBP", exp(mu), sqrt(exp(sigma^2)-1), "LN4")
  expect_equal(ln1$mean(), ln4$mean(), tolerance=0.01)
  expect_equal(ln1$mode(), ln4$mode(), tolerance=0.01)
  # 5
  ln5 <- LogNormModVar$new("ln5", "GBP", mu, 1/sigma^2, "LN5")
  expect_equal(ln1$mean(), ln5$mean(), tolerance=0.01)
  expect_equal(ln1$mode(), ln5$mode(), tolerance=0.01)
  # 6
  ln6 <- LogNormModVar$new("ln6", "GBP", exp(mu), exp(sigma), "LN6")
  expect_equal(ln1$mean(), ln6$mean(), tolerance=0.01)
  expect_equal(ln1$mode(), ln6$mode(), tolerance=0.01)
  # 7
  ln7 <- LogNormModVar$new("ln7", "GBP", exp(mu+(sigma^2)/2), 
                           exp(mu+(sigma^2)/2)*sqrt(exp(sigma^2)-1), "LN7")
  expect_equal(ln1$mean(), ln7$mean(), tolerance=0.01)
  expect_equal(ln1$mode(), ln7$mode(), tolerance=0.01)
})

