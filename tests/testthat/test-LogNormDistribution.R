
test_that("illegal initializations are rejected", {
  expect_silent(LogNormDistribution$new(1, 1))
  expect_error(LogNormDistribution$new("1",1), class="p1_not_numeric")
  expect_error(LogNormDistribution$new(1,"1"), class="p2_not_numeric")
  expect_error(LogNormDistribution$new(1,1,"LN8"), 
               class="parametrization_not_supported")
})

test_that("distribution name is correct", {
  ln <- LogNormDistribution$new(1, 1)
  expect_identical(ln$distribution(), "LN(1,1)")
  ln <- LogNormDistribution$new(1, 1, "LN2")
  expect_identical(ln$distribution(), "LN(1,1)")
})

test_that("mean, mode, sd and quantiles are returned correctly", {
  mu <- 0
  sigma <- 0.25
  ln <- LogNormDistribution$new(p1=mu, p2=sigma, "LN1")
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
  ln <- LogNormDistribution$new(p1=mu, p2=sigma, "LN1")
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

test_that("random sampling is from a log normal distribution", {
  mu <- 0
  sigma <- 0.25
  n <- 1000
  ln <- LogNormDistribution$new(mu, sigma, "LN1")
  # check mean
  ln$sample(TRUE)
  expect_equal(ln$r(), exp(mu+0.5*sigma^2))
  # check sampling
  samp <- sapply(1:n, FUN=function(i) {
    ln$sample()
    rv <- ln$r()
    return(rv)
  })
  expect_length(samp, n)
  # expect sample mean and sd to fall within 99.9% CI; test
  # expected to fail 0.1% of the time, exclude from CRAN
  skip_on_cran()
  ht <- ks.test(samp, rlnorm(n,meanlog=mu,sdlog=sigma))
  expect_true(ht$p.value > 0.001)
})

test_that("parametrizations are linked", {
  mu <- 0
  sigma <- 0.25
  # 1
  ln1 <- LogNormDistribution$new(mu, sigma, "LN1")
  # 2
  ln2 <- LogNormDistribution$new(mu, sigma^2, "LN2")
  expect_intol(ln1$mean(), ln2$mean(), 0.01)
  expect_intol(ln1$mode(), ln2$mode(), 0.01)
  # 3
  ln3 <- LogNormDistribution$new(exp(mu), sigma, "LN3")
  expect_intol(ln1$mean(), ln3$mean(), 0.01)
  expect_intol(ln1$mode(), ln3$mode(), 0.01)
  # 4
  ln4 <- LogNormDistribution$new(exp(mu), sqrt(exp(sigma^2)-1), "LN4")
  expect_intol(ln1$mean(), ln4$mean(), 0.01)
  expect_intol(ln1$mode(), ln4$mode(), 0.01)
  # 5
  ln5 <- LogNormDistribution$new(mu, 1/sigma^2, "LN5")
  expect_intol(ln1$mean(), ln5$mean(), 0.01)
  expect_intol(ln1$mode(), ln5$mode(), 0.01)
  # 6
  ln6 <- LogNormDistribution$new(exp(mu), exp(sigma), "LN6")
  expect_intol(ln1$mean(), ln6$mean(), 0.01)
  expect_intol(ln1$mode(), ln6$mode(), 0.01)
  # 7
  ln7 <- LogNormDistribution$new(
    exp(mu+(sigma^2)/2), 
    exp(mu+(sigma^2)/2)*sqrt(exp(sigma^2)-1), "LN7"
  )
  expect_intol(ln1$mean(), ln7$mean(), 0.01)
  expect_intol(ln1$mode(), ln7$mode(), 0.01)
})

