test_that("illegal initializations are rejected", {
  alpha <- c(2,6)
  expect_silent(DirichletDistribution$new(alpha))
  expect_error(DirichletDistribution$new(c(2)), class="alpha_unsupported")
  expect_error(DirichletDistribution$new(c("0.5",2)), class="alpha_not_numeric")
  expect_error(DirichletDistribution$new(c(2,NA)), class="alpha_not_defined")
  expect_error(DirichletDistribution$new(c(2,0)), class="alpha_unsupported")
  expect_error(DirichletDistribution$new(c(2,-1)), class="alpha_unsupported")
})

test_that("order is returned correctly", {
  alpha <- c(2,6)
  D <- DirichletDistribution$new(alpha)
  expect_equal(D$order(), 2)
  alpha <- c(3, 12, 9)
  D <- DirichletDistribution$new(alpha=alpha)
  expect_equal(D$order(), 3)
})

test_that("the correct distribution name is created", {
  alpha <- c(2,6)
  D <- DirichletDistribution$new(alpha)
  expect_equal(D$distribution(), "Dir(2,6)")
})

test_that("mean is calculated correctly", {
  alpha <- c(3, 12, 9)
  D <- DirichletDistribution$new(alpha=alpha)
  m <- D$mean()
  expect_equal(m, c(1/8, 1/2, 3/8))
})

test_that("mode is calculated correctly", {
  alpha <- c(3, 12, 9)
  D <- DirichletDistribution$new(alpha=alpha)
  m <- D$mode()
  expect_equal(m, c(2/21, 11/21, 8/21))
  #
  alpha <- c(3, 0.5, 9)
  D <- DirichletDistribution$new(alpha=alpha)
  m <- D$mode()
  expect_true(all(is.na(m)))
})

test_that("SD is NA for a multivariate distribution", {
  alpha <- c(3, 12, 9)
  D <- DirichletDistribution$new(alpha=alpha)
  expect_length(D$SD(), 3)
  expect_true(all(is.na(D$SD())))
})

test_that("quantile function checks inputs and has correct output", {
  alpha <- c(3, 12, 9)
  x <- DirichletDistribution$new(alpha=alpha)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(x$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(x$quantile(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(x$quantile(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(x$quantile(probs), class="probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5, 0.9)
  q <- x$quantile(probs)
  expect_true(is.matrix(q))
  expect_equal(ncol(q),3)
  expect_equal(nrow(q),4)
})

test_that("marginal quantiles are from Beta distributions", {
  # create distribution
  alpha <- c(3, 12, 9)
  D <- DirichletDistribution$new(alpha=alpha)
  # test single quantile
  Q <- D$quantile(0.5)
  qm <- stats::qbeta(p=0.5, shape1=3, shape2=21)
  expect_equal(unname(qm), Q["0.5","1"])
  # test UQ and LQ in 3D
  p <- c(0.25, 0.75)
  Q <- D$quantile(p)
  expect_true(is.matrix(Q))
  q1 <- stats::qbeta(p, shape1=3, shape2=21)
  expect_equal(q1, unname(Q[,"1"]))
  q2 <- stats::qbeta(p, shape1=12, shape2=12)
  expect_equal(q2, unname(Q[,"2"]))
  q3 <- stats::qbeta(p, shape1=9, shape2=15)
  expect_equal(q3, unname(Q[,"3"]))
})

test_that("variance/covariance matrix is calculated correctly", {
  alpha <- c(3, 12, 9)
  D <- DirichletDistribution$new(alpha=alpha)
  VC <- D$varcov()
  expect_true(is.matrix(VC))
  expect_equal(nrow(VC), 3)
  expect_equal(ncol(VC), 3)
  VCE <- matrix(
    c(7/64, -1/16, -3/64, -1/16, 1/4, -3/16, -3/64, -3/16, 15/64), 
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )
  VCE <- VCE/25
  expect_intol(sum(VC-VCE), 0, tolerance = 0.0001)
})

test_that("random sampling is from a Dirichlet distribution", {
  # create a distribution
  alpha <- c(3, 12, 9)
  D <- DirichletDistribution$new(alpha=alpha)
  # test that means are provided at first call to r() without sampling
  samp <- D$r()
  expect_identical(samp, c(1/8, 1/2, 3/8))
  # sample from it
  n <- 1000
  osamp <- matrix(nrow=n, ncol=3)
  for (i in 1:n) {
    D$sample()
    osamp[i,] <- D$r()
  }
  expect_equal(nrow(osamp), n)
  expect_equal(ncol(osamp), 3)
  expect_equal(sum(is.na(osamp)), 0)
  # check that marginal distributions are Beta(). Use 99.9% confidence limits;
  # expected test failure rate is 0.1%; 
  # skip for CRAN
  skip_on_cran()
  marg1 <- rbeta(n, shape1=alpha[1], shape2=sum(alpha)-alpha[1])
  marg2 <- rbeta(n, shape1=alpha[2], shape2=sum(alpha)-alpha[2])
  marg3 <- rbeta(n, shape1=alpha[3], shape2=sum(alpha)-alpha[3])
  ht <- ks.test(osamp[,1], marg1)
  expect_true(ht$p.value > 0.001)
  ht <- ks.test(osamp[,2], marg2)
  expect_true(ht$p.value > 0.001)
  ht <- ks.test(osamp[,3], marg3)
  expect_true(ht$p.value > 0.001)
})
