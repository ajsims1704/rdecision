test_that("illegal initializations are rejected", {
  alpha <- c(2.0, 6.0)
  expect_silent(DirichletDistribution$new(alpha))
  expect_error(DirichletDistribution$new(2.0), class = "alpha_unsupported")
  expect_error(
    DirichletDistribution$new(c("0.5", 2.0)), class = "alpha_not_numeric"
  )
  expect_error(
    DirichletDistribution$new(c(2.0, NA_real_)), class = "alpha_not_defined"
  )
  expect_error(
    DirichletDistribution$new(c(2.0, 0.0)), class = "alpha_unsupported"
  )
  expect_error(
    DirichletDistribution$new(c(2.0, -1.0)), class = "alpha_unsupported"
  )
})

test_that("order is returned correctly", {
  alpha <- c(2.0, 6.0)
  D <- DirichletDistribution$new(alpha)
  expect_identical(D$order(), 2L)
  alpha <- c(3.0, 12.0, 9.0)
  D <- DirichletDistribution$new(alpha = alpha)
  expect_identical(D$order(), 3L)
})

test_that("the correct distribution name is created", {
  alpha <- c(2.0, 6.0)
  D <- DirichletDistribution$new(alpha)
  expect_identical(D$distribution(), "Dir(2,6)")
})

test_that("mean is calculated correctly", {
  alpha <- c(3L, 12L, 9L)
  D <- DirichletDistribution$new(alpha = alpha)
  m <- D$mean()
  expect_identical(m, c(1L / 8L, 1L / 2L, 3L / 8L))
})

test_that("mode is calculated correctly", {
  alpha <- c(3L, 12L, 9L)
  D <- DirichletDistribution$new(alpha = alpha)
  m <- D$mode()
  expect_identical(m, c(2L / 21L, 11L / 21L, 8L / 21L))
  #
  alpha <- c(3.0, 0.5, 9.0)
  D <- DirichletDistribution$new(alpha = alpha)
  m <- D$mode()
  expect_true(all(is.na(m)))
})

test_that("SD is NA for a multivariate distribution", {
  alpha <- c(3L, 12L, 9L)
  D <- DirichletDistribution$new(alpha = alpha)
  expect_length(D$SD(), 3L)
  expect_true(all(is.na(D$SD())))
})

test_that("quantile function checks inputs and has correct output", {
  alpha <- c(3L, 12L, 9L)
  x <- DirichletDistribution$new(alpha = alpha)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(x$quantile(probs))
  probs <- c(0.1, NA_real_, 0.5)
  expect_error(x$quantile(probs), class = "probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(x$quantile(probs), class = "probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(x$quantile(probs), class = "probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5, 0.9)
  q <- x$quantile(probs)
  expect_true(is.matrix(q))
  expect_identical(ncol(q), 3L)
  expect_identical(nrow(q), 4L)
})

test_that("marginal quantiles are from Beta distributions", {
  # create distribution
  alpha <- c(3L, 12L, 9L)
  D <- DirichletDistribution$new(alpha = alpha)
  # test single quantile
  Q <- D$quantile(0.5)
  qm <- stats::qbeta(p = 0.5, shape1 = 3L, shape2 = 21L)
  expect_identical(unname(qm), Q["0.5", "1"])
  # test UQ and LQ in 3D
  p <- c(0.25, 0.75)
  Q <- D$quantile(p)
  expect_true(is.matrix(Q))
  q1 <- stats::qbeta(p, shape1 = 3L, shape2 = 21L)
  expect_identical(q1, unname(Q[, "1"]))
  q2 <- stats::qbeta(p, shape1 = 12L, shape2 = 12L)
  expect_identical(q2, unname(Q[, "2"]))
  q3 <- stats::qbeta(p, shape1 = 9L, shape2 = 15L)
  expect_identical(q3, unname(Q[, "3"]))
})

test_that("variance-covariance matrix is calculated correctly", {
  alpha <- c(3L, 12L, 9L)
  D <- DirichletDistribution$new(alpha = alpha)
  VC <- D$varcov()
  expect_true(is.matrix(VC))
  expect_identical(nrow(VC), 3L)
  expect_identical(ncol(VC), 3L)
  VCE <- matrix(
    c(7L / 64L, -1L / 16L, -3L / 64L,
      -1L / 16L, 1L / 4L, -3L / 16L,
      -3L / 64L, -3L / 16L, 15L / 64L
    ),
    nrow = 3L,
    ncol = 3L,
    byrow = TRUE
  )
  VCE <- VCE / 25L
  expect_intol(sum(VC - VCE), 0.0, tolerance = 0.0001)
})

test_that("random sampling is from a Dirichlet distribution", {
  # create a distribution
  alpha <- c(3L, 12L, 9L)
  D <- DirichletDistribution$new(alpha = alpha)
  # test that means are provided at first call to r() without sampling
  samp <- D$r()
  expect_identical(samp, c(1L / 8L, 1L / 2L, 3L / 8L))
  # test that sample argument works
  D$sample()
  D$sample(expected = TRUE)
  samp <- D$r()
  expect_identical(samp, c(1L / 8L, 1L / 2L, 3L / 8L))
  # sample from it
  n <- 1000L
  osamp <- matrix(nrow = n, ncol = 3L)
  for (i in seq_len(n)) {
    D$sample()
    osamp[i, ] <- D$r()
  }
  expect_identical(nrow(osamp), n)
  expect_identical(ncol(osamp), 3L)
  expect_identical(sum(is.na(osamp)), 0L)
  # check that marginal distributions are Beta(). Use 99.9% confidence limits;
  # expected test failure rate is 0.1%;
  # skip for CRAN
  skip_on_cran()
  marg1 <- rbeta(n, shape1 = alpha[[1L]], shape2 = sum(alpha) - alpha[[1L]])
  marg2 <- rbeta(n, shape1 = alpha[[2L]], shape2 = sum(alpha) - alpha[[2L]])
  marg3 <- rbeta(n, shape1 = alpha[[3L]], shape2 = sum(alpha) - alpha[[3L]])
  ht <- ks.test(osamp[, 1L], marg1)
  expect_gt(ht$p.value, 0.001)
  ht <- ks.test(osamp[, 2L], marg2)
  expect_gt(ht$p.value, 0.001)
  ht <- ks.test(osamp[, 3L], marg3)
  expect_gt(ht$p.value, 0.001)
})
