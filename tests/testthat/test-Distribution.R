# --------------------------------------------------------------------------
# tests of creating distributions
# --------------------------------------------------------------------------
test_that("Invalid base distributions are rejected", {
  # missing name
  expect_error(
    Distribution$new(),
    class = "invalid_name"
  )
  # invalid name
  expect_error(
    Distribution$new(name = 42L),
    class = "invalid_name"
  )
  # invalid dimensions
  expect_error(
    Distribution$new(name = "Base", K = 1.0),
    class = "invalid_order"
  )
  expect_error(
    Distribution$new(name = "Base", K = 0L),
    class = "order_not_supported"
  )
  # valid dimension
  expect_silent(
    Distribution$new(name = "Base", K = 1L)
  )
  expect_silent(
    Distribution$new(name = "Base", K = 3L)
  )
  # order is returned
  D <- Distribution$new(name = "Base", K = 3L)
  expect_identical(D$order(), 3L)
})

# --------------------------------------------------------------------------
# tests of mean and mode
# --------------------------------------------------------------------------
test_that("mean has K dimensions", {
  D <- Distribution$new("Base", K = 3L)
  expect_length(D$mean(), 3L)
  D <- Distribution$new("Base", K = 1L)
  expect_length(D$mean(), 1L)
})

test_that("mode has K dimensions", {
  D <- Distribution$new("Base", K = 3L)
  expect_length(D$mode(), 3L)
  D <- Distribution$new("Base", K = 1L)
  expect_length(D$mode(), 1L)
})

# --------------------------------------------------------------------------
# tests of SD and variance
# --------------------------------------------------------------------------
test_that("sd has the correct length", {
  D <- Distribution$new("Base", K = 3L)
  expect_length(D$SD(), 3L)
  D <- Distribution$new("Base")
  expect_length(D$SD(), 1L)
})

test_that("varcov matrix has correct properties", {
  # univariate
  D <- Distribution$new("Base", K = 1L)
  VC <- D$varcov()
  expect_false(is.matrix(VC))
  expect_type(VC, "double")
  # multivariate
  D <- Distribution$new("Base", K = 3L)
  VC <- D$varcov()
  expect_true(is.matrix(VC))
  expect_identical(nrow(VC), 3L)
  expect_identical(ncol(VC), 3L)
})

# --------------------------------------------------------------------------
# tests of quantiles
# --------------------------------------------------------------------------
test_that("form of quantiles object is correct", {
  # univariate
  D <- Distribution$new("Base")
  q <- D$quantile(probs = c(0.025, 0.975))
  expect_length(q, 2L)
  expect_setequal(names(q), c("0.025", "0.975"))
  # trivariate
  D <- Distribution$new("Base", K = 3L)
  q <- D$quantile(probs = c(0.025, 0.975))
  expect_true(is.matrix(q))
  expect_identical(nrow(q), 2L)
  expect_identical(ncol(q), 3L)
  expect_setequal(rownames(q), c("0.025", "0.975"))
  expect_setequal(colnames(q), c("1", "2", "3"))
})

# --------------------------------------------------------------------------
# tests of random sampling
# --------------------------------------------------------------------------
test_that("single sample has K dimensions", {
  D <- Distribution$new("Base", K = 3L)
  expect_length(D$r(), 3L)
  D$sample()
  expect_length(D$r(), 3L)
})

test_that("invalid arguments to sample are detected", {
  D <- Distribution$new("Base", K = 3L)
  expect_error(D$sample(42L), class = "invalid_expected")
})

test_that("univariate samples are scalars", {
  D <- Distribution$new("Base", K = 1L)
  R <- D$r()
  expect_true(is.vector(R))
  expect_length(R, 1L)
})
