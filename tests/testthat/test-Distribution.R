
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
    Distribution$new(name=42),
    class = "invalid_name"
  )
  # invalid dimensions
  expect_error(
    Distribution$new(name="Base", K=1),
    class = "invalid_order"
  )
  expect_error(
    Distribution$new(name="Base", K=as.integer(0)),
    class = "order_not_supported"
  )
  # valid dimension
  expect_silent(
    Distribution$new(name="Base", K=as.integer(1))
  )
  expect_silent(
    Distribution$new(name="Base", K=as.integer(3))
  )
  # order is returned
  D <- Distribution$new(name="Base", K=as.integer(3))
  expect_equal(D$order(), 3)
})

# --------------------------------------------------------------------------
# tests of random sampling
# --------------------------------------------------------------------------
test_that("single sample has K dimensions", {
  D <- Distribution$new("Base", K=as.integer(3))
  expect_length(D$r(), 3)
  D$sample()
  expect_length(D$r(), 3)
})

test_that("univariate samples are scalars", {
  D <- Distribution$new("Base", K=as.integer(1))
  R <- D$r()
  expect_true(is.vector(R))
  expect_length(R, 1)
})
  
# --------------------------------------------------------------------------
# tests of mean and mode
# --------------------------------------------------------------------------
test_that("mean has K dimensions", {
  D <- Distribution$new("Base", K=as.integer(3))
  expect_length(D$mean(), 3)
  D <- Distribution$new("Base", K=as.integer(1))
  expect_length(D$mean(), 1)
})

test_that("mode has K dimensions", {
  D <- Distribution$new("Base", K=as.integer(3))
  expect_length(D$mode(), 3)
  D <- Distribution$new("Base", K=as.integer(1))
  expect_length(D$mode(), 1)
})

# --------------------------------------------------------------------------
# tests of SD and variance
# --------------------------------------------------------------------------
test_that("sd is only defined for K=1", {
  D <- Distribution$new("Base", K=as.integer(3))
  expect_error(
    D$SD(),
    class = "SD_undefined"
  )
  D <- Distribution$new("Base")
  expect_silent(D$SD())
})

# --------------------------------------------------------------------------
# tests of quantiles
# --------------------------------------------------------------------------
test_that("quantiles are only available for K=1", {
  D <- Distribution$new("Base", K=as.integer(3))
  expect_error(
    D$quantile(probs=c(0.025,0.975)),
    class = "quantile_undefined"
  )
  D <- Distribution$new("Base")
  q <- D$quantile(probs=c(0.025,0.975))
  expect_length(q, 2)
})

