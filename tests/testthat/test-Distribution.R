
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
test_that("sd has the correct length", {
  D <- Distribution$new("Base", K=as.integer(3))
  expect_length(D$SD(),3)
  D <- Distribution$new("Base")
  expect_length(D$SD(),1)
})

test_that("varcov matrix has correct properties", {
  # univariate
  D <- Distribution$new("Base", K=as.integer(1))
  VC <- D$varcov()
  expect_false(is.matrix(VC))
  expect_true(is.numeric(VC))
  # multivariate
  D <- Distribution$new("Base", K=as.integer(3))
  VC <- D$varcov()
  expect_true(is.matrix(VC))
  expect_equal(nrow(VC), 3)
  expect_equal(ncol(VC), 3)
})

# --------------------------------------------------------------------------
# tests of quantiles
# --------------------------------------------------------------------------
test_that("form of quantiles object is correct", {
  # univariate
  D <- Distribution$new("Base")
  q <- D$quantile(probs=c(0.025,0.975))
  expect_length(q, 2)
  expect_setequal(names(q), c("0.025", "0.975"))
  # trivariate
  D <- Distribution$new("Base", K=as.integer(3))
  q <- D$quantile(probs=c(0.025,0.975))
  expect_true(is.matrix(q))
  expect_equal(nrow(q), 2)
  expect_equal(ncol(q), 3)
  expect_setequal(rownames(q), c("0.025", "0.975"))
  expect_setequal(colnames(q), c("1", "2", "3"))
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
