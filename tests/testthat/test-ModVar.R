# --------------------------------------------------------------------------
# tests of creation
# --------------------------------------------------------------------------
test_that("illegal arguments are rejected", {
  expect_error(ModVar$new(42L, "GBP"), class = "description_not_string")
  expect_error(ModVar$new(TRUE, "GBP"), class = "description_not_string")
  expect_error(ModVar$new("x", 42L), class = "units_not_string")
  expect_error(ModVar$new("x", FALSE), class = "units_not_string")
})

test_that("properties are set correctly", {
  v <- ModVar$new("dummy", "m")
  expect_false(v$is_probabilistic())
  expect_false(v$is_expression())
})

test_that("ModVar description is saved", {
  # same environment
  x <- ModVar$new("x", "GBP")
  expect_identical(x$description(), "x")
  # child environment
  f <- function() {
    expect_identical(x$description(), "x")
  }
  f()
  # parent environment
  g <- function() {
    y <- ModVar$new("y", "GBP")
    return(y)
  }
  yy <- g()
  expect_identical(yy$description(), "y")
})

test_that("association with uncertainty distributions is correct", {
  # incorrect distribution type
  expect_error(
    ModVar$new("x", "GBP", 42L),
    class = "invalid_distribution"
  )
  # non-integer index
  expect_error(
    ModVar$new("x", "GBP", k = 2.0),
    class = "invalid_index"
  )
  # integer index
  expect_silent(
    ModVar$new("x", "GBP", k = 1L)
  )
  # index out of range
  expect_error(
    ModVar$new("x", "GBP", k = 0L),
    class = "invalid_index"
  )
  expect_error(
    ModVar$new("x", "GBP", k = 2L),
    class = "invalid_index"
  )
})

# --------------------------------------------------------------------------
# tests of uncertainty values
# --------------------------------------------------------------------------
test_that("stub quantile function checks inputs and has correct output", {
  x <- ModVar$new("x", "GBP")
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(x$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(x$quantile(probs), class = "probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(x$quantile(probs), class = "probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(x$quantile(probs), class = "probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_length(x$quantile(probs), 3L)
})

test_that("stub functions return NA", {
  x <- ModVar$new("x", "GBP")
  expect_true(is.na(x$distribution()))
  expect_true(is.na(x$mode()))
  expect_true(is.na(x$SD()))
})

# --------------------------------------------------------------------------
# tests of set and get
# --------------------------------------------------------------------------
test_that("set checks its argument", {
  x <- ModVar$new("x", "GBP")
  expect_error(x$set(42L), class = "what_not_character")
  expect_error(x$set(TRUE), class = "what_not_character")
  expect_error(x$set("arodnm"), class = "what_not_supported")
  expect_error(x$set("value"), class = "invalid_val")
  expect_error(x$set("value", "b42"), class = "invalid_val")
  expect_silent(x$set("expected"))
  expect_silent(x$set())
  expect_silent(x$set("value", 42L))
  expect_silent(x$set("value", 42.0))
})

test_that("get is initialized to NA for base class", {
  x <- ModVar$new("x", "GBP")
  expect_true(is.na(x$get()))
})

test_that("get() after set('current') returns NA", {
  x <- ModVar$new("x", "GBP")
  x$set("current")
  expect_true(is.na(x$get()))
})

test_that("get() after set('random') returns NA", {
  x <- ModVar$new("x", "GBP")
  x$set("random")
  expect_true(is.na(x$get()))
})

# --------------------------------------------------------------------------
# modvars associated with univariate and multivariate distributions
# --------------------------------------------------------------------------
test_that("modvar can be associated with a univariate uncertainty", {
  # create Beta distribution
  D <- BetaDistribution$new(alpha = 1.0, beta = 9.0)
  # create a ModVar and associate with the distribution
  m <- ModVar$new("p(success)", "P", D = D, k = 1L)
  expect_identical(m$mean(), 1L / 10L)
  expect_identical(
    unname(m$quantile(0.5)),
    stats::qbeta(p = 0.5, shape1 = 1.0, shape2 = 9.0)
  )
})

test_that("modvars can be associated with a dimension of a multivariate dist", {
  # create a Dirichlet distribution
  D <- DirichletDistribution$new(c(1L, 9L))
  # create a ModVar and associate it with the first dimension
  m1 <- ModVar$new("p(success)", "P", D = D, k = 1L)
  expect_identical(m1$mean(), 1L / 10L)
  expect_identical(
    unname(m1$quantile(0.5)),
    stats::qbeta(p = 0.5, shape1 = 1.0, shape2 = 9.0)
  )
  expect_identical(m1$distribution(), "Dir(1,9)[1]")
  expect_identical(m1$r(), 1L / 10L)   # initialized to mean()
  # create a ModVar and associate it with the second dimension
  m2 <- ModVar$new("p(failure)", "P", D = D, k = 2L)
  expect_identical(m2$mean(), 9L / 10L)
  expect_identical(
    unname(m2$quantile(0.5)),
    stats::qbeta(p = 0.5, shape1 = 9L, shape2 = 1L)
  )
  expect_identical(m2$distribution(), "Dir(1,9)[2]")
  expect_identical(m2$r(), 9L / 10L)   # initialized to mean()
  # access distributional values via get()
  m1$set("expected")
  expect_identical(m1$get(), 1L / 10L)
})
