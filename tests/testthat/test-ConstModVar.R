
test_that("illegal initializations are rejected", {
  expect_silent(ConstModVar$new("const", "GBP", 42.5))
  expect_error(
    ConstModVar$new(42.0, 42.0, 42.0), class = "description_not_string"
  )
  expect_error(ConstModVar$new("const", 42L, 42L), class = "units_not_string")
  expect_error(
    ConstModVar$new("const", "GBP", "42"), class = "const_not_numeric"
  )
})

test_that("properties are correct", {
  lue <- ConstModVar$new("lue", "GBP", 42.0)
  expect_false(lue$is_expression())
  expect_false(lue$is_probabilistic())
})

test_that("it has correct distribution name", {
  lue <- ConstModVar$new("lue", "GBP", 42.0)
  expect_identical(lue$distribution(), "Const(42)")
})

test_that("const values are returned", {
  x <- ConstModVar$new("const", "GBP", 42.0)
  expect_identical(x$mean(), 42.0)
  expect_identical(x$SD(), 0.0)
  expect_identical(x$mode(), 42.0)
  expect_error(
    x$quantile(probs = c(0.25, NA, 0.75)),
    class = "probs_not_defined"
  )
  expect_error(
    x$quantile(probs = c(0.25, "A", 0.75)), class = "probs_not_numeric"
  )
  expect_error(
    x$quantile(probs = c(-0.25, 0.75)), class = "probs_out_of_range"
  )
  expect_equal(x$quantile(probs = 0.22), 42.0)
})

test_that("set and get function as expected", {
  x <- ConstModVar$new("y", "GBP", 42.0)
  expect_intol(x$get(), 42.0, 0.01)
  expect_error(x$set(TRUE), class = "what_not_character")
  expect_error(x$set("red"), class = "what_not_supported")
  expect_silent(x$set())
  expect_silent(x$set("expected"))
  expect_intol(x$get(), 42.0, 0.01)
  n <- 1000L
  S <- vector(mode = "numeric", length = n)
  for (i in seq_len(n)) {
    x$set()
    S[i] <- x$get()
  }
  expect_intol(mean(S), 42.0, 0.1)
  expect_intol(sd(S), 0.0, 0.01)
})

test_that("set('value') works as expected", {
  x <- ConstModVar$new("x", "GBP", 42.0)
  expect_identical(x$get(), 42.0)
  x$set("value", 7.0)
  expect_identical(x$get(), 7.0)
  # but the hyperparameter should be unchanged
  expect_identical(x$mean(), 42.0)
})
