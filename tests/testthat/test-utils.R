test_that("abortifnot raises an error condition as expected", {
  x <- 1
  expect_silent(abortifnot(x == 1, class = "no"))
  expect_error(abortifnot(x == 2, class = "no"), class = "no")
  expect_error(abortifnot(x == 2))
})

test_that("abortifnot behaves correctly with non-boolean conditions", {
  expect_error(abortifnot("42"))
  expect_error(abortifnot(42))
  expect_error(abortifnot(x))
})

test_that("as_value missing argument is detected correctly", {
  expect_error(as_value())
})

test_that("as_value identifies non ModVar objects", {
  # numeric scalar
  xi <- 42
  xo <- as_value(xi)
  expect_equal(xo, 42)
  # numeric vector
  xi <- c(1, 2, 3)
  xo <- as_value(xi)
  expect_equal(xo, c(1, 2, 3))
  # non-numeric scalar
  xi <- "42"
  xo <- as_value(xi)
  expect_true(is.na(xo))
  # non-numeric vector
  xi <- c("1", "2", "3")
  xo <- as_value(xi)
  expect_length(xo, 3)
  expect_true(all(is.numeric(xo)))
  expect_true(all(is.na(xo)))
  # R6 class but not ModVar    
  xi <- R6::R6Class()
  xo <- as_value(xi)
  expect_true(is.na(xo))
})

test_that("as_value detects ModVar and derived objects", {
  # scalar ModVars
  xi <- ModVar$new(description = "", units = "")
  xo <- as_value(xi)
  expect_true(is.na(xo))
  xi <- ConstModVar$new(description = "", units = "", const = 42)
  xo <- as_value(xi)
  expect_equal(xo, 42)
  xi <- BetaModVar$new(description = "", units = "", alpha = 10, beta = 10)
  xo <- as_value(xi)
  expect_intol(xo, 0.5, 0.01)
  n <- NormModVar$new(description = "", units = "", mu = 0, sigma = 1)
  xi <- ExprModVar$new(description = "", units = "", quo = rlang::quo(n * n))
  xo <- as_value(xi)
  expect_false(is.na(xo))
  # array of valid ModVars
  cv <- ConstModVar$new(description = "", units = "", const = 42)
  xi <- c(cv, cv)
  xo <- as_value(xi)
  expect_length(xo, 2)
  expect_equal(xo, c(42, 42))
  # mixed array
  xi <- c(42, cv, "42", cv)
  xo <- as_value(xi)
  expect_length(xo, 4)
  expect_equal(xo[1], 42)
  expect_equal(xo[2], 42)
  expect_true(is.na(xo[3]))
  expect_equal(xo[4], 42)
})

test_that("is_ModVar detects a missing argument", {
  expect_error(is_ModVar())
})

test_that("is_ModVar detects non ModVar objects", {
  # vector
  xi <- c(1, 2, 3)
  xo <- is_ModVar(xi)
  expect_length(xo, 3)
  expect_equal(sum(xo), 0)
  # scalars
  expect_false(is_ModVar(TRUE))
  expect_false(is_ModVar("ModVar"))
  x <- R6::R6Class()
  expect_false(is_ModVar(x))
})

test_that("is_ModVar detects ModVar and derived objects", {
  # scalar ModVars
  x <- ModVar$new(description = "", units = "")
  expect_true(is_ModVar(x))
  x <- ConstModVar$new(description = "", units = "", const = 42)
  expect_true(is_ModVar(x))
  x <- BetaModVar$new(description = "", units = "", alpha = 10, beta = 10)
  expect_true(is_ModVar(x))
  y <- ExprModVar$new(description = "", units = "", quo = rlang::quo(x * x))
  expect_true(is_ModVar(y))
  # vector (list) of ModVars
  x <- ConstModVar$new(description = "", units = "", const = 42)
  y <- is_ModVar(c(x, x))
  expect_length(y, 2)
  expect_equal(sum(y), 2)
  # mixed ModVars and non-ModVars
  x <- c(42, x, "a", x, TRUE)
  y <- is_ModVar(x)
  expect_length(y, 5)
  expect_equal(y, c(FALSE, TRUE, FALSE, TRUE, FALSE))
})
