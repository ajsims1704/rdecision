# tests of abortif
test_that("abortif accepts no expressions", {
  expect_silent(abortif(class = "no"))
  expect_silent(abortif(class = "no", message = "none"))
  expect_silent(abortif())
})

test_that("abortif behaves correctly with non-boolean conditions", {
  expect_error(abortif("42"))
  expect_error(abortif(42L))
  expect_error(abortif("x"))
  expect_error(abortif("x", FALSE))
})

test_that("abortif raises an error condition with a single expression", {
  x <- 1L
  expect_silent(abortif(x > 1L, class = "no"))
  expect_error(abortif(x == 1L, class = "no"), class = "no")
  expect_error(abortif(x == 1L))
  x <- NA_real_
  expect_error(abortif(is.na(x), class = "no"), class = "no")
})

test_that("abortif raises an error condition with > 1 expression", {
  x <- 1L
  y <- "apple"
  z <- NA_real_
  expect_silent(
    abortif(
      x > 1L,
      y %in% c("orange", "pear"),
      class = "no"
    )
  )
  expect_error(
    abortif(
      x == 1L,
      y %in% c("orange", "pear"),
      class = "no"
    )
  )
  expect_error(
    abortif(
      x > 1L,
      y %in% c("orange", "pear", "apple")
    )
  )
  expect_error(
    abortif(
      x == 1L,
      y %in% c("orange", "pear"),
      class = "no"
    ),
    class = "no"
  )
  expect_error(
    abortif(
      x > 2L,
      y %in% c("banana", "pear"),
      is.na(z),
      class = "no",
      message = "no NAs"
    ),
    class = "no"
  )
})

test_that("abortif error message with >1 expression is correct", {
  f <- function(x) {
    abortif(FALSE, x > 2L, FALSE, message = "failed inequality condition")
  }
  expect_error(
    f(3L),
    regexp = "failed inequality condition x > 2L is not FALSE",
    class = NULL
  )
})

# tests of abortifnot
test_that("abortifnot accepts no expressions", {
  expect_silent(abortifnot(class = "no"))
  expect_silent(abortifnot(class = "no", message = "none"))
  expect_silent(abortifnot())
})

test_that("abortifnot behaves correctly with non-boolean conditions", {
  expect_error(abortifnot("42"))
  expect_error(abortifnot(42L))
  expect_error(abortifnot("x"))
  expect_error(abortifnot("x", TRUE))
})

test_that("abortifnot raises an error condition with a single expression", {
  x <- 2L
  expect_silent(abortifnot(x > 1L, class = "no"))
  expect_error(abortifnot(x == 1L, class = "no"), class = "no")
  expect_error(abortifnot(x == 1L))
  expect_silent(abortifnot(is.character("random")))
})

test_that("abortifnot raises an error condition with > 1 expression", {
  x <- 2L
  y <- "orange"
  expect_silent(
    abortifnot(
      x > 1L,
      y %in% c("orange", "pear"),
      class = "no"
    )
  )
  expect_error(
    abortifnot(
      x == 1L,
      y %in% c("orange", "pear"),
      class = "no"
    )
  )
  expect_error(
    abortifnot(
      x > 1L,
      y %in% c("pear", "apple")
    )
  )
  expect_error(
    abortifnot(
      x == 1L,
      y %in% c("orange", "pear"),
      class = "no"
    ),
    class = "no"
  )
})

test_that("abortifnot error message with >1 expression is correct", {
  f <- function(x) {
    abortifnot(TRUE, x > 2L, TRUE, message = "failed inequality condition")
  }
  expect_error(
    f(1L),
    regexp = "failed inequality condition x > 2L is not TRUE",
    class = NULL
  )
})

# tests of as_numeric
test_that("as_numeric missing argument is detected correctly", {
  expect_error(as_numeric())
})

test_that("as_numeric identifies non ModVar objects", {
  # numeric scalar
  xi <- 42.0
  xo <- as_numeric(xi)
  expect_intol(xo, 42.0, 0.01)
  # numeric vector with integers, which should be coerced to numeric
  xi <- c(1L, 2L, 3L)
  xo <- as_numeric(xi)
  expect_identical(xo, c(1.0, 2.0, 3.0))
  # numeric vector with numerics
  xi <- c(1.0, 2.0, 3.0)
  xo <- as_numeric(xi)
  expect_identical(xo, c(1.0, 2.0, 3.0))
  # non-numeric scalar
  xi <- "42"
  xo <- as_numeric(xi)
  expect_true(is.na(xo))
  # non-numeric vector
  xi <- c("1", "2", "3")
  xo <- as_numeric(xi)
  expect_length(xo, 3L)
  expect_true(all(is.numeric(xo)))
  expect_true(all(is.na(xo)))
  # R6 class but not ModVar
  xi <- R6::R6Class()
  xo <- as_numeric(xi)
  expect_true(is.na(xo))
})

test_that("as_numeric detects ModVar and derived objects", {
  # scalar ModVars
  xi <- ModVar$new(description = "", units = "")
  xo <- as_numeric(xi)
  expect_true(is.na(xo))
  xi <- ConstModVar$new(description = "", units = "", const = 42.0)
  xo <- as_numeric(xi)
  expect_intol(xo, 42.0, 0.01)
  xi <- BetaModVar$new(description = "", units = "", alpha = 10L, beta = 10L)
  xo <- as_numeric(xi)
  expect_intol(xo, 0.5, 0.01)
  n <- NormModVar$new(description = "", units = "", mu = 0.0, sigma = 1.0)
  xi <- ExprModVar$new(description = "", units = "", quo = rlang::quo(n * n))
  xo <- as_numeric(xi)
  expect_false(is.na(xo))
  # array of valid ModVars
  cv <- ConstModVar$new(description = "", units = "", const = 42.0)
  xi <- c(cv, cv)
  xo <- as_numeric(xi)
  expect_length(xo, 2L)
  expect_equal(xo, c(42.0, 42.0))
  # mixed array
  xi <- c(42L, cv, "42", cv)
  xo <- as_numeric(xi)
  expect_length(xo, 4L)
  expect_intol(xo[[1L]], 42.0, 0.01)
  expect_intol(xo[[2L]], 42.0, 0.01)
  expect_true(is.na(xo[[3L]]))
  expect_intol(xo[[4L]], 42.0, 0.01)
})

# tests of gbp
test_that("gbp returns in currency format", {
  x <- list(42L, 2L, 0.002, 4.567, "a")
  agbp <- gbp(x, p = TRUE)
  expect_identical(agbp, c("42.00", "2.00", "0.00", "4.57", "NA"))
})

# tests of class type tests
test_that("is_class detects missing arguments", {
  expect_error(is_class())
  expect_error(is_class(42L))
  expect_error(is_class(what = "ModVar"))
})

test_that("is_class supports vectorized arguments", {
  n1 <- DecisionNode$new("n1")
  n2 <- ChanceNode$new()
  n3 <- LeafNode$new("n3")
  e1 <- Action$new(source = n1, target = n2, label = "e1")
  e2 <- Reaction$new(source = n2, target = n3)
  l <- is_class(x = list(n1, n2, n3, e1, e2), what = "Node")
  expect_identical(l, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  l <- is_class(x = list(n1, n2, n3, e1, e2), what = "Edge")
  expect_identical(l, c(FALSE, FALSE, FALSE, TRUE, TRUE))
  l <- is_class(x = list(n1, n2, n3, e1, e2), what = "DecisionNode")
  expect_identical(l, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  l <- is_class(x = list(n1, n2, n3, e1, e2), what = "Action")
  expect_identical(l, c(FALSE, FALSE, FALSE, TRUE, FALSE))
  l <- is_class(
    x = list(n1, n2, n3, e1, e2),
    what = c("DecisionNode", "Action")
  )
  expect_identical(l, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  l <- is_class(
    x = list(n1, n2, n3, e1, e2),
    what = c("DecisionNode", "Node")
  )
  expect_identical(l, c(TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("is_ModVar detects non ModVar objects", {
  # vector
  xi <- c(1.0, 2.0, 3.0)
  xo <- is_ModVar(xi)
  expect_length(xo, 3L)
  expect_intol(sum(xo), 0.0, 0.01)
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
  x <- ConstModVar$new(description = "", units = "", const = 42.0)
  expect_true(is_ModVar(x))
  x <- BetaModVar$new(description = "", units = "", alpha = 10.0, beta = 10.0)
  expect_true(is_ModVar(x))
  y <- ExprModVar$new(description = "", units = "", quo = rlang::quo(x * x))
  expect_true(is_ModVar(y))
  # vector (list) of ModVars
  x <- ConstModVar$new(description = "", units = "", const = 42.0)
  y <- is_ModVar(c(x, x))
  expect_length(y, 2L)
  expect_identical(sum(y), 2L)
  # mixed ModVars and non-ModVars
  x <- c(42.0, x, "a", x, TRUE)
  y <- is_ModVar(x)
  expect_length(y, 5L)
  expect_identical(y, c(FALSE, TRUE, FALSE, TRUE, FALSE))
})

test_that("is_Arrow detects objects of type Arrow", {
  s <- Node$new()
  t <- Node$new()
  a <- Arrow$new(s, t)
  expect_false(is_Arrow(s))
  expect_true(is_Arrow(a))
})

test_that("tornado plot checks arguments as expected", {
  # to is not a data frame
  to <- 42L
  expect_error(
    tornado_plot(to = to, outcome_mean = 0.0),
    class = "invalid_parameter"
  )
  # data frame with missing column
  to <- data.frame(
    Description = "p1",
    Units = "%",
    LL = 25.0,
    UL = 29.0,
    outcome.min = 100.0,
    stringsAsFactors = FALSE
  )
  expect_error(
    tornado_plot(to = to, outcome_mean = 0.0),
    class = "invalid_parameter"
  )
  # data frame with column of incorrect type
  to <- data.frame(
    Description = "p1",
    Units = "%",
    LL = 25.0,
    UL = 29.0,
    outcome.min = 100.0,
    outcome.max = FALSE,
    stringsAsFactors = FALSE
  )
  expect_error(
    tornado_plot(to = to, outcome_mean = 0.0),
    class = "invalid_parameter"
  )
  # data frame with an NA
  to <- data.frame(
    Description = "p1",
    Units = "%",
    LL = 25.0,
    UL = NA_real_,
    outcome.min = 100.0,
    outcome.max = 120.0,
    stringsAsFactors = FALSE
  )
  expect_error(
    tornado_plot(to = to, outcome_mean = 0.0),
    class = "invalid_parameter"
  )
  # valid data frame
  to <- data.frame(
    Description = "p1",
    Units = "%",
    LL = 25.0,
    UL = 27.0,
    outcome.min = 100.0,
    outcome.max = 120.0,
    stringsAsFactors = FALSE
  )
  grDevices::pdf(file = NULL)
  expect_no_condition(
    tornado_plot(to = to, outcome_mean = 0.0)
  )
  grDevices::dev.off()
})


test_that("vigtable behaves as expected", {
  # create test data frame
  df <- data.frame(
    Strategy = c("Intervention", "Comparator"),
    Cost = c(2700.0, 2500.0),
    LYG = c(0.2345, 0.00026),
    QALY = c(6.5, 6.2),
    stringsAsFactors = FALSE
  )
  # test arguments
  expect_error(vigtable(x = 3L))
  # create text for table
  expect_output(vigtable(df))
})
