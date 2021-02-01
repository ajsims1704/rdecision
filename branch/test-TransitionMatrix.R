
# -----------------------------------------------------------------------------
# test state name operations
# -----------------------------------------------------------------------------
test_that("state names are the matrix dimnames", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  expect_equal(Ip$dimnames()[["from"]], names)
  expect_equal(Ip$dimnames()[["to"]], names)
})

test_that("coerced state names are allowed", {
  names <- c("Well", "Disabled", 42)
  Ip <- TransitionMatrix$new(names)
  expect_equal(Ip$dimnames()[["from"]], names)
  expect_equal(Ip$dimnames()[["to"]], names)
})

test_that("missing state names are rejected", {
  names <- list("Well", "Disabled", NA)
  expect_error(TransitionMatrix$new(names), class = "missing_state_name")
})

test_that("non string state names are rejected", {
  names <- list("Well", "Disabled", 42)
  expect_error(TransitionMatrix$new(names), class = "non-string_state_name")
})

test_that("repeated state names are rejected", {
  names <- list("Well", "Disabled", "Well")
  expect_error(TransitionMatrix$new(names), class = "non-unique_state_names")
})


# ----------------------------------------------------------------------------
# tests of setting transition rates
# ----------------------------------------------------------------------------
test_that("undefined state names are rejected", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  expect_error(Ip$set_rate(from="Well", to="Poorly", rate=0.5),
               class = "undefined_state_name")
  expect_error(Ip$set_rate(from="Poorly", to="Dead", rate=0.5),
               class = "undefined_state_name")
})

test_that("non-numeric rates are rejected", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  expect_error(Ip$set_rate(from="Well", to="Disabled", rate=TRUE),
               class = "non-numeric_rate")
  expect_error(Ip$set_rate(from="Well", to="Dead", rate="42"),
               class = "non-numeric_rate")
})

test_that("numeric rates within [0,1] are accepted", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  expect_silent(Ip$set_rate(from="Well", to="Disabled", rate=0.5))
})

test_that("numeric rates not within [0,1] are rejected", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  expect_error(Ip$set_rate(from="Well", to="Disabled", rate=-1), 
               class="numeric_rate_out_of_range")
  expect_error(Ip$set_rate(from="Well", to="Disabled", rate=42),
               class="numeric_rate_out_of_range")
})

test_that("missing rates are accepted", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  expect_silent(Ip$set_rate(from="Well", to="Disabled", rate=NA))
})

test_that("ModVar rates are accepted", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  V <- BetaModVar$new('A proportion', 'P', alpha=25, beta=100)
  expect_silent(Ip$set_rate(from="Well", to="Disabled", rate=V))
})

# ----------------------------------------------------------------------------
# tests of matrix properties
# ----------------------------------------------------------------------------
test_that("number of rows equals number of states", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  expect_equal(Ip$nrow(), length(names))
})

test_that("number of columns equals number of states", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  expect_equal(Ip$ncol(), length(names))
})

test_that("value is a numeric matrix", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  M <- Ip$value()
  expect_true(is.matrix(M))
})

test_that("initial value is the identity matrix", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  M <- Ip$value()
  IM <- diag(length(names))
  L <- all.equal(target=M, current=IM, check.attributes=FALSE)
  expect_true(L)
})

test_that("value is not computable if there is not one NA per row", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  Ip$set_rate(from="Well", to="Disabled", rate=NA)
  expect_error(Ip$value(), class="incorrect_NA_count")
  Ip$set_rate(from="Well", to="Disabled", rate=0)
  Ip$set_rate(from="Well", to="Well", rate=1)
  expect_error(Ip$value(), class="incorrect_NA_count")
})

