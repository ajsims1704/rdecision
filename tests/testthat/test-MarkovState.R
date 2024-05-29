
# tests of creating states
test_that("missing state names are rejected", {
  expect_error(MarkovState$new(name = NA), class = "missing_state_name")
})

test_that("non-character state names are rejected", {
  expect_error(MarkovState$new(name = 42L), class = "non-string_state_name")
})

test_that("invalid annual costs are rejected", {
  expect_error(
    MarkovState$new(name = "Answer", cost = "42"),
    class = "invalid_annual_cost"
  )
  s <- MarkovState$new(name = "Answer")
  expect_error(
    s$set_cost(cost = "42"),
    class = "invalid_annual_cost"
  )
})

test_that("invalid utilities are rejected", {
  expect_error(
    MarkovState$new(name = "Futile", utility = list(42L)),
    class = "invalid_utility"
  )
  expect_error(
    MarkovState$new(name = "Futile", utility = 2.0),
    class = "invalid_utility"
  )
  s <- MarkovState$new(name = "Futile")
  expect_error(
    s$set_utility(utility = list(42L)),
    class = "invalid_utility"
  )
  expect_error(
    s$set_utility(utility = 2.0),
    class = "invalid_utility"
  )
})

# tests of label, cost and utility
test_that("names are set and got", {
  s <- MarkovState$new("S1")
  expect_identical(s$name(), "S1")
})

test_that("costs are set and got", {
  s <- MarkovState$new("S1", cost = 42.0)
  expect_identical(s$cost(), 42.0)
})

test_that("utilities are set and got", {
  s <- MarkovState$new("S1", utility = 0.5)
  expect_identical(s$utility(), 0.5)
  s <- MarkovState$new(name = "S1")
  expect_identical(s$utility(), 1.0)
  s$set_utility(0.5)
  expect_identical(s$utility(), 0.5)
})

# tests of costs and utilities that are ModVars
test_that("state cost and utility ModVars are recognised", {
  # create model variables
  c.dis <- GammaModVar$new("Cost", "GBP", shape = 10000.0, scale = 1.0 / 10.0)
  u.dis <- BetaModVar$new("Utility", "U", alpha = 10L, beta = 10L)
  # create states
  expect_silent(
    MarkovState$new(name = "Disabled", cost = c.dis)
  )
  expect_silent(
    MarkovState$new(name = "Disabled", utility = u.dis)
  )
})

test_that("ModVar state utilities whose values exceed 1 don't cause warnings", {
  u.beta <- BetaModVar$new("Utility", "U", alpha = 10L, beta = 10L)
  u.vw <- ExprModVar$new("vwell", "U", rlang::quo(2L + u.beta))
  u.beta$set("random")
  s.vw <- MarkovState$new(name = "Very Well", utility = u.vw)
  expect_silent(s.vw$utility())
  s <- MarkovState$new("vwell")
  expect_identical(s$utility(), 1.0)
  s$set_utility(u.vw)
  u.beta$set("expected")
  expect_identical(s$utility(), 2.5)
})

test_that("ModVars are identified and returned", {
  fortytwo <- ConstModVar$new("fortytwo", "GBP", 42L)
  poorly <- BetaModVar$new("poorly", "U", alpha = 10L, beta = 40L)
  # one modvar
  s <- MarkovState$new("Ill", cost = fortytwo, utility = 0.2)
  mv <- s$modvars()
  expect_length(mv, 1L)
  # two modvars
  s <- MarkovState$new("Ill", cost = fortytwo, utility = poorly)
  mv <- s$modvars()
  expect_length(mv, 2L)
  d <- vapply(mv, FUN.VALUE = "x", FUN = function(v) {
    return(v$description())
  })
  expect_setequal(d, c("fortytwo", "poorly"))
  expect_identical(s$cost(), 42.0)     # mean
  expect_identical(s$utility(), 0.2) # mean
})
