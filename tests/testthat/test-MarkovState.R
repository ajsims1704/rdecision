
# tests of creating states
test_that("missing state names are rejected", {
  expect_error(MarkovState$new(name=NA), class="missing_state_name")
})

test_that("non-character state names are rejected", {
  expect_error(MarkovState$new(name=42), class="non-string_state_name")
})

test_that("invalid annual costs are rejected", {
  expect_error(
    MarkovState$new(name="Answer", cost="42"),
    class="invalid_annual_cost"
  )
})

test_that("invalid utilities are rejected", {
  expect_error(
    MarkovState$new(name = "Futile", utility = list(42)),
    class = "invalid_utility"
  )
  expect_error(
    MarkovState$new(name = "Futile", utility = 2),
    class = "invalid_utility")
})

# tests of label, cost and utility
test_that("names are set and got", {
  s <- MarkovState$new("S1")
  expect_identical(s$name(), "S1")
})

test_that("costs are set and got", {
  s <- MarkovState$new("S1", cost=42)
  expect_equal(s$cost(), 42)
})

test_that("utilties are set and got", {
  s <- MarkovState$new("S1", utility=0.5)
  expect_equal(s$utility(), 0.5)
})

# tests of costs and utilities that are ModVars
test_that("state cost and utility ModVars are recognised", {
  # create model variables
  c.dis <- GammaModVar$new("Cost", "GBP", shape = 10000, scale = 1/10)
  u.dis <- BetaModVar$new("Utility", "U", alpha = 10, beta = 10) 
  # create states
  expect_silent(
    MarkovState$new(name = "Disabled", cost = c.dis)
  )
  expect_silent(
    MarkovState$new(name = "Disabled", utility = u.dis)
  )
})

test_that("ModVar state utilities whose values exceed 1 cause warnings", {
  u.beta <- BetaModVar$new("Utility", "U", alpha=10, beta=10) 
  u.vw <- ExprModVar$new("vwell", "U", rlang::quo(2+u.beta))
  u.beta$set("random")
  expect_silent(
    s.vw <- MarkovState$new(name="Very Well", utility=u.vw)
  )
  expect_silent(
    u <- s.vw$utility()
  )
})

test_that("ModVars are identified and returned", {
  fortytwo <- ConstModVar$new("fortytwo", "GBP", 42)
  poorly <- BetaModVar$new("poorly", "U", alpha=10, beta=40)
  # one modvar
  s <- MarkovState$new("Ill", cost=fortytwo, utility=0.2)
  mv <- s$modvars()
  expect_equal(length(mv),1)
  # two modvars
  s <- MarkovState$new("Ill", cost=fortytwo, utility=poorly)
  mv <- s$modvars()
  expect_length(mv,2)
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_setequal(d, c("fortytwo", "poorly"))
  expect_equal(s$cost(), 42)     # mean
  expect_equal(s$utility(), 0.2) # mean
})
