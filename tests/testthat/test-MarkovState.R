
# tests of creating states
test_that("missing state names are rejected", {
  expect_error(MarkovState$new(name=NA), class="missing_state_name")
})

test_that("non-character state names are rejected", {
  expect_error(MarkovState$new(name=42), class="non-string_state_name")
})

test_that("invalid annual costs are rejected", {
  expect_error(MarkovState$new(name="Answer", cost="42"),
               class="non-numeric_annual_cost")
})

test_that("invalid utilities are rejected", {
  expect_error(MarkovState$new(name="Futile", utility=list(42)),
               class="non-numeric_utility")
  expect_error(MarkovState$new(name="Futile", utility=2),
               class="utility_out_of_range")
})

# tests of label, cost and utility
test_that("names are set and got", {
  s <- MarkovState$new("S1")
  expect_identical(s$get_name(), "S1")
})

test_that("costs are set and got", {
  s <- MarkovState$new("S1", cost=42)
  expect_equal(s$get_cost(), 42)
  s$set_cost(94)
  expect_equal(s$get_cost(), 94)
})

test_that("utilties are set and got", {
  s <- MarkovState$new("S1", utility=0.5)
  expect_equal(s$get_utility(), 0.5)
})
