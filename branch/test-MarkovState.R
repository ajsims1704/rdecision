
# -----------------------------------------------------------------------------
# tests of creating new states
# -----------------------------------------------------------------------------
test_that("missing state names are rejected", {
  expect_error(MarkovState$new(name=NA), class="missing_state_name")
})

test_that("non-character state names are rejected", {
  expect_error(MarkovState$new(name=42), class="non-string_state_name")
})

test_that("invalid cycle limits are rejected", {
  expect_error(MarkovState$new(name="Tunnel", cycleLimit="4"), 
               class="non-numeric_cycle_limit")
  expect_error(MarkovState$new(name="Tunnel", cycleLimit=1.5), 
               class="non-integer_cycle_limit")
})

test_that("invalid annual costs are rejected", {
  expect_error(MarkovState$new(name="Answer", annualCost="42"),
               class="non-numeric_annual_cost")
})

test_that("invalid entry costs are rejected", {
  expect_error(MarkovState$new(name="Logical", entryCost=FALSE),
               class="non-numeric_entry_cost")
})

test_that("invalid utilities are rejected", {
  expect_error(MarkovState$new(name="Futile", utility=list(42)),
               class="non-numeric_utility")
  expect_error(MarkovState$new(name="Futile", utility=2),
               class="utility_out_of_range")
})

test_that("cycle limits are set correctly", {
  s <- MarkovState$new("normal")
  expect_false(s$has_cycle_limit())
  s <- MarkovState$new("tunnel", cycleLimit=42)
  expect_true(s$has_cycle_limit())
  expect_true(all.equal(s$get_cycle_limit(), 42))
})

test_that("absorbing status is processed", {
  expect_error(MarkovState$new("Alive", absorbing=1),
               class="non-logical_absorbing_status")
  expect_error(MarkovState$new("Alive", cycleLimit=42, absorbing=TRUE),
               class="temporary_absorbing_conflict")
  s <- MarkovState$new("Alive")
  expect_false(s$is_absorbing())
  s <- MarkovState$new("Dead", absorbing=TRUE)
  expect_true(s$is_absorbing())
})
