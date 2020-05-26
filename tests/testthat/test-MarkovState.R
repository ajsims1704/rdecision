
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

test_that("cycle limits are set correctly", {
  s <- MarkovState$new("normal")
  expect_false(s$has_cycle_limit())
  s <- MarkovState$new("tunnel", cycleLimit=42)
  expect_true(s$has_cycle_limit())
  print("nnnn")
  print(s$get_cycle_limit())
  expect_true(all.equal(s$get_cycle_limit(), 42))
})
