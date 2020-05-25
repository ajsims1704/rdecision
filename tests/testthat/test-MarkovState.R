
# -----------------------------------------------------------------------------
# tests of creating new states
# -----------------------------------------------------------------------------

test_that("missing state names are rejected", {
  expect_error(MarkovState$new(name=NA), class="missing_state_name")
})

test_that("non-character state names are rejected", {
  expect_error(MarkovState$new(name=42), class="non-string_state_name")
})
