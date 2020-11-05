
test_that("missing, non-character or empty labels are rejected", {
  expect_error(DecisionNode$new(), class="missing_label")
  expect_error(DecisionNode$new(42), class="non-string_label")
  expect_error(DecisionNode$new(""), class="empty_label")
  expect_silent(DecisionNode$new("my node"))
})


