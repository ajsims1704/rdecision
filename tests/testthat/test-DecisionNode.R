
test_that("missing, non-character or empty labels are rejected", {
  expect_error(DecisionNode$new(), class = "missing_label")
  expect_error(DecisionNode$new(42L), class = "non-string_label")
  expect_error(DecisionNode$new(""), class = "empty_label")
  expect_silent(DecisionNode$new("my node"))
})

test_that("syntactically invalid labels are corrected", {
  d <- DecisionNode$new("my_label")
  expect_identical(d$label(), "my_label")
  d <- DecisionNode$new("my label")
  expect_identical(d$label(), "my.label")
  d <- DecisionNode$new("2_label")
  expect_identical(d$label(), "X2_label")
  d <- DecisionNode$new("2.label")
  expect_identical(d$label(), "X2.label")
})
