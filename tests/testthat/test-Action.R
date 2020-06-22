

test_that("unlabelled actions are rejected", {
  n1 <- DecisionNode$new()
  n2 <- LeafNode$new("n2")
  expect_error(Action$new(n1,n2,42), class="non-string_label")
  expect_error(Action$new(n1,n2,""), class="empty_label")
  expect_silent(Action$new(n1,n2,"mychoice"))
})