
test_that("missing, non-character or empty labels are rejected", {
  expect_error(LeafNode$new(), class="missing_label")
  expect_error(LeafNode$new(42), class="non-string_label")
  expect_error(LeafNode$new(""), class="empty_label")
  expect_silent(LeafNode$new("my node"))
})
