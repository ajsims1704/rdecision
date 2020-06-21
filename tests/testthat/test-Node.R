

test_that("incorrect label types are rejected", {
  expect_silent(n1 <- Node$new("n1"))
  expect_equal(n1$label(), "n1")
  expect_error(Node$new(42), class="non-string_label")
  expect_error(Node$new(TRUE), class="non-string_label")
})
