test_that("incorrect label types are rejected", {
  n1 <- Node$new("n1")
  expect_identical(n1$label(), "n1")
  expect_error(Node$new(42L), class = "non-string_label")
  expect_error(Node$new(TRUE), class = "non-string_label")
})

test_that("modvars returns an empty list", {
  n1 <- Node$new("n1")
  mv <- n1$modvars()
  expect_length(mv, 0L)
})

test_that("node type is returned", {
  n1 <- Node$new()
  expect_identical(n1$type(), "Node")
})
