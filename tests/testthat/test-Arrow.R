

test_that("incorrect source and target are rejected", {
  n1 <- Node$new()
  expect_error(e <- Arrow$new(42, n1), class="non-Node_endpoint")
  expect_error(e <- Arrow$new(n1, 42), class="non-Node_endpoint")
})

test_that("incorrect labels are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  expect_error(a <- Arrow$new(n1, n2, TRUE), class="non-string_label")
})

test_that("arrow is defined correctly", {
  n1 <- Node$new()
  n2 <- Node$new()
  expect_silent(a <- Arrow$new(n1, n2, "a1"))
  expect_true(n1$is_same_node(a$source()))
  expect_true(n2$is_same_node(a$target()))
  expect_equal(a$label(), "a1")
})

test_that("base edge object is defined correctly", {
  n1 <- Node$new()
  n2 <- Node$new()
  expect_silent(a <- Arrow$new(n1, n2, "a1"))
  V <- a$endpoints()
  expect_identical(n1, V[[1]])
  expect_identical(n2, V[[2]])
})


