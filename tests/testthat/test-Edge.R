

test_that("incorrect source and target are rejected", {
  n1 <- Node$new()
  expect_error(e <- Edge$new(42, n1), class="non-Node_source")
  expect_error(e <- Edge$new(n1, 42), class="non-Node_target")
})

test_that("incorrect labels are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  expect_error(e <- Edge$new(n1, n2, TRUE), 
               class="non-string_label")
})

test_that("node is defined correctly", {
  n1 <- Node$new()
  n2 <- Node$new()
  expect_silent(e <- Edge$new(n1, n2, "e1"))
  expect_true(n1$is_same_node(e$get_source()))
  expect_true(n2$is_same_node(e$get_target()))
  expect_equal(e$get_label(), "e1")
})

