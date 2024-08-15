
test_that("incorrect endpoints are rejected", {
  n1 <- Node$new()
  expect_error(Edge$new(42L, n1), class = "non-Node_endpoint")
  expect_error(Edge$new(n1, 42L), class = "non-Node_endpoint")
})

test_that("incorrect labels are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  expect_error(Edge$new(n1, n2, TRUE), class = "non-string_label")
})

test_that("edge is defined correctly", {
  n1 <- Node$new()
  n2 <- Node$new()
  e <- Edge$new(n1, n2, "e1")
  V <- e$endpoints()
  expect_identical(n1, V[[1L]])
  expect_identical(n2, V[[2L]])
  expect_identical(e$label(), "e1")
})

test_that("an edge identifies itself", {
  n1 <- Node$new()
  n2 <- Node$new()
  e <- Edge$new(n1, n2, "e1")
  expect_true(e$is_same_edge(e))
})

test_that("modvars returns an empty list", {
  n1 <- Node$new("n1")
  n2 <- Node$new()
  e <- Edge$new(n1, n2, "e1")
  mv <- e$modvars()
  expect_length(mv, 0L)
})
