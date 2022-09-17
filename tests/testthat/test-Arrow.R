

test_that("incorrect source and target are rejected", {
  n1 <- Node$new()
  expect_error(e <- Arrow$new(42L, n1), class="non-Node_endpoint")
  expect_error(e <- Arrow$new(n1, 42L), class="non-Node_endpoint")
})

test_that("argument name matching supports old names", {
  ns <- Node$new()
  nt <- Node$new()
  expect_silent(Arrow$new(source = ns, target = nt, label = "x"))
  expect_silent(Arrow$new(source = ns, target_node = nt, label = "x"))
  expect_silent(Arrow$new(source_node = ns, target = nt, label = "x"))
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
  expect_identical(n1, a$source())
  expect_identical(n2, a$target())
  expect_identical(a$label(), "a1")
})

test_that("base edge object is defined correctly", {
  n1 <- Node$new()
  n2 <- Node$new()
  expect_silent(a <- Arrow$new(n1, n2, "a1"))
  V <- a$endpoints()
  expect_identical(n1, V[[1L]])
  expect_identical(n2, V[[2L]])
})
