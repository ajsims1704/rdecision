

test_that("Essential stack operations are supported", {
  S <- Stack$new()
  S$push(3L)
  S$push(4L)
  expect_identical(S$size(), 2L)
  expect_identical(S$pop(), 4L)
  expect_identical(S$size(), 1L)
  expect_identical(S$pop(), 3L)
  expect_identical(S$size(), 0L)
  expect_error(S$pop(), class = "underflow")
})

test_that("stacks of exotic objects are supported", {
  S <- Stack$new()
  S$push(Node$new("n1"))
  S$push(Node$new("n2"))
  expect_identical(S$size(), 2L)
  expect_identical(S$pop()$label(), "n2")
  expect_identical(S$size(), 1L)
  expect_identical(S$pop()$label(), "n1")
  expect_identical(S$size(), 0L)
})

test_that("stacks of lists are supported", {
  S <- Stack$new()
  S$push(list("A", "B", "C"))
  S$push(list("1", 2L, "3"))
  expect_identical(S$size(), 2L)
})

test_that("the stack can be extracted as a list", {
  S <- Stack$new()
  S$push(3L)
  S$push(4L)
  S$push(5L)
  S$push(6L)
  expect_setequal(S$as_list(), list(3L, 4L, 5L, 6L))
  #
  S <- Stack$new()
  S$push(Node$new("n1"))
  S$push(Node$new("n2"))
  L <- S$as_list()
  expect_identical(L[[2L]]$label(), "n2")
})
