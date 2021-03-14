

test_that("Essential stack operations are supported", {
  S <- Stack$new()
  S$push(3)
  S$push(4)
  expect_equal(S$size(),2)
  expect_equal(S$pop(),4)
  expect_equal(S$size(),1)
  expect_equal(S$pop(),3)
  expect_equal(S$size(),0)
  expect_error(S$pop(), class="underflow")
})

test_that("stacks of exotic objects are supported", {
  S <- Stack$new()
  S$push(Node$new("n1"))
  S$push(Node$new("n2"))
  expect_equal(S$size(),2)
  expect_identical(S$pop()$label(),"n2")
  expect_equal(S$size(),1)
  expect_identical(S$pop()$label(),"n1")
  expect_equal(S$size(),0)
})

test_that("stacks of lists are supported", {
  S <- Stack$new()
  S$push(list("A", "B", "C"))
  S$push(list("1",2,"3"))
  expect_equal(S$size(),2)
})

test_that("the stack can be extracted as a list", {
  S <- Stack$new()
  S$push(3)
  S$push(4)
  S$push(5)
  S$push(6)
  expect_setequal(S$as_list(), list(3,4,5,6))
  #
  S <- Stack$new()
  S$push(Node$new("n1"))
  S$push(Node$new("n2"))
  L <- S$as_list()
  expect_identical(L[[2]]$label(), "n2")
})
