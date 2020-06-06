

test_that("incorrect edge types are rejected", {
  C1 <- Node$new()
  C2 <- Node$new()
  ParentNode <- R6::R6Class(
    classname = "ParentNode",
    inherit = Node,
    public=list(
      initialize=function() {},
      public_add_edge=function(e) {private$addArrow(e)}
    )
  )
  P <- ParentNode$new()
  expect_error(P$public_add_edge(42), class="non-Arrow_edge")
  e <- Arrow$new(P, C1, "edge 1")
  expect_silent(P$public_add_edge(e))
  expect_error(P$public_add_edge(e))
})
