

# tests of digraph creation
test_that("incorrect node and edge types are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  expect_error(Digraph$new(n1, list(e1)), class="non-list_vertices")
  expect_error(Digraph$new(list(n1,n2), e1), class="non-list_edges")
  expect_error(Digraph$new(list(n1,42), list(e1)), class="non-Node_vertex")
  expect_error(Digraph$new(list(n1,n2), list(e1,42)), class="non-Edge_edge")
})

# tests of digraph properties
test_that("order and size are correct", {
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)

  V <- list(n1,n2)
  E <- list(e1)
  G <- Digraph$new(V, E)
  expect_equal(G$order(), length(V))
  expect_equal(G$size(), length(E))

  V <- list(n1)
  E <- list()
  G <- Digraph$new(V, E)
  expect_equal(G$order(), 1)
  expect_equal(G$size(), 0)
})

