

test_that("incorrect node and edge types are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  expect_error(Graph$new(n1, list(e1)), class="non-list_vertices")
  expect_error(Graph$new(list(n1,n2), e1), class="non-list_edges")
  expect_error(Graph$new(list(n1,42), list(e1)), class="non-Node_vertex")
  expect_error(Graph$new(list(n1,n2), list(e1,42)), class="non-Edge_edge")
})