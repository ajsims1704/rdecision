
# setequal function for Nodes
nodesetequal <- function(A,B) {
  AinB <- all(sapply(A, function(a) {
    return(any(sapply(B, function(b){return(b$is_same_node(a))})))
  }))  
  BinA <- all(sapply(B, function(b) {
    return(any(sapply(A, function(a){return(a$is_same_node(b))})))
  }))
  return(AinB & BinA)
}

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

# tests of simple digraph properties
test_that("order and size are correct", {
  #
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  #
  V <- list(n1,n2)
  E <- list(e1)
  G <- Digraph$new(V, E)
  expect_equal(G$order(), length(V))
  expect_equal(G$size(), length(E))
  #
  V <- list(n1)
  E <- list()
  G <- Digraph$new(V, E)
  expect_equal(G$order(), 1)
  expect_equal(G$size(), 0)
})

# test 4 node graph with a cycle
test_that("4 node graph with cycle is correct", {
  # construct graph
  a <- Node$new('a')
  b <- Node$new('b')
  c <- Node$new('c')
  d <- Node$new('d')
  e <- Node$new('e')
  e1 <- Edge$new(a,b)
  e2 <- Edge$new(b,c)
  e3 <- Edge$new(c,a)
  e4 <- Edge$new(a,d)
  V <- list(a, b, c, d)
  E <- list(e1, e2, e3, e4)
  G <- Digraph$new(V, E)
  #
  expect_equal(G$order(), 4)
  expect_equal(G$size(), 4)
  #
  expect_error(G$direct_successors(42), class="non-Node_node")
  expect_error(G$direct_successors(e), class="node_not_in_tree")
  expect_true(nodesetequal(G$direct_successors(a), list(b,d)))
  expect_true(nodesetequal(G$direct_successors(c), list(a)))
  expect_true(length(G$direct_successors(d))==0)
  #
  expect_error(G$direct_predecessors(42), class="non-Node_node")
  expect_error(G$direct_predecessors(e), class="node_not_in_tree")
  expect_true(nodesetequal(G$direct_predecessors(a), list(c)))
  expect_true(nodesetequal(G$direct_predecessors(c), list(b)))
  expect_true(nodesetequal(G$direct_predecessors(d), list(a)))
  # 
  expect_error(G$DFS(42), class="non-Node_node")
  expect_error(G$DFS(e), class="node_not_in_tree")
  expect_true(nodesetequal(G$DFS(a), list(a,b,c,d)))
  expect_true(nodesetequal(G$DFS(b), list(a,b,c,d)))
  expect_true(nodesetequal(G$DFS(d), list(d)))
})
