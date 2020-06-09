
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
  a1 <- Arrow$new(n1, n2)
  expect_error(Arborescence$new(n1, list(a1)), class="non-list_vertices")
  expect_error(Arborescence$new(list(n1,n2), a1), class="non-list_arrows")
  expect_error(Arborescence$new(list(n1,42), list(a1)), class="non-Node_vertex")
  expect_error(Arborescence$new(list(n1,n2), list(a1,42)), class="non-Arrow_edge")
})
