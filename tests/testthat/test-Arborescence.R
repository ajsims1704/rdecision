
# setequal function for Nodes
nodesetequal <- function(A,B) {
  AinB <- all(sapply(A, function(a) {
    return(any(sapply(B, function(b){identical(a,b)})))
  }))  
  BinA <- all(sapply(B, function(b) {
    return(any(sapply(A, function(a){identical(a,b)})))
  }))
  return(AinB & BinA)
}

# tests of arborescence creation
test_that("incorrect node and edge types are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  a1 <- Arrow$new(n1, n2)
  expect_error(Arborescence$new(n1, list(a1)), class="non-list_vertices")
  expect_error(Arborescence$new(list(n1,n2), a1), class="non-list_arrows")
  expect_error(Arborescence$new(list(n1,42), list(a1)), class="non-Node_vertex")
  expect_error(Arborescence$new(list(n1,n2), list(a1,42)), class="non-Arrow_edge")
})

test_that("graphs that are not trees are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Arrow$new(n1,n2)
  e2 <- Arrow$new(n1,n3)
  expect_silent(Arborescence$new(V=list(n1,n2,n3), A=list(e1,e2)))
  #
  e3 <- Arrow$new(n2,n3)
  expect_error(Arborescence$new(V=list(n1,n2,n3), A=list(e1,e2,e3)),
               class="not_arborescence")
})

test_that("graphs that are not arborescences are rejected", {
  # out tree (single root)
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Arrow$new(n1,n2)
  e2 <- Arrow$new(n1,n3)
  expect_silent(G <- Arborescence$new(V=list(n1,n2,n3),A=list(e1,e2)))
  # in tree (2 roots)
  e1 <- Arrow$new(n2,n1)
  e2 <- Arrow$new(n3,n1)
  expect_error(G <- Arborescence$new(V=list(n1,n2,n3), A=list(e1,e2)), 
               class="not_arborescence")
  # tree with zero roots
  n4 <- Node$new()
  e1 <- Arrow$new(n2,n1)
  e2 <- Arrow$new(n2,n3)
  e3 <- Arrow$new(n2,n4)
  expect_error(G <- Arborescence$new(V=list(n1,n2,n3,n4), A=list(e1,e2,e3)), 
               class="not_arborescence")
})
