
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
  # out tree (single root) with nodes and edges in different order
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Arrow$new(n1,n2)
  e2 <- Arrow$new(n1,n3)
  G <- Digraph$new(V=list(n2,n3,n1),A=list(e1,e2))
  expect_true(G$is_weakly_connected())
  expect_true(G$is_tree())
  expect_silent(G <- Arborescence$new(V=list(n2,n3,n1),A=list(e1,e2)))
  # in tree (2 roots)
  e1 <- Arrow$new(n2,n1)
  e2 <- Arrow$new(n3,n1)
  expect_error(G <- Arborescence$new(V=list(n1,n2,n3), A=list(e1,e2)), 
               class="not_arborescence")
})

test_that("drawing functions are correct", {
  # create the tree using example from Walker (1989), fig 12
  O <- Node$new("O")
  E <- Node$new("E")
  F <- Node$new("F")
  N <- Node$new("N")
  A <- Node$new("A")
  D <- Node$new("D")
  G <- Node$new("G")
  M <- Node$new("M")
  B <- Node$new("B")
  C <- Node$new("C")
  H <- Node$new("H")
  I <- Node$new("I")
  J <- Node$new("J")
  K <- Node$new("K")
  L <- Node$new("L")
  eOE <- Arrow$new(O,E)  
  eOF <- Arrow$new(O,F)  
  eON <- Arrow$new(O,N)  
  eEA <- Arrow$new(E,A)  
  eED <- Arrow$new(E,D)  
  eDB <- Arrow$new(D,B)  
  eDC <- Arrow$new(D,C)  
  eNG <- Arrow$new(N,G)  
  eNM <- Arrow$new(N,M)  
  eMH <- Arrow$new(M,H)  
  eMI <- Arrow$new(M,I)  
  eMJ <- Arrow$new(M,J)  
  eMK <- Arrow$new(M,K)  
  eML <- Arrow$new(M,L)  
  A <- Arborescence$new(
    V=list(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O), 
    A=list(eOE,eOF,eON,eEA,eED,eDB,eDC,eNG,eNM,eMH,eMI,eMJ,eMK,eML)
  )  
  expect_equal(A$order(),15)
  # check the node coordinates
  XY <- A$position_tree()
  expect_equal(nrow(XY),A$order())
})
