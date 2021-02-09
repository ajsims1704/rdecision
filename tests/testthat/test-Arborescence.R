
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
  expect_error(Arborescence$new(list(n1,n2), list(a1,42)), 
               class="non-Arrow_edge")
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

test_that("parent, sibling and drawing functions are correct", {
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
  T <- Arborescence$new(
    V=list(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O), 
    A=list(eOE,eOF,eON,eEA,eED,eDB,eDC,eNG,eNM,eMH,eMI,eMJ,eMK,eML)
  )  
  expect_equal(T$order(),15)
  # check siblings
  expect_equal(length(T$siblings(O)),0)
  expect_true(nodesetequal(T$siblings(E), list(F,N)))
  expect_true(nodesetequal(T$siblings(A), list(D)))
  expect_true(nodesetequal(T$siblings(J), list(H,I,K,L)))
  # check the node coordinates
  XY <- T$postree()
  expect_equal(nrow(XY),T$order())
  rownames(XY) <- LETTERS[1:T$order()]
  expect_equal(XY["O","x"],13.5)
  expect_equal(XY["E","x"],3)
  expect_equal(XY["A","x"],0)
  expect_equal(XY["D","x"],6)
  expect_equal(XY["B","x"],3)
  expect_equal(XY["C","x"],9)
  expect_equal(XY["F","x"],13.5,tolerance=0.1)
  expect_equal(XY["N","x"],24)
  expect_equal(XY["G","x"],21)
  expect_equal(XY["M","x"],27)
  expect_equal(XY["H","x"],15)
  expect_equal(XY["I","x"],21)
  expect_equal(XY["J","x"],27)
  expect_equal(XY["K","x"],33)
  expect_equal(XY["L","x"],39)
  expect_equal(XY["O","y"],0)
  expect_equal(XY["E","y"],1)
  expect_equal(XY["F","y"],1)
  expect_equal(XY["N","y"],1)
  expect_equal(XY["A","y"],2)
  expect_equal(XY["D","y"],2)
  expect_equal(XY["G","y"],2)
  expect_equal(XY["M","y"],2)
  expect_equal(XY["B","y"],3)
  expect_equal(XY["C","y"],3)
  expect_equal(XY["H","y"],3)
  expect_equal(XY["I","y"],3)
  expect_equal(XY["J","y"],3)
  expect_equal(XY["K","y"],3)
  expect_equal(XY["L","y"],3)
  
})
