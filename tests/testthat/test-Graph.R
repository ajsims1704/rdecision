
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

# tests of graph creation
test_that("incorrect node and edge types are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  expect_error(Graph$new(n1, list(e1)), class="non-list_vertices")
  expect_error(Graph$new(list(n1,n2), e1), class="non-list_edges")
  expect_error(Graph$new(list(n1,42), list(e1)), class="non-Node_vertex")
  expect_error(Graph$new(list(n1,n2), list(e1,42)), class="non-Edge_edge")
  expect_error(Graph$new(V=list(n1,n1), E=list(e1)), class="repeated_nodes")
})

# tests of simple graph properties
test_that("order and size are correct", {
  #
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  #
  V <- list(n1,n2)
  E <- list(e1)
  G <- Graph$new(V, E)
  expect_equal(G$order(), length(V))
  expect_equal(G$size(), length(E))
  #
  V <- list(n1)
  E <- list()
  G <- Graph$new(V, E)
  expect_equal(G$order(), 1)
  expect_equal(G$size(), 0)
})

# tests of vertex and edge properties
test_that("vertex and edge properties are correct", {
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1, n2)
  V <- list(n1,n2)
  E <- list(e1)
  G <- Graph$new(V, E)
  #
  expect_error(G$degree(42), class="non-Node_node")
  expect_error(G$degree(n3), class="not_in_graph")
  expect_error(G$degree(e1), class="non-Node_node")
  expect_equal(G$degree(n1), 1)
})

# Known examples
test_that("Fig 1.1.1 from Gross & Yellen (2003, ISBN 9780203490204) is replicated", {
  # the graph
  u <- Node$new("u")
  v <- Node$new("v")
  w <- Node$new("w")
  x <- Node$new("x")
  a <- Edge$new(u,v,"a")
  b <- Edge$new(v,u,"b")
  c <- Edge$new(x,x,"c")
  d <- Edge$new(x,w,"d")
  e <- Edge$new(x,v,"e")
  f <- Edge$new(w,v,"f")
  G <- Graph$new(V=list(u,v,w,x), E=list(a,b,c,d,e,f))
  # counts
  expect_equal(G$order(), 4)
  expect_equal(G$size(), 6)
  expect_equal(G$degree(u), 2)
  expect_equal(G$degree(v), 4)
  expect_equal(G$degree(w), 2)
  expect_equal(G$degree(x), 4)
  # neighbours
  expect_true(nodesetequal(G$neighbours(u),list(v)))
  expect_true(nodesetequal(G$neighbours(v),list(u,w,x)))
  expect_true(nodesetequal(G$neighbours(w),list(v,x)))
  expect_true(nodesetequal(G$neighbours(x),list(v,w)))
})

test_that("Simple tree is searched in DFS order", {
  # create graph
  A <- Node$new("A")
  B <- Node$new("B")
  C <- Node$new("C")
  D <- Node$new("D")
  E <- Node$new("E")
  F <- Node$new("F")
  e1 <- Edge$new(A,B)
  e2 <- Edge$new(A,C)
  e3 <- Edge$new(B,D)
  e4 <- Edge$new(C,E)
  e5 <- Edge$new(C,F)
  G <- Graph$new(V=list(A,B,C,D,E,F), E=list(e1,e2,e3,e4,e5))
  # search
  N <- G$DFS(A)
  # check order
  L <- paste(sapply(N, function(n) {n$get_label()}), sep='', collapse='')
  expect_true((L=="ABDCEF" || L=="ABDCFE" || L=="ACEFBD" || L=="ACFEBD"))
})
