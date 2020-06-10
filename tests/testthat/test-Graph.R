
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
  #
  expect_error(Graph$new(n1, list(e1)), class="non-list_vertices")
  expect_error(Graph$new(list(n1,n2), e1), class="non-list_edges")
  expect_error(Graph$new(list(n1,42), list(e1)), class="non-Node_vertex")
  expect_error(Graph$new(list(n1,n2), list(e1,42)), class="non-Edge_edge")
  expect_error(Graph$new(V=list(n1,n1), E=list(e1)), class="repeated_nodes")
  expect_error(Graph$new(V=list(n1,n2), E=list(e1,e1)), class="repeated_edges")
  #
  n3 <- Node$new()
  e2 <- Edge$new(n1,n3)
  expect_error(Graph$new(V=list(n1,n2),E=list(e1,e2)), class="not_in_graph")
  #
  n1 <- Node$new("n1")
  n2 <- Node$new("n1")
  n3 <- Node$new()
  e1 <- Edge$new(n1,n2,"e1")
  expect_error(Graph$new(V=list(n1,n2),E=list(e1)), class="repeated_node_labels")
  expect_error(Graph$new(V=list(n1,n2,n3),E=list(e1)), class="repeated_node_labels")
  #
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1,n2,"e1")
  e2 <- Edge$new(n2,n1,"e1")
  e3 <- Edge$new(n1,n2)
  expect_error(Graph$new(V=list(n1,n2),E=list(e1,e2)), class="repeated_edge_labels")
  expect_error(Graph$new(V=list(n1,n2),E=list(e1,e2,e3)), class="repeated_edge_labels")
})

# tests of simple graph properties
test_that("basic graph properties are set and got", {
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
test_that("vertex and edge properties are set and got", {
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1, n2)
  G <- Graph$new(V=list(n1,n2), E=list(e1))
  #
  expect_error(G$has_element(42), class="incorrect_element_type")
  expect_true(G$has_element(n1))
  expect_true(G$has_vertex(n2))
  expect_true(G$has_element(e1))
  expect_true(G$has_edge(e1))
  expect_false(G$has_element(n3))
  #
  expect_equal(G$element_index(n1),1)
  expect_equal(G$element_index(n2),2)
  expect_equal(G$element_index(e1),1)
  #
  expect_error(G$degree(42), class="incorrect_element_type")
  expect_error(G$degree(n3), class="not_in_graph")
  expect_error(G$degree(e1), class="incorrect_element_type")
  expect_equal(G$degree(n1), 1)
})

# tests of adjacency matrix
test_that("adjacency matrix has correct properties", {
  # empty graph
  G <- Graph$new(V=list(),E=list())
  expect_error(G$adjacency_matrix(42), class="non-logical_binary")
  A <- G$adjacency_matrix()
  expect_true(is.matrix(A))
  expect_equal(nrow(A),0)
  expect_equal(ncol(A),0)
  # trivial graph
  n1 <- Node$new()
  G <- Graph$new(V=list(n1),E=list())
  A <- G$adjacency_matrix()
  expect_true(is.matrix(A))
  expect_equal(nrow(A),1)
  expect_equal(ncol(A),1)
  expect_equal(A[1,1],0)
  # named nodes
  n1 <- Node$new("n1")
  n2 <- Node$new()
  e1 <- Edge$new(n1,n2)
  G <- Graph$new(V=list(n1,n2),E=list(e1))
  A <- G$adjacency_matrix()
  expect_true(is.null(dimnames(A))) 
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1,n2)
  G <- Graph$new(V=list(n1,n2),E=list(e1))
  A <- G$adjacency_matrix()
  dn <- dimnames(A)
  expect_equal(names(dn), c("out.node", "in.node"))  
  expect_equal(dn$out.node, c("n1", "n2"))
  expect_equal(dn$in.node, c("n1", "n2"))
  expect_equal(sum(A-matrix(c(0,1,0,1),nrow=2)),0)
  # binary
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1,n2)
  e2 <- Edge$new(n1,n1)
  G <- Graph$new(V=list(n1,n2),E=list(e1,e2))
  A <- G$adjacency_matrix(binary=FALSE)
  expect_equal(A["n1","n1"],2)
  A <- G$adjacency_matrix(binary=TRUE)
  expect_equal(A["n1","n1"],1)
})

# tests of graph algorithms
test_that("simple and non-simple graphs are detected", {
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1,n2)
  #
  G <- Graph$new(V=list(n1,n2), E=list(e1))
  expect_true(G$is_simple())
  #
  e2 <- Edge$new(n1,n1)
  G <- Graph$new(V=list(n1,n2), E=list(e1,e2))
  expect_false(G$is_simple())
  #
  e2 <- Edge$new(n2,n1)
  G <- Graph$new(V=list(n1,n2), E=list(e1,e2))
  expect_false(G$is_simple())
})

test_that("connected and non-connected graphs are identified", {
  #
  G <- Graph$new(V=list(), E=list())
  expect_false(G$is_connected())
  # 
  n1 <- Node$new()
  G <- Graph$new(V=list(n1), E=list())
  expect_true(G$is_connected())
  e1 <- Edge$new(n1,n1)
  G <- Graph$new(V=list(n1), E=list(e1))
  expect_true(G$is_connected())
  #
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1,n2)
  e2 <- Edge$new(n3,n3)
  G <- Graph$new(V=list(n1,n2,n3), E=list(e1,e2))
  expect_false(G$is_connected())
})

test_that("cyclic and acyclic graphs are identified", {
  # 
  G <- Graph$new(V=list(), E=list())
  expect_true(G$is_acyclic())
  #
  n1 <- Node$new()
  G <- Graph$new(V=list(n1), E=list())
  expect_true(G$is_acyclic())
  #
  n1 <- Node$new()
  e1 <- Edge$new(n1,n1)
  G <- Graph$new(V=list(n1), E=list(e1))
  expect_false(G$is_acyclic())
  #
  n0 <- Node$new("0")
  n1 <- Node$new("1")
  n2 <- Node$new("2")
  n3 <- Node$new("3")
  e1 <- Edge$new(n0,n1)
  e2 <- Edge$new(n1,n2)
  e3 <- Edge$new(n2,n3)
  #
  G <- Graph$new(V=list(n0,n1,n2,n3), E=list(e1,e2,e3))
  expect_true(G$is_acyclic())
  #
  e4 <- Edge$new(n0,n2)
  G <- Graph$new(V=list(n0,n1,n2,n3), E=list(e1,e2,e3,e4))
  expect_false(G$is_acyclic())
})

test_that("a simple tree is searched in DFS order", {
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
  # check
  expect_true(G$is_connected())
  expect_true(G$is_acyclic())
  expect_true(G$is_tree())
  # search
  N <- G$DFS(A)
  # check order
  L <- paste(sapply(N, function(n) {n$get_label()}), sep='', collapse='')
  expect_true((L=="ABDCEF" || L=="ABDCFE" || L=="ACEFBD" || L=="ACFEBD"))
})

# Published examples
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
  # adjacency
  A <- G$adjacency_matrix()
  EA <- matrix(c(0,2,0,0, 2,0,1,1, 0,1,0,1, 0,1,1,2), nrow=4, byrow=TRUE,
               dimnames=list(out.node=c("u","v","w","x"),
                             in.node=c("u","v","w","x")))
  expect_identical(A, EA)
  # neighbours
  expect_true(nodesetequal(G$neighbours(u),list(v)))
  expect_true(nodesetequal(G$neighbours(v),list(u,w,x)))
  expect_true(nodesetequal(G$neighbours(w),list(v,x)))
  expect_true(nodesetequal(G$neighbours(x),list(v,w)))
  # connected
  expect_true(G$is_connected())
  # cycle
  expect_false(G$is_acyclic())
  # tree
  expect_false(G$is_tree())
})

