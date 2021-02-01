
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

# tests of digraph creation
test_that("incorrect node and edge types are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  a1 <- Arrow$new(n1, n2)
  expect_error(Digraph$new(n1, list(a1)), class="non-list_vertices")
  expect_error(Digraph$new(list(n1,n2), a1), class="non-list_arrows")
  expect_error(Digraph$new(list(n1,42), list(a1)), class="non-Node_vertex")
  expect_error(Digraph$new(list(n1,n2), list(a1,42)), class="non-Arrow_edge")
})

# tests of simple digraph properties
test_that("order and size are correct", {
  #
  n1 <- Node$new()
  n2 <- Node$new()
  a1 <- Arrow$new(n1, n2)
  #
  V <- list(n1,n2)
  A <- list(a1)
  G <- Digraph$new(V, A)
  expect_equal(G$order(), length(V))
  expect_equal(G$size(), length(A))
  #
  V <- list(n1)
  A <- list()
  G <- Digraph$new(V, A)
  expect_equal(G$order(), 1)
  expect_equal(G$size(), 0)
})

test_that("connectedness of underlying graph is correct", {
  # three nodes and two edges
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Arrow$new(n1,n2)
  e2 <- Arrow$new(n1,n3)
  G <- Digraph$new(V=list(n1,n2,n3), A=list(e1,e2))
  expect_false(G$is_connected())
  expect_true(G$is_weakly_connected())
  # same, but specified in different order
  G <- Digraph$new(V=list(n2,n3,n1), A=list(e1,e2))
  expect_false(G$is_connected())
  expect_true(G$is_weakly_connected())
})

# tests of adjacency and incidence matrix
test_that("adjacency matrix has correct properties", {
  # empty graph
  G <- Digraph$new(V=list(),A=list())
  expect_error(G$adjacency_matrix(42), class="non-logical_boolean")
  A <- G$adjacency_matrix()
  expect_true(is.matrix(A))
  expect_equal(nrow(A),0)
  expect_equal(ncol(A),0)
  # trivial graph
  n1 <- Node$new()
  G <- Digraph$new(V=list(n1),A=list())
  A <- G$adjacency_matrix()
  expect_true(is.matrix(A))
  expect_equal(nrow(A),1)
  expect_equal(ncol(A),1)
  expect_equal(A[1,1],0)
  # named nodes
  n1 <- Node$new("n1")
  n2 <- Node$new()
  e1 <- Arrow$new(n1,n2)
  G <- Digraph$new(V=list(n1,n2),A=list(e1))
  A <- G$adjacency_matrix()
  expect_true(is.null(dimnames(A))) 
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Arrow$new(n1,n2)
  G <- Digraph$new(V=list(n1,n2),A=list(e1))
  A <- G$adjacency_matrix()
  dn <- dimnames(A)
  expect_equal(names(dn), c("out.node", "in.node"))  
  expect_equal(dn$out.node, c("n1", "n2"))
  expect_equal(dn$in.node, c("n1", "n2"))
  expect_equal(sum(A-matrix(c(0,1,0,0),nrow=2,byrow=TRUE)),0)
  # boolean
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Arrow$new(n1,n2)
  e2 <- Arrow$new(n1,n2)
  e3 <- Arrow$new(n1,n1)
  G <- Digraph$new(V=list(n1,n2),A=list(e1,e2,e3))
  A <- G$adjacency_matrix(boolean=FALSE)
  expect_equal(A["n1","n1"],1)
  expect_equal(A["n1","n2"],2)
  A <- G$adjacency_matrix(boolean=TRUE)
  expect_true(A["n1","n1"])
  expect_true(A["n1","n2"])
})

test_that("incidence matrix has correct properties", {
  # named nodes
  n1 <- Node$new("n1")
  n2 <- Node$new()
  e1 <- Arrow$new(n1,n2)
  G <- Digraph$new(V=list(n1,n2),A=list(e1))
  B <- G$incidence_matrix()
  expect_true(is.null(dimnames(B))) 
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  ea <- Arrow$new(n1,n2,"a")
  eb <- Arrow$new(n1,n2,"b")
  G <- Digraph$new(V=list(n1,n2),A=list(ea,eb))
  B <- G$incidence_matrix()
  dn <- dimnames(B)
  expect_equal(names(dn), c("vertex", "edge"))  
  expect_equal(dn$vertex, c("n1", "n2"))
  expect_equal(dn$edge, c("a", "b"))
  expect_equal(sum(B-matrix(c(-1,1,1,-1),nrow=2,byrow=TRUE)),0)
})

test_that("arborescences are detected", {
  # out tree (single root)
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Arrow$new(n1,n2)
  e2 <- Arrow$new(n1,n3)
  G <- Digraph$new(V=list(n1,n2,n3),A=list(e1,e2))
  expect_true(G$is_arborescence())
  # in tree (2 roots)
  e1 <- Arrow$new(n2,n1)
  e2 <- Arrow$new(n3,n1)
  G <- Digraph$new(V=list(n1,n2,n3), A=list(e1,e2))
  expect_false(G$is_arborescence())
  # tree with one root and 3 branches
  n4 <- Node$new()
  e1 <- Arrow$new(n2,n1)
  e2 <- Arrow$new(n2,n3)
  e3 <- Arrow$new(n2,n4)
  G <- Digraph$new(V=list(n1,n2,n3,n4), A=list(e1,e2,e3))
  expect_true(G$is_arborescence())
  # tree with two roots
  n4 <- Node$new()
  e1 <- Arrow$new(n1,n2)
  e2 <- Arrow$new(n3,n2)
  e3 <- Arrow$new(n2,n4)
  G <- Digraph$new(V=list(n1,n2,n3,n4), A=list(e1,e2,e3))
  expect_false(G$is_arborescence())
})

# tests of topological sorting
# (https://www.cs.hmc.edu/~keller/courses/cs60/s98/examples/acyclic/)
test_that("topological sorting is correct", {
  # non-trivial DAG with one sort order
  n1 <- Node$new("1")
  n2 <- Node$new("2")
  n3 <- Node$new("3")
  n4 <- Node$new("4")
  n5 <- Node$new("5")
  n6 <- Node$new("6")
  e1 <- Arrow$new(n1,n2)
  e2 <- Arrow$new(n2,n3)
  e3 <- Arrow$new(n2,n4)
  e4 <- Arrow$new(n4,n6)
  e5 <- Arrow$new(n4,n5)
  e6 <- Arrow$new(n5,n6)
  e7 <- Arrow$new(n6,n3)
  V <- list(n1,n2,n3,n4,n5,n6)
  A <- list(e1,e2,e3,e4,e5,e6,e7)
  G <- Digraph$new(V,A)
  L <- G$topological_sort()
  expect_identical(L, list(n1,n2,n4,n5,n6,n3))
  # same graph with e4 reversed, making a cycle
  e4 <- Arrow$new(n6,n4)
  A <- list(e1,e2,e3,e4,e5,e6,e7)
  G <- Digraph$new(V,A)
  L <- G$topological_sort()
  expect_false(length(L)==length(V))
})

# tests of directed paths
test_that("all paths in a 4-node graph with cycle are discovered", {
  # https://www.geeksforgeeks.org/find-paths-given-source-destination/
  n0 <- Node$new("0")
  n1 <- Node$new("1")
  n2 <- Node$new("2")
  n3 <- Node$new("3")
  ea <- Arrow$new(n0,n2)
  eb <- Arrow$new(n2,n0)
  ec <- Arrow$new(n0,n1)
  ed <- Arrow$new(n0,n3)
  ee <- Arrow$new(n2,n1)
  ef <- Arrow$new(n1,n3)
  G <- Digraph$new(V=list(n0,n1,n2,n3),A=list(ea,eb,ec,ed,ee,ef))
  #
  P <- G$paths(n2,n3)
  expect_equal(length(P),3)
})

# example (Wikipedia URL)
test_that("example of 4 node digraph with cycle has correct properties", {
  # construct graph
  a <- Node$new('a')
  b <- Node$new('b')
  c <- Node$new('c')
  d <- Node$new('d')
  e <- Node$new('e')
  a1 <- Arrow$new(a,b)
  a2 <- Arrow$new(b,c)
  a3 <- Arrow$new(c,a)
  a4 <- Arrow$new(a,d)
  V <- list(a, b, c, d)
  A <- list(a1, a2, a3, a4)
  G <- Digraph$new(V, A)
  #
  expect_equal(G$order(), 4)
  expect_equal(G$size(), 4)
  #
  expect_error(G$direct_successors(42), class="incorrect_element_type")
  expect_error(G$direct_successors(e), class="not_in_graph")
  expect_true(nodesetequal(G$direct_successors(a), list(b,d)))
  expect_true(nodesetequal(G$direct_successors(c), list(a)))
  expect_true(length(G$direct_successors(d))==0)
  #
  expect_error(G$direct_predecessors(42), class="incorrect_element_type")
  expect_error(G$direct_predecessors(e), class="not_in_graph")
  expect_true(nodesetequal(G$direct_predecessors(a), list(c)))
  expect_true(nodesetequal(G$direct_predecessors(c), list(b)))
  expect_true(nodesetequal(G$direct_predecessors(d), list(a)))
  #
  expect_false(G$is_acyclic())
})

# example - New Scientist puzzle 62; 6th June 2020
test_that("rdecision solves New Scientist Puzzle 62", {
  # create vertices
  V <- list()
  for (i in 1:5) {
    for (j in 1:5) {
      V <- c(V, Node$new(paste("N",i,j,sep="")))
    }
  }
  # create edges
  E <- list()
  for (i in 1:5) {
    for (j in 1:4) {
      E <- c(E, Arrow$new(V[[5*(i-1)+j]], V[[5*(i-1)+j+1]], paste("H",i,j,sep="")))
    }
  } 
  for (i in 1:4) {
    for (j in 1:5) {
      E <- c(E, Arrow$new(V[[5*(i-1)+j]], V[[5*i+j]], paste("V",i,j,sep="")))
    }
  } 
  # create graph
  G <- Digraph$new(V,E)
  # test graph properties
  expect_true(G$is_simple())
  expect_false(G$is_connected())
  expect_true(G$is_weakly_connected())
  expect_false(G$is_tree())
  expect_false(G$is_polytree())
  expect_true(G$is_acyclic())
  # get all paths from A to B
  A <- V[[1]]
  B <- V[[25]]
  P <- G$paths(A,B)
  # convert paths to walks
  W <- lapply(P,function(p){G$walk(p)})
  # count and tabulate how many special edges each walk traverses
  BB <- c("V11", "H22", "V25", "H33", "V32", "H44", "V43")
  nw <- sapply(W, function(w) {
  lv <- sapply(w, function(e) {e$label() %in% BB}) 
   return(sum(lv))
  })
  ct <- as.data.frame(table(nw))
  # check that 23 paths traverse one special edge
  expect_equal(ct$Freq[ct$nw==1],23)
})
