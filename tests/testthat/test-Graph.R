
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
  expect_silent(Graph$new(V=list(n1,n2),E=list(e1)))
  expect_silent(Graph$new(V=list(n1,n2,n3),E=list(e1)))
  #
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1,n2,"e1")
  e2 <- Edge$new(n2,n1,"e1")
  e3 <- Edge$new(n1,n2)
  expect_silent(Graph$new(V=list(n1,n2),E=list(e1,e2)))
  expect_silent(Graph$new(V=list(n1,n2),E=list(e1,e2,e3)))
})

# tests of simple graph properties
test_that("basic graph properties are set and got", {
  #
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  # empty graph
  V <- list()
  E <- list()
  G <- Graph$new(V, E)
  expect_equal(G$order(), 0)
  expect_equal(G$size(), 0)
  expect_equal(length(G$vertex_along()), 0)
  expect_equal(length(G$edge_along()), 0)
  # a graph with 2 nodes and an edge
  V <- list(n1,n2)
  E <- list(e1)
  G <- Graph$new(V, E)
  expect_equal(G$order(), length(V))
  expect_equal(G$size(), length(E))
  expect_equal(length(G$vertex_along()), 2)
  expect_equal(length(G$edge_along()), 1)
  # a graph with one node
  V <- list(n1)
  E <- list()
  G <- Graph$new(V, E)
  expect_equal(G$order(), 1)
  expect_equal(G$size(), 0)
  expect_equal(length(G$vertex_along()), 1)
  expect_equal(length(G$edge_along()), 0)
})

# tests of vertex and edge properties
test_that("vertex and edge properties are set and got", {
  # create graph
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1, n2)
  e2 <- Edge$new(n1, n3)
  G <- Graph$new(V = list(n1, n2), E = list(e1))
  # check that valid vertices in the graph are identified
  expect_false(G$has_vertex(42))
  expect_true(G$has_vertex(n1))
  expect_true(G$has_vertex(n2))
  expect_false(G$has_vertex(n3))
  # check that valid edges in the graph are identified
  expect_false(G$has_edge(42))
  expect_true(G$has_edge(e1))
  # tests of vertex indices
  in1 <- G$vertex_index(n1)
  expect_identical(G$vertex_at(in1), n1)
  in2 <- G$vertex_index(n2)
  expect_identical(G$vertex_at(in2), n2)
  in3 <- G$vertex_index(n3)
  expect_true(is.na(in3))
  expect_error(G$vertex_at(in3), class = "invalid_index")
  expect_error(G$vertex_at(42), class = "invalid_index")
  expect_error(G$vertex_at("42"), class = "invalid_index")
  expect_equal(length(G$vertex_along()), 2)
  # tests of edge indexes
  ie1 <- G$edge_index(e1)
  expect_identical(G$edge_at(ie1), e1)
  ie2 <- G$edge_index(e2)
  expect_true(is.na(ie2))
  ie3 <- G$edge_index(42)
  expect_true(is.na(ie3))
  expect_error(G$edge_at(42), class = "invalid_index")
  expect_error(G$edge_at("42"), class = "invalid_index")
  expect_error(G$edge_at(ie2), class = "invalid_index")
  # tests of degree function
  expect_error(G$degree(42), class="invalid_vertex")
  expect_error(G$degree(n3), class="invalid_vertex")
  expect_error(G$degree(e1), class="invalid_vertex")
  expect_equal(G$degree(n1), 1)
})

# tests of adjacency matrix
test_that("adjacency matrix has correct properties", {
  # empty graph
  G <- Graph$new(V=list(),E=list())
  expect_error(G$graph_adjacency_matrix(42), class="non-logical_boolean")
  A <- G$graph_adjacency_matrix()
  expect_true(is.matrix(A))
  expect_equal(nrow(A),0)
  expect_equal(ncol(A),0)
  # trivial graph
  n1 <- Node$new()
  G <- Graph$new(V=list(n1),E=list())
  A <- G$graph_adjacency_matrix()
  expect_true(is.matrix(A))
  expect_equal(nrow(A),1)
  expect_equal(ncol(A),1)
  expect_equal(A[1,1],0)
  # graph with some labelled nodes should have indices as labels
  n1 <- Node$new("n1")
  n2 <- Node$new()
  e1 <- Edge$new(n1,n2)
  G <- Graph$new(V=list(n1,n2),E=list(e1))
  A <- G$graph_adjacency_matrix()
  dn <- dimnames(A)
  expect_setequal(names(dn), c("out.node", "in.node"))
  expect_setequal(dn$out.node, as.character(G$vertex_along()))
  expect_setequal(dn$in.node, as.character(G$vertex_along()))
  # graph with all nodes named should have node names as labels
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1,n2)
  G <- Graph$new(V=list(n1,n2),E=list(e1))
  A <- G$graph_adjacency_matrix()
  dn <- dimnames(A)
  expect_setequal(names(dn), c("out.node", "in.node"))  
  expect_setequal(dn$out.node, c("n1", "n2"))
  expect_setequal(dn$in.node, c("n1", "n2"))
  expect_equal(sum(A-matrix(c(0,1,0,1),nrow=2)),0)
  # binary
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1,n2)
  e2 <- Edge$new(n1,n1)
  G <- Graph$new(V=list(n1,n2),E=list(e1,e2))
  A <- G$graph_adjacency_matrix(boolean=FALSE)
  expect_equal(A["n1","n1"],2)
  A <- G$graph_adjacency_matrix(boolean=TRUE)
  expect_true(A["n1","n1"])
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
  #
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1,n2)
  e2 <- Edge$new(n1,n3)
  G <- Graph$new(V=list(n2,n3,n1), E=list(e1,e2))
  expect_true(G$is_connected())
})

test_that("cyclic and acyclic graphs are identified", {
  # 
  G <- Graph$new(V=list(), E=list())
  expect_true(G$is_simple())
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

# Published examples
test_that("Fig 1.1.1 from Gross & Yellen (2013)", {
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
  A <- G$graph_adjacency_matrix()
  EA <- matrix(c(0,2,0,0, 2,0,1,1, 0,1,0,1, 0,1,1,2), nrow=4, byrow=TRUE,
               dimnames=list(out.node=c("u","v","w","x"),
                             in.node=c("u","v","w","x")))
  expect_identical(A, EA)
  # neighbours
  XX <- Node$new("XX")
  expect_error(G$neighbours(XX), class="not_in_graph")
  expect_R6setequal(G$neighbours(u),list(v))
  expect_R6setequal(G$neighbours(v),list(u,w,x))
  expect_R6setequal(G$neighbours(w),list(v,x))
  expect_R6setequal(G$neighbours(x),list(v,w))
  # connected
  expect_true(G$is_connected())
  # cycle
  expect_false(G$is_acyclic())
  # tree
  expect_false(G$is_tree())
})

test_that("Fig 1.1.1 from Gross & Yellen (2013) is drawn correctly", {
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
  # draw it
  expect_silent(DOT <- G$as_DOT())
})
