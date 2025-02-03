# ensure suggested packages needed for testing are loaded
requireNamespace("igraph", quietly = TRUE)

# tests of graph creation
test_that("incorrect node and edge types are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  #
  expect_error(Graph$new(n1, list(e1)), class = "invalid_vertexes")
  expect_error(Graph$new(list(n1, n2), e1), class = "invalid_edges")
  expect_error(Graph$new(list(n1, 42L), list(e1)), class = "invalid_vertexes")
  expect_error(Graph$new(list(n1, n2), list(e1, 42L)), class = "invalid_edges")
  expect_error(
    Graph$new(V = list(n1, n1), E = list(e1)), class = "invalid_vertexes"
  )
  expect_error(
    Graph$new(V = list(n1, n2), E = list(e1, e1)),
    class = "invalid_edges"
  )
  #
  n3 <- Node$new()
  e2 <- Edge$new(n1, n3)
  expect_error(
    Graph$new(V = list(n1, n2), E = list(e1, e2)), class = "invalid_edges"
  )
  #
  n1 <- Node$new("n1")
  n2 <- Node$new("n1")
  n3 <- Node$new()
  e1 <- Edge$new(n1, n2, "e1")
  expect_silent(Graph$new(V = list(n1, n2), E = list(e1)))
  expect_silent(Graph$new(V = list(n1, n2, n3), E = list(e1)))
  #
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1, n2, "e1")
  e2 <- Edge$new(n2, n1, "e1")
  e3 <- Edge$new(n1, n2)
  expect_silent(Graph$new(V = list(n1, n2), E = list(e1, e2)))
  expect_silent(Graph$new(V = list(n1, n2), E = list(e1, e2, e3)))
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
  expect_identical(G$order(), 0L)
  expect_identical(G$size(), 0L)
  expect_length(G$vertex_along(), 0L)
  expect_length(G$edge_along(), 0L)
  # a graph with 2 nodes and an edge
  V <- list(n1, n2)
  E <- list(e1)
  G <- Graph$new(V, E)
  expect_identical(G$order(), length(V))
  expect_identical(G$size(), length(E))
  expect_length(G$vertex_along(), 2L)
  expect_length(G$edge_along(), 1L)
  # a graph with one node
  V <- list(n1)
  E <- list()
  G <- Graph$new(V, E)
  expect_identical(G$order(), 1L)
  expect_identical(G$size(), 0L)
  expect_length(G$vertex_along(), 1L)
  expect_length(G$edge_along(), 0L)
})

# tests of vertex and edge properties
test_that("vertex and edge properties are set and got", {
  # create graph
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1, n2, label = "e1")
  e2 <- Edge$new(n1, n3)
  G <- Graph$new(V = list(n1, n2), E = list(e1))
  # check that valid vertices in the graph are identified
  expect_false(G$has_vertex(42L))
  expect_true(G$has_vertex(n1))
  expect_true(G$has_vertex(n2))
  expect_false(G$has_vertex(n3))
  # check that valid edges in the graph are identified
  expect_false(G$has_edge(42L))
  expect_true(G$has_edge(e1))
  # tests of vertex indices
  in1 <- G$vertex_index(n1)
  expect_identical(G$vertex_at(in1), n1)
  in2 <- G$vertex_index(n2)
  expect_identical(G$vertex_at(in2), n2)
  in3 <- G$vertex_index(n3)
  expect_true(is.na(in3))
  expect_error(G$vertex_at(in3), class = "invalid_index")
  expect_error(G$vertex_at(42L), class = "invalid_index")
  expect_error(G$vertex_at("42"), class = "invalid_index")
  expect_length(G$vertex_along(), 2L)
  # tests of edge indexes
  ie1 <- G$edge_index(e1)
  expect_identical(G$edge_at(ie1), e1)
  ie2 <- G$edge_index(e2)
  expect_true(is.na(ie2))
  ie3 <- G$edge_index(42L)
  expect_true(is.na(ie3))
  expect_length(G$edge_index(list()), 0L)
  # test of finding edges, given index
  expect_error(G$edge_at(42L), class = "invalid_index")
  expect_error(G$edge_at("42"), class = "invalid_index")
  expect_error(G$edge_at(ie2), class = "invalid_index")
  expect_error(G$edge_at(-1L, class = "invalid_index"))
  # tests of degree function
  expect_error(G$degree(42L), class = "invalid_vertex")
  expect_error(G$degree(n3), class = "invalid_vertex")
  expect_error(G$degree(e1), class = "invalid_vertex")
  expect_identical(G$degree(n1), 1L)
})

# tests of node and edge indexes in vector mode
test_that("vectorized node and edge indexes are as expected", {
  # create graph
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1, n2, label = "e1")
  e2 <- Edge$new(n1, n3)
  G <- Graph$new(V = list(n1, n2, n3), E = list(e1, e2))
  # vertexes
  v <- G$vertexes()
  expect_r6_setequal(v, list(n1, n2, n3))
  # vertex indexes
  in1 <- G$vertex_index(n1)
  in2 <- G$vertex_index(n2)
  in3 <- G$vertex_index(n3)
  expect_identical(G$vertex_index(list(n1, n2, n3)), c(in1, in2, in3))
  expect_identical(G$vertex_index(list(n1, 42L)), c(in1, NA_integer_))
  expect_identical(G$vertex_index(G$vertexes()), G$vertex_along())
  # vertexes at given indexes
  expect_length(G$vertex_at(vector(mode = "integer", length = 0L)), 0L)
  expect_identical(G$vertex_at(c(in1, in2)), c(n1, n2))
  expect_identical(G$vertex_at(c(in2, in1)), c(n2, n1))
  expect_identical(G$vertex_at(list(in1, in2)), c(n1, n2))
  expect_identical(G$vertex_at(list(in1, in1)), c(n1, n1))
  expect_r6_setequal(G$vertex_at(c(in1, in2, in3)), list(n1, n2, n3))
  expect_error(G$vertex_at(c(in1, NA_integer_)), class = "invalid_index")
  expect_error(G$vertex_at(c(in1, G$order() + 1L)), class = "invalid_index")
  # test vertex_at with single index forced to a list
  expect_false(is.list(G$vertex_at(in1)))
  expect_identical(G$vertex_at(in1), n1)
  expect_type(G$vertex_at(in1, as_list = TRUE), "list")
  expect_identical(G$vertex_at(in1, as_list = TRUE), list(n1))
  # vertex existence
  expect_identical(G$has_vertex(list(n1, n2)), c(TRUE, TRUE))
  expect_identical(G$has_vertex(list(n1, n1, n2)), c(TRUE, TRUE, TRUE))
  expect_identical(G$has_vertex(list(n1, n2, 42L)), c(TRUE, TRUE, FALSE))
  # edges
  e <- G$edges()
  expect_r6_setequal(e, list(e1, e2))
  # edge indexes
  ie1 <- G$edge_index(e1)
  ie2 <- G$edge_index(e2)
  expect_identical(G$edge_index(list(e1, e2)), c(ie1, ie2))
  expect_identical(G$edge_index(list(e1, 42L)), c(ie1, NA_integer_))
  expect_identical(G$edge_index(G$edges()), G$edge_along())
  # edges at given indexes
  expect_length(G$edge_at(vector(mode = "integer", length = 0L)), 0L)
  expect_identical(G$edge_at(c(ie1, ie2)), c(e1, e2))
  expect_identical(G$edge_at(c(ie2, ie1)), c(e2, e1))
  expect_identical(G$edge_at(list(ie1, ie2)), c(e1, e2))
  expect_identical(G$edge_at(list(ie1, ie1)), c(e1, e1))
  expect_error(G$edge_at(c(ie1, NA_integer_)), class = "invalid_index")
  expect_error(G$edge_at(c(ie1, G$size() + 1L)), class = "invalid_index")
  # test edge_at with single index forced to a list
  expect_false(is.list(G$edge_at(ie1)))
  expect_identical(G$edge_at(ie1), e1)
  expect_type(G$edge_at(ie1, as_list = TRUE), "list")
  expect_identical(G$edge_at(ie1, as_list = TRUE), list(e1))
  # edge existence
  expect_identical(G$has_edge(list(e1, e2)), c(TRUE, TRUE))
  expect_identical(G$has_edge(list(e1, e1, e2)), c(TRUE, TRUE, TRUE))
  expect_identical(G$has_edge(list(e1, e2, 42L)), c(TRUE, TRUE, FALSE))
})

# tests of node and edge labels
test_that("node and edge label indexes are as expected", {
  # create graph
  n1 <- Node$new()
  n2 <- Node$new(label = "n2")
  n3 <- Node$new(label = "n3")
  e1 <- Edge$new(n1, n2, label = "e1")
  e2 <- Edge$new(n1, n3)
  G <- Graph$new(V = list(n1, n2, n3), E = list(e1, e2))
  # indexes
  in1 <- G$vertex_index(n1)
  in2 <- G$vertex_index(n2)
  in3 <- G$vertex_index(n3)
  ie1 <- G$edge_index(e1)
  ie2 <- G$edge_index(e2)
  # tests of edge labels
  expect_identical(e1$label(), "e1")
  expect_identical(e2$label(), "")
  expect_identical(G$edge_label(ie1), "e1")
  expect_identical(G$edge_label(ie2), "")
  expect_error(G$edge_label(42L), class = "invalid_index")
  expect_identical(G$edge_label(c(ie1, ie2)), c("e1", ""))
  expect_error(G$edge_label(c(ie1, as.numeric(ie2))), class = "invalid_index")
  expect_identical(G$edge_label(list(ie1, ie2)), c("e1", ""))
  # tests of vertex labels
  expect_identical(n1$label(), "")
  expect_identical(n2$label(), "n2")
  expect_identical(n3$label(), "n3")
  expect_identical(G$vertex_label(in1), "")
  expect_identical(G$vertex_label(in2), "n2")
  expect_identical(G$vertex_label(in3), "n3")
  expect_error(G$vertex_label(42L), class = "invalid_index")
  expect_identical(G$vertex_label(c(in1, in2)), c("", "n2"))
  expect_error(G$vertex_label(c(in1, as.numeric(in2))), class = "invalid_index")
  expect_identical(G$vertex_label(list(in1, in2, in3)), c("", "n2", "n3"))
})

# tests of adjacency matrix
test_that("adjacency matrix has correct properties", {
  # empty graph
  G <- Graph$new(V = list(), E = list())
  expect_error(G$graph_adjacency_matrix(42L), class = "non-logical_boolean")
  A <- G$graph_adjacency_matrix()
  expect_true(is.matrix(A))
  expect_identical(nrow(A), 0L)
  expect_identical(ncol(A), 0L)
  # trivial graph
  n1 <- Node$new()
  G <- Graph$new(V = list(n1), E = list())
  A <- G$graph_adjacency_matrix()
  expect_true(is.matrix(A))
  expect_identical(nrow(A), 1L)
  expect_identical(ncol(A), 1L)
  expect_identical(A[[1L, 1L]], 0L)
  # graph with some labelled nodes should have indices as labels
  n1 <- Node$new("n1")
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  G <- Graph$new(V = list(n1, n2), E = list(e1))
  A <- G$graph_adjacency_matrix()
  dn <- dimnames(A)
  expect_setequal(names(dn), c("out.node", "in.node"))
  expect_setequal(dn$out.node, as.character(G$vertex_along()))
  expect_setequal(dn$in.node, as.character(G$vertex_along()))
  # graph with all nodes named should have node names as labels
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1, n2)
  G <- Graph$new(V = list(n1, n2), E = list(e1))
  A <- G$graph_adjacency_matrix()
  dn <- dimnames(A)
  expect_setequal(names(dn), c("out.node", "in.node"))
  expect_setequal(dn$out.node, c("n1", "n2"))
  expect_setequal(dn$in.node, c("n1", "n2"))
  expect_identical(sum(A - matrix(c(0L, 1L, 0L, 1L), nrow = 2L)), 0L)
  # binary
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1, n2)
  e2 <- Edge$new(n1, n1)
  G <- Graph$new(V = list(n1, n2), E = list(e1, e2))
  A <- G$graph_adjacency_matrix(boolean = FALSE)
  expect_identical(A[["n1", "n1"]], 2L)
  A <- G$graph_adjacency_matrix(boolean = TRUE)
  expect_true(A[["n1", "n1"]])
})

# tests of graph algorithms
test_that("simple and non-simple graphs are detected", {
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  #
  G <- Graph$new(V = list(n1, n2), E = list(e1))
  expect_true(G$is_simple())
  #
  e2 <- Edge$new(n1, n1)
  G <- Graph$new(V = list(n1, n2), E = list(e1, e2))
  expect_false(G$is_simple())
  #
  e2 <- Edge$new(n2, n1)
  G <- Graph$new(V = list(n1, n2), E = list(e1, e2))
  expect_false(G$is_simple())
})

test_that("connected and non-connected graphs are identified", {
  #
  G <- Graph$new(V = list(), E = list())
  expect_false(G$is_connected())
  #
  n1 <- Node$new()
  G <- Graph$new(V = list(n1), E = list())
  expect_true(G$is_connected())
  e1 <- Edge$new(n1, n1)
  G <- Graph$new(V = list(n1), E = list(e1))
  expect_true(G$is_connected())
  #
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1, n2)
  e2 <- Edge$new(n3, n3)
  G <- Graph$new(V = list(n1, n2, n3), E = list(e1, e2))
  expect_false(G$is_connected())
  #
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1, n2)
  e2 <- Edge$new(n1, n3)
  G <- Graph$new(V = list(n2, n3, n1), E = list(e1, e2))
  expect_true(G$is_connected())
})

test_that("cyclic and acyclic graphs are identified", {
  #
  G <- Graph$new(V = list(), E = list())
  expect_true(G$is_simple())
  expect_true(G$is_acyclic())
  #
  n1 <- Node$new()
  G <- Graph$new(V = list(n1), E = list())
  expect_true(G$is_acyclic())
  #
  n1 <- Node$new()
  e1 <- Edge$new(n1, n1)
  G <- Graph$new(V = list(n1), E = list(e1))
  expect_false(G$is_acyclic())
  #
  n0 <- Node$new("0")
  n1 <- Node$new("1")
  n2 <- Node$new("2")
  n3 <- Node$new("3")
  e1 <- Edge$new(n0, n1)
  e2 <- Edge$new(n1, n2)
  e3 <- Edge$new(n2, n3)
  #
  G <- Graph$new(V = list(n0, n1, n2, n3), E = list(e1, e2, e3))
  expect_true(G$is_acyclic())
  #
  e4 <- Edge$new(n0, n2)
  G <- Graph$new(V = list(n0, n1, n2, n3), E = list(e1, e2, e3, e4))
  expect_false(G$is_acyclic())
})

# Published examples
test_that("Fig 1.1.1 from Gross & Yellen (2013)", {
  # the graph
  u <- Node$new("u")
  v <- Node$new("v")
  w <- Node$new("w")
  x <- Node$new("x")
  a <- Edge$new(u, v, "a")
  b <- Edge$new(v, u, "b")
  c <- Edge$new(x, x, "c")
  d <- Edge$new(x, w, "d")
  e <- Edge$new(x, v, "e")
  f <- Edge$new(w, v, "f")
  G <- Graph$new(V = list(u, v, w, x), E = list(a, b, c, d, e, f))
  # counts
  expect_identical(G$order(), 4L)
  expect_identical(G$size(), 6L)
  expect_identical(G$degree(u), 2L)
  expect_identical(G$degree(v), 4L)
  expect_identical(G$degree(w), 2L)
  expect_identical(G$degree(x), 4L)
  # adjacency, noting that nodes may not be in the same order as supplied
  nodenames <- c("u", "v", "w", "x")
  EA <- matrix(
    data = c(
      0L, 2L, 0L, 0L,  2L, 0L, 1L, 1L,
      0L, 1L, 0L, 1L,  0L, 1L, 1L, 2L
    ),
    nrow = 4L, byrow = TRUE,
    dimnames = list(out.node = nodenames, in.node = nodenames)
  )
  A <- G$graph_adjacency_matrix()
  A <- A[nodenames, nodenames]
  expect_identical(A, EA)
  # neighbours
  XX <- Node$new("XX")
  expect_error(G$neighbours(XX), class = "invalid_argument")
  expect_r6_setequal(G$neighbours(u), list(v))
  expect_r6_setequal(G$neighbours(v), list(u, w, x))
  expect_r6_setequal(G$neighbours(w), list(v, x))
  expect_r6_setequal(G$neighbours(x), list(v, w))
  expect_error(G$neighbours(list(u, v)), class = "invalid_argument")
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
  a <- Edge$new(u, v, "a")
  b <- Edge$new(v, u, "b")
  c <- Edge$new(x, x, "c")
  d <- Edge$new(x, w, "d")
  e <- Edge$new(x, v, "e")
  f <- Edge$new(w, v, "f")
  G <- Graph$new(V = list(u, v, w, x), E = list(a, b, c, d, e, f))
  # draw it as a GraphViz dot stream
  expect_silent(G$as_DOT())
  # draw it as a GML stream and check its properties with igraph
  gmlfile <- tempfile(fileext = ".gml")
  gml <- G$as_gml()
  writeLines(gml, con = gmlfile)
  ig <- igraph::read_graph(gmlfile, format = "gml")
  expect_identical(as.integer(igraph::gorder(ig)), 4L)
  expect_identical(as.integer(igraph::gsize(ig)), 6L)
  vlabels <- igraph::vertex_attr(ig, name = "label")
  expect_setequal(vlabels, c("u", "v", "w", "x"))
  elabels <- igraph::edge_attr(ig, name = "label")
  expect_setequal(elabels, c("a", "b", "c", "d", "e", "f"))
  expect_false(igraph::is_simple(ig))
  expect_false(igraph::is_dag(ig))
})
