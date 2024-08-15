
# tests of digraph creation
test_that("incorrect node and edge types are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  a1 <- Arrow$new(n1, n2)
  expect_error(Digraph$new(n1, list(a1)), class = "invalid_vertexes")
  expect_error(Digraph$new(list(n1, n2), a1), class = "invalid_edges")
  expect_error(Digraph$new(list(n1, 42L), list(a1)), class = "invalid_vertexes")
  expect_error(
    Digraph$new(list(n1, n2), list(a1, 42L)),
    class = "invalid_edges"
  )
})

# tests of simple digraph properties
test_that("order and size are correct", {
  #
  n1 <- Node$new()
  n2 <- Node$new()
  a1 <- Arrow$new(n1, n2)
  #
  V <- list(n1, n2)
  A <- list(a1)
  G <- Digraph$new(V, A)
  expect_identical(G$order(), length(V))
  expect_identical(G$size(), length(A))
  #
  V <- list(n1)
  A <- list()
  G <- Digraph$new(V, A)
  expect_identical(G$order(), 1L)
  expect_identical(G$size(), 0L)
})

test_that("connectedness of underlying graph is correct", {
  # three nodes and two edges
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Arrow$new(n1, n2)
  e2 <- Arrow$new(n1, n3)
  G <- Digraph$new(V = list(n1, n2, n3), A = list(e1, e2))
  expect_false(G$is_connected())
  expect_true(G$is_weakly_connected())
  # same, but specified in different order
  G <- Digraph$new(V = list(n2, n3, n1), A = list(e1, e2))
  expect_false(G$is_connected())
  expect_true(G$is_weakly_connected())
})

# tests of adjacency and incidence matrix
test_that("adjacency matrix has correct properties", {
  # empty graph
  G <- Digraph$new(V = list(), A = list())
  expect_error(G$digraph_adjacency_matrix(42L), class = "non-logical_boolean")
  A <- G$digraph_adjacency_matrix()
  expect_true(is.matrix(A))
  expect_identical(nrow(A), 0L)
  expect_identical(ncol(A), 0L)
  # trivial graph
  n1 <- Node$new()
  G <- Digraph$new(V = list(n1), A = list())
  A <- G$digraph_adjacency_matrix()
  expect_true(is.matrix(A))
  expect_identical(nrow(A), 1L)
  expect_identical(ncol(A), 1L)
  expect_identical(A[[1L, 1L]], 0L)
  # named nodes
  n1 <- Node$new("n1")
  n2 <- Node$new()
  e1 <- Arrow$new(n1, n2)
  G <- Digraph$new(V = list(n1, n2), A = list(e1))
  A <- G$digraph_adjacency_matrix()
  expect_null(dimnames(A))
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Arrow$new(n1, n2)
  G <- Digraph$new(V = list(n1, n2), A = list(e1))
  A <- G$digraph_adjacency_matrix()
  dn <- dimnames(A)
  expect_setequal(names(dn), c("out.node", "in.node"))
  expect_setequal(dn$out.node, c("n1", "n2"))
  expect_setequal(dn$in.node, c("n1", "n2"))
  expect_identical(
    sum(A - matrix(c(0L, 1L, 0L, 0L), nrow = 2L, byrow = TRUE)), 0L
  )
  # self loops and double self loops
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  ea <- Arrow$new(n1, n1, "a")
  eb <- Arrow$new(n1, n2, "b")
  ec <- Arrow$new(n2, n2, "c")
  ed <- Arrow$new(n2, n2, "d")
  G <- Digraph$new(V = list(n1, n2), A = list(ea, eb, ec, ed))
  A <- G$digraph_adjacency_matrix(boolean = FALSE)
  expect_identical(
    sum(A - matrix(c(1L, 1L, 0L, 2L), nrow = 2L, byrow = TRUE)), 0L
  )
  # boolean
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Arrow$new(n1, n2)
  e2 <- Arrow$new(n1, n2)
  e3 <- Arrow$new(n1, n1)
  G <- Digraph$new(V = list(n1, n2), A = list(e1, e2, e3))
  A <- G$digraph_adjacency_matrix(boolean = FALSE)
  expect_identical(A[["n1", "n1"]], 1L)
  expect_identical(A[["n1", "n2"]], 2L)
  A <- G$digraph_adjacency_matrix(boolean = TRUE)
  expect_true(A[["n1", "n1"]])
  expect_true(A[["n1", "n2"]])
})

test_that("incidence matrix has correct properties", {
  # named nodes
  n1 <- Node$new("n1")
  n2 <- Node$new()
  e1 <- Arrow$new(n1, n2)
  G <- Digraph$new(V = list(n1, n2), A = list(e1))
  B <- G$digraph_incidence_matrix()
  expect_null(dimnames(B))
  # two nodes linked by two edges
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  ea <- Arrow$new(n1, n2, "a")
  eb <- Arrow$new(n1, n2, "b")
  G <- Digraph$new(V = list(n1, n2), A = list(ea, eb))
  B <- G$digraph_incidence_matrix()
  dn <- dimnames(B)
  expect_setequal(names(dn), c("vertex", "edge"))
  expect_setequal(dn$vertex, c("n1", "n2"))
  expect_setequal(dn$edge, c("a", "b"))
  expect_identical(
    sum(B - matrix(c(-1L, 1L, 1L, -1L), nrow = 2L, byrow = TRUE)), 0L
  )
  # two nodes and two edges with self loops
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  ea <- Arrow$new(n1, n1, "a")
  eb <- Arrow$new(n1, n2, "b")
  ec <- Arrow$new(n2, n2, "c")
  G <- Digraph$new(V = list(n1, n2), A = list(ea, eb, ec))
  B <- G$digraph_incidence_matrix()
  dn <- dimnames(B)
  expect_setequal(names(dn), c("vertex", "edge"))
  expect_setequal(dn$vertex, c("n1", "n2"))
  expect_setequal(dn$edge, c("a", "b", "c"))
  expect_identical(
    sum(B - matrix(c(0L, 1L, 0L, 0L, -1L, 0L), nrow = 2L, byrow = TRUE)), 0L
  )
})

test_that("arborescences are detected", {
  # out tree (single root)
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Arrow$new(n1, n2)
  e2 <- Arrow$new(n1, n3)
  G <- Digraph$new(V = list(n1, n2, n3), A = list(e1, e2))
  expect_true(G$is_arborescence())
  # in tree (2 roots)
  e1 <- Arrow$new(n2, n1)
  e2 <- Arrow$new(n3, n1)
  G <- Digraph$new(V = list(n1, n2, n3), A = list(e1, e2))
  expect_false(G$is_arborescence())
  # tree with one root and 3 branches
  n4 <- Node$new()
  e1 <- Arrow$new(n2, n1)
  e2 <- Arrow$new(n2, n3)
  e3 <- Arrow$new(n2, n4)
  G <- Digraph$new(V = list(n1, n2, n3, n4), A = list(e1, e2, e3))
  expect_true(G$is_arborescence())
  # tree with two roots
  n4 <- Node$new()
  e1 <- Arrow$new(n1, n2)
  e2 <- Arrow$new(n3, n2)
  e3 <- Arrow$new(n2, n4)
  G <- Digraph$new(V = list(n1, n2, n3, n4), A = list(e1, e2, e3))
  expect_false(G$is_arborescence())
})

# tests of topological sorting
# (https://www.cs.hmc.edu/~keller/courses/cs60/s98/examples/acyclic/)
test_that("topological sorting is correct", {
  # attempt to sort an empty graph
  g <- Digraph$new(V = list(), A = list())
  l <- g$topological_sort()
  expect_length(l, 0L)
  # non-trivial DAG with one sort order
  n1 <- Node$new("1")
  n2 <- Node$new("2")
  n3 <- Node$new("3")
  n4 <- Node$new("4")
  n5 <- Node$new("5")
  n6 <- Node$new("6")
  e1 <- Arrow$new(n1, n2)
  e2 <- Arrow$new(n2, n3)
  e3 <- Arrow$new(n2, n4)
  e4 <- Arrow$new(n4, n6)
  e5 <- Arrow$new(n4, n5)
  e6 <- Arrow$new(n5, n6)
  e7 <- Arrow$new(n6, n3)
  V <- list(n1, n2, n3, n4, n5, n6)
  A <- list(e1, e2, e3, e4, e5, e6, e7)
  G <- Digraph$new(V, A)
  L <- G$topological_sort()
  expect_identical(L, list(n1, n2, n4, n5, n6, n3))
  # same graph with e4 reversed, making a cycle
  e4 <- Arrow$new(n6, n4)
  A <- list(e1, e2, e3, e4, e5, e6, e7)
  G <- Digraph$new(V, A)
  L <- G$topological_sort()
  expect_false(length(L) == length(V))
})

# tests of directed paths
test_that("all paths in a 4-node graph with cycle are discovered", {
  # https://www.geeksforgeeks.org/find-paths-given-source-destination/
  n0 <- Node$new("0")
  n1 <- Node$new("1")
  n2 <- Node$new("2")
  n3 <- Node$new("3")
  ea <- Arrow$new(n0, n2)
  eb <- Arrow$new(n2, n0)
  ec <- Arrow$new(n0, n1)
  ed <- Arrow$new(n0, n3)
  ee <- Arrow$new(n2, n1)
  ef <- Arrow$new(n1, n3)
  G <- Digraph$new(V = list(n0, n1, n2, n3), A = list(ea, eb, ec, ed, ee, ef))
  # test paths and walks
  nX <- Node$new("X")
  expect_error(G$paths(nX, n3), class = "not_in_graph")
  expect_error(G$paths(n2, nX), class = "not_in_graph")
  P <- list()
  expect_error(G$walk(P), class = "invalid_argument")
  P <- list(n0)
  expect_error(G$walk(P), class = "invalid_argument")
  P <- list(n0, n1, nX)
  expect_error(G$walk(P), class = "not_in_graph")
  P <- list(n2, n3)
  expect_error(G$walk(P), class = "missing_edge")
  # test that all paths are found
  P <- G$paths(n2, n3)
  expect_length(P, 3L)
  PE <- list(c(n2, n1, n3), c(n2, n0, n3), c(n2, n0, n1, n3))
  nmatch <- 0L
  for (p in P) {
    for (pe in PE) {
      if (r6_setequal(p, pe)) nmatch <- nmatch + 1L
    }
  }
  expect_identical(nmatch, 3L)
  # check that walks for one path in edge and index form are as expected
  p <- list(n2, n1, n3)
  w <- G$walk(p)
  expect_length(w, 2L)
  expect_r6_setequal(w, list(ee, ef))
  w <- G$walk(p, what = "index")
  expect_length(w, 2L)
  expect_setequal(w, list(G$edge_index(ee), G$edge_index(ef)))
  expect_error(G$walk(p, "42"), class = "invalid_argument")
})

# example (Wikipedia Directed Graph page, "Basic Terminology" example
# https://en.wikipedia.org/wiki/Directed_graph
test_that("example of 4 node digraph with cycle has correct properties", {
  # construct graph
  a <- Node$new("a")
  b <- Node$new("b")
  c <- Node$new("c")
  d <- Node$new("d")
  a1 <- Arrow$new(a, b)
  a2 <- Arrow$new(b, c)
  a3 <- Arrow$new(c, a)
  a4 <- Arrow$new(a, d)
  V <- list(a, b, c, d)
  A <- list(a1, a2, a3, a4)
  G <- Digraph$new(V, A)
  # check graph dimensions
  expect_identical(G$order(), 4L)
  expect_identical(G$size(), 4L)
  # check direct successors
  expect_error(G$direct_successors(42L), class = "invalid_argument")
  expect_error(G$direct_successors(list(a, c)), class = "invalid_argument")
  e <- Node$new("e")
  expect_error(G$direct_successors(e), class = "invalid_argument")
  expect_r6_setequal(G$direct_successors(a), list(b, d))
  expect_r6_setequal(G$direct_successors(c), list(a))
  expect_length(G$direct_successors(d), 0L)
  expect_identical(G$direct_successors(d), list())
  expect_length(G$direct_successors(b), 1L)
  expect_identical(G$direct_successors(b), list(c))
  #
  # test of target() function
  expect_error(G$arrow_target(42L), class = "not_in_graph")
  expect_error(G$arrow_target(a), class = "not_in_graph")
  a5 <- Arrow$new(b, d)
  expect_true(is_Arrow(a5))
  expect_error(G$arrow_target(a5), class = "not_in_graph")
  expect_true(G$has_edge(a1))
  expect_identical(G$arrow_target(a1), G$vertex_index(b))
  expect_identical(G$arrow_target(a2), G$vertex_index(c))
  expect_identical(G$arrow_target(a4), G$vertex_index(d))
  # check direct predecessors
  expect_error(G$direct_predecessors(42L), class = "invalid_argument")
  expect_error(G$direct_predecessors(e), class = "invalid_argument")
  expect_error(G$direct_predecessors(list(a, c)), class = "invalid_argument")
  expect_r6_setequal(G$direct_predecessors(a), list(c))
  expect_r6_setequal(G$direct_predecessors(c), list(b))
  expect_r6_setequal(G$direct_predecessors(d), list(a))
  expect_length(G$direct_predecessors(d), 1L)
  expect_identical(G$direct_predecessors(d), list(a))
  #
  # test of source() function
  expect_error(G$arrow_source(42L), class = "not_in_graph")
  expect_error(G$arrow_source(a), class = "not_in_graph")
  a5 <- Arrow$new(b, d)
  expect_true(is_Arrow(a5))
  expect_error(G$arrow_source(a5), class = "not_in_graph")
  expect_true(G$has_edge(a1))
  expect_identical(G$arrow_source(a1), G$vertex_index(a))
  expect_identical(G$arrow_source(a2), G$vertex_index(b))
  expect_identical(G$arrow_source(a4), G$vertex_index(a))
  #
  expect_false(G$is_acyclic())
})

# create DOT representation of a graph (Sonnenberg & Beck, 1993, Fig 3)
test_that("DOT files of S&B fig 3 are as expected", {
  # check that a graph with some unlabelled nodes has nodes labelled without
  # text strings
  s1 <- Node$new("Well")
  s2 <- Node$new("Disabled")
  s3 <- Node$new()
  e1 <- Arrow$new(s1, s1)
  e2 <- Arrow$new(s1, s2, "ill")
  e3 <- Arrow$new(s1, s3)
  e4 <- Arrow$new(s2, s2)
  e5 <- Arrow$new(s2, s3)
  e6 <- Arrow$new(s3, s3)
  s3x <- Node$new()
  G <- Digraph$new(V = list(s1, s2, s3), A = list(e1, e2, e3, e4, e5, e6))
  # case as described in the paper
  s1 <- Node$new("Well")
  s2 <- Node$new("Disabled")
  s3 <- Node$new("Dead")
  e1 <- Arrow$new(s1, s1)
  e2 <- Arrow$new(s1, s2, "ill")
  e3 <- Arrow$new(s1, s3)
  e4 <- Arrow$new(s2, s2)
  e5 <- Arrow$new(s2, s3)
  e6 <- Arrow$new(s3, s3)
  G <- Digraph$new(V = list(s1, s2, s3), A = list(e1, e2, e3, e4, e5, e6))
  expect_error(G$as_DOT(rankdir = "TT"), class = "invalid_rankdir")
  expect_error(G$as_DOT(width = "42"), class = "invalid_size")
  dot <- G$as_DOT()
  expect_true(any(grepl(pattern = 'rankdir="LR"', fixed = TRUE, x = dot)))
  dot <- G$as_DOT(rankdir = "TB")
  expect_true(any(grepl(pattern = 'rankdir="TB"', fixed = TRUE, x = dot)))
  dot <- G$as_DOT()
  expect_true(any(grepl(pattern = 'size="7,7"', fixed = TRUE, x = dot)))
  dot <- G$as_DOT(width = 6.0)
  expect_true(any(grepl(pattern = 'size="6,7"', fixed = TRUE, x = dot)))
  dot <- G$as_DOT(rankdir = "TB", width = 6.5, height = 6.5)
  expect_true(any(grepl(pattern = 'size="6.5,6.5"', fixed = TRUE, x = dot)))
})

# check GML representation of a graph (Sonnenberg & Beck, 1993, Fig 3)
test_that("gml representation of S&B fig 3 is as expected", {
  # graph with some unlabelled nodes
  s1 <- Node$new("Well")
  s2 <- Node$new("Disabled")
  s3 <- Node$new()
  e1 <- Arrow$new(s1, s1)
  e2 <- Arrow$new(s1, s2, "ill")
  e3 <- Arrow$new(s1, s3)
  e4 <- Arrow$new(s2, s2)
  e5 <- Arrow$new(s2, s3)
  e6 <- Arrow$new(s3, s3)
  s3x <- Node$new()
  G <- Digraph$new(V = list(s1, s2, s3), A = list(e1, e2, e3, e4, e5, e6))
  # represent in GML format
  gml <- G$as_gml()
  expect_true(any(grepl(pattern = "Well", fixed = TRUE, x = gml)))
  expect_true(any(grepl(pattern = "ill", fixed = TRUE, x = gml)))
  # check import to igraph
  gmlfile <- tempfile(fileext = ".gml")
  writeLines(gml, con = gmlfile)
  ig <- igraph::read_graph(gmlfile, format = "gml")
  expect_identical(as.integer(igraph::gorder(ig)), 3L)
  expect_identical(as.integer(igraph::gsize(ig)), 6L)
  # case as described in the paper
  s1 <- Node$new("Well")
  s2 <- Node$new("Disabled")
  s3 <- Node$new("Dead")
  e1 <- Arrow$new(s1, s1)
  e2 <- Arrow$new(s1, s2, "ill")
  e3 <- Arrow$new(s1, s3)
  e4 <- Arrow$new(s2, s2)
  e5 <- Arrow$new(s2, s3)
  e6 <- Arrow$new(s3, s3)
  G <- Digraph$new(V = list(s1, s2, s3), A = list(e1, e2, e3, e4, e5, e6))
  # check import to igraph
  gmlfile <- tempfile(fileext = ".gml")
  gml <- G$as_gml()
  writeLines(gml, con = gmlfile)
  ig <- igraph::read_graph(gmlfile, format = "gml")
  expect_identical(as.integer(igraph::gorder(ig)), 3L)
  expect_identical(as.integer(igraph::gsize(ig)), 6L)
  vlabels <- igraph::vertex_attr(ig, name = "label")
  expect_setequal(vlabels, c("Well", "Disabled", "Dead"))
  elabels <- igraph::edge_attr(ig, name = "label")
  expect_setequal(elabels, c(rep("", times = 5L), "ill"))
})
