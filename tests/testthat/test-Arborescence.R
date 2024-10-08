# tests of arborescence creation
test_that("incorrect node and edge types are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  a1 <- Arrow$new(n1, n2)
  expect_error(Arborescence$new(n1, list(a1)), class = "invalid_vertexes")
  expect_error(Arborescence$new(list(n1, n2), a1), class = "invalid_edges")
  expect_error(
    Arborescence$new(list(n1, 42L), list(a1)),
    class = "invalid_vertexes"
  )
  expect_error(
    Arborescence$new(list(n1, n2), list(a1, 42L)),
    class = "invalid_edges"
  )
})

test_that("graphs that are not trees are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Arrow$new(n1, n2)
  e2 <- Arrow$new(n1, n3)
  expect_silent(Arborescence$new(V = list(n1, n2, n3), A = list(e1, e2)))
  #
  e3 <- Arrow$new(n2, n3)
  expect_error(
    Arborescence$new(V = list(n1, n2, n3), A = list(e1, e2, e3)),
    class = "not_arborescence"
  )
})

test_that("graphs that are not arborescences are rejected", {
  # out tree (single root)
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Arrow$new(n1, n2)
  e2 <- Arrow$new(n1, n3)
  a <- Arborescence$new(V = list(n1, n2, n3), A = list(e1, e2))
  expect_true(a$is_tree())
  expect_true(a$is_polytree())
  expect_identical(a$root(), n1)
  expect_true(a$is_root(n1))
  expect_identical(a$is_root(list(n1, n2, n3)), c(TRUE, FALSE, FALSE))
  # out tree (single root) with nodes and edges in different order
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Arrow$new(n1, n2)
  e2 <- Arrow$new(n1, n3)
  g <- Digraph$new(V = list(n2, n3, n1), A = list(e1, e2))
  expect_true(g$is_weakly_connected())
  expect_true(g$is_tree())
  a <- Arborescence$new(V = list(n2, n3, n1), A = list(e1, e2))
  expect_true(a$is_tree())
  expect_identical(a$root(), n1)
  expect_true(a$is_root(n1))
  # in tree (2 roots)
  e1 <- Arrow$new(n2, n1)
  e2 <- Arrow$new(n3, n1)
  expect_error(
    Arborescence$new(V = list(n1, n2, n3), A = list(e1, e2)),
    class = "not_arborescence"
  )
})

test_that("parent, sibling and drawing functions are correct", {
  # create the tree using example from Walker (1989), fig 12
  nO <- Node$new("O")
  nE <- Node$new("E")
  nF <- Node$new("F")
  nN <- Node$new("N")
  nA <- Node$new("A")
  nD <- Node$new("D")
  nG <- Node$new("G")
  nM <- Node$new("M")
  nB <- Node$new("B")
  nC <- Node$new("C")
  nH <- Node$new("H")
  nI <- Node$new("I")
  nJ <- Node$new("J")
  nK <- Node$new("K")
  nL <- Node$new("L")
  eOE <- Arrow$new(nO, nE)
  eOF <- Arrow$new(nO, nF)
  eON <- Arrow$new(nO, nN)
  eEA <- Arrow$new(nE, nA)
  eED <- Arrow$new(nE, nD)
  eDB <- Arrow$new(nD, nB)
  eDC <- Arrow$new(nD, nC)
  eNG <- Arrow$new(nN, nG)
  eNM <- Arrow$new(nN, nM)
  eMH <- Arrow$new(nM, nH)
  eMI <- Arrow$new(nM, nI)
  eMJ <- Arrow$new(nM, nJ)
  eMK <- Arrow$new(nM, nK)
  eML <- Arrow$new(nM, nL)
  A <- Arborescence$new(
    V = list(nA, nB, nC, nD, nE, nF, nG, nH, nI, nJ, nK, nL, nM, nN, nO),
    A = list(
      eOE, eOF, eON, eEA, eED, eDB, eDC, eNG, eNM, eMH, eMI, eMJ, eMK, eML
    )
  )
  # check the tree size
  expect_identical(A$order(), 15L)
  # check parents
  expect_identical(A$parent(nE), nO)
  expect_identical(A$parent(nO), NA)
  expect_identical(A$parent(list(nE, nA, nO)), list(nO, nE, NA))
  expect_identical(A$parent(list()), list())
  # check parent status
  expect_false(A$is_parent(nA))
  expect_identical(A$is_parent(list()), vector(mode = "logical"))
  expect_identical(A$is_parent(list(nE, nA, nD)), c(TRUE, FALSE, TRUE))
  expect_true(A$is_parent(nO))
  # check leaf status
  expect_false(A$is_leaf(nO))
  expect_true(A$is_leaf(nA))
  expect_identical(A$is_leaf(list()), vector(mode = "logical"))
  expect_identical(A$is_leaf(list(nE, nA, nD)), c(FALSE, TRUE, FALSE))
  # check siblings
  expect_length(A$siblings(nO), 0L)
  expect_error(A$siblings(list(nA, nE)), class = "invalid_argument")
  expect_r6_setequal(A$siblings(nE), list(nF, nN))
  expect_r6_setequal(A$siblings(nA), list(nD))
  expect_r6_setequal(A$siblings(nJ), list(nH, nI, nK, nL))
  # check root-to-leaf paths
  paths <- A$root_to_leaf_paths()
  expect_length(paths, 10L)
  # check postree arguments
  expect_error(
    A$postree(SiblingSeparation = "x"),
    class = "non-numeric_SiblingSeparation"
  )
  expect_error(
    A$postree(SubtreeSeparation = "x"),
    class = "non-numeric_SubtreeSeparation"
  )
  expect_error(
    A$postree(LevelSeparation = "x"),
    class = "non-numeric_LevelSeparation"
  )
  expect_error(
    A$postree(RootOrientation = 90.0),
    class = "non-character_RootOrientation"
  )
  expect_error(
    A$postree(RootOrientation = "SOUTHWEST"),
    class = "invalid_RootOrientation"
  )
  expect_error(
    A$postree(MaxDepth = FALSE),
    class = "invalid_MaxDepth"
  )
  # check for coverage
  expect_silent(A$postree(RootOrientation = "NORTH"))
  expect_silent(A$postree(RootOrientation = "WEST"))
  # check max depth exceeded is detected
  expect_error(A$postree(MaxDepth = 2L), class = "POSITIONTREE_error")
  # check the node coordinates
  XY <- A$postree()
  XY$Label <- A$vertex_label(XY$n)
  expect_identical(nrow(XY), A$order())
  rownames(XY) <- A$vertex_label(A$vertex_along())
  expect_identical(XY["O", "x"], 13.5)
  expect_identical(XY["E", "x"], 3.0)
  expect_identical(XY["A", "x"], 0.0)
  expect_identical(XY["D", "x"], 6.0)
  expect_identical(XY["B", "x"], 3.0)
  expect_identical(XY["C", "x"], 9.0)
  expect_identical(XY["F", "x"], 13.5)
  expect_identical(XY["N", "x"], 24.0)
  expect_identical(XY["G", "x"], 21.0)
  expect_identical(XY["M", "x"], 27.0)
  expect_identical(XY["H", "x"], 15.0)
  expect_identical(XY["I", "x"], 21.0)
  expect_identical(XY["J", "x"], 27.0)
  expect_identical(XY["K", "x"], 33.0)
  expect_identical(XY["L", "x"], 39.0)
  expect_identical(XY["O", "y"], 0.0)
  expect_identical(XY["E", "y"], 1.0)
  expect_identical(XY["F", "y"], 1.0)
  expect_identical(XY["N", "y"], 1.0)
  expect_identical(XY["A", "y"], 2.0)
  expect_identical(XY["D", "y"], 2.0)
  expect_identical(XY["G", "y"], 2.0)
  expect_identical(XY["M", "y"], 2.0)
  expect_identical(XY["B", "y"], 3.0)
  expect_identical(XY["C", "y"], 3.0)
  expect_identical(XY["H", "y"], 3.0)
  expect_identical(XY["I", "y"], 3.0)
  expect_identical(XY["J", "y"], 3.0)
  expect_identical(XY["K", "y"], 3.0)
  expect_identical(XY["L", "y"], 3.0)
})
