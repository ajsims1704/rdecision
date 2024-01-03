# Script to construct, in rdecision, a directed graph solution to New Scientist
# puzzle 62 "Burger Run", June 2020.
#
# The checks are done as part of the testthat framework, ensuring that
# changes in the package code which unintentionally result in deviations
# from the expected results of the model are identified.
#
# Code to construct and run the model is contained within labelled knitr code
# chunks and do not contain test expectations, so can be used by a vignette.
# Unlabelled code chunks may contain testthat expectations and should be
# ignored by a vignette.

## @knitr construct-graph -----------------------------------------------------
# node index function
idx <- function(i, j) {
  return(5L * (i - 1L) + j)
}
# create vertices
N <- vector(mode = "list", length = 5L * 4L)
for (i in seq(5L)) {
  for (j in seq(5L)) {
    N[[idx(i, j)]] <- Node$new(paste0("N", i, j))
  }
}
# create edges
H <- vector(mode = "list", length = 5L * 4L)
ie <- 1L
for (i in seq(5L)) {
  for (j in seq(4L)) {
    a <- Arrow$new(
      N[[idx(i, j)]], N[[idx(i, j + 1L)]], paste0("H", i, j)
    )
    H[[ie]] <- a
    ie <- ie + 1L
  }
}
V <- vector(mode = "list", length = 4L * 5L)
ie <- 1L
for (i in seq(4L)) {
  for (j in seq(5L)) {
    a <- Arrow$new(
      N[[idx(i, j)]], N[[idx(i + 1L, j)]], paste0("V", i, j)
    )
    V[[ie]] <- a
    ie <- ie + 1L
  }
}
# create graph
G <- Digraph$new(V = N, A = c(V, H))

## @knitr ---------------------------------------------------------------------
test_that("check graph properties are as expected", {
  expect_true(G$is_simple())
  expect_false(G$is_connected())
  expect_true(G$is_weakly_connected())
  expect_false(G$is_tree())
  expect_false(G$is_polytree())
  expect_true(G$is_acyclic())
})

## @knitr findpaths -----------------------------------------------------------
# get all paths from A to B
A <- N[[1L]]
B <- N[[25L]]
P <- G$paths(A, B)
# convert paths to walks
W <- lapply(P, FUN = G$walk)
# count and tabulate how many special edges each walk traverses
BB <- c("V11", "H22", "V25", "H33", "V32", "H44", "V43")
nw <- vapply(W, FUN.VALUE = 1L, FUN = function(w) {
  lv <- vapply(w, FUN.VALUE = TRUE, FUN = function(e) e$label() %in% BB)
  return(sum(lv))
})
# tabulate
ct <- as.data.frame(table(nw))

## @knitr ---------------------------------------------------------------------
test_that("23 paths traverse one special edge", {
  expect_identical(ct[[which(ct[, "nw"] == 1L), "Freq"]], 23L)
})
