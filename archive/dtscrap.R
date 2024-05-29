####

# create an edge properties matrix, one row per edge per run
epr <- c("index", "probability", "cost", "benefit", "run")
ei <- self$edge_along()
ep <- matrix(
  data = NA_real_, nrow = length(ei) * N, ncol = length(epr),
  dimnames = list(NULL, epr)
)
# create a leaf node properties matrix, one row per leaf per run
lpr <- c("index", "utility", "QALY", "run")
li <- self$leaf_nodes(what = "index")
lp <- matrix(
  data = NA_real_, nrow = length(li) * N, ncol = length(lpr),
  dimnames = list(NULL, lpr)
)
# populate epr and lpr for each run
for (i in seq_len(N)) {
  # set the ModVar values (to chosen option)
  for (v in MV) {
    v$set(setvars)
  }
  # fetch the edge properties and add them to the matrix
  ep[seq(from = (i - 1L) * length(ei) + 1L, to = i * length(ei)), ] <-
    cbind(self$edge_properties(), run = rep(i, times = length(ei)))
  # fetch the leaf properties and add them to the matrix
  for (l in seq_along(li)) {
    iv <- li[[l]]
    v <- self$vertex_at(iv)
    r <- c(index = iv, utility = v$utility(), QALY = v$QALY(), run = i)
    lp[(i - 1L) * length(li) + l, ] <- r
  }
}
# create a matrix representation of walks: each column is an edge index
# each row is a walk, entries are TRUE if an edge is in a walk or FALSE
# otherwise, row names are the index of the terminating leaf.
r2lp <- self$root_to_leaf_paths()
r2lw <- lapply(r2lp, FUN = self$walk, what = "index")
wm <- matrix(
  data = FALSE, nrow = length(li), ncol = length(ei),
  dimnames = list(li, ei)
)
for (i in seq_along(r2lw)) {
  walk <- r2lw[[i]]
  # find index of leaf (terminal node) used for row label
  stem <- self$edge_at(walk[[length(walk)]])
  leaf <- stem$target()
  ileaf <- self$vertex_index(leaf)
  # set edges visited on the walk to TRUE, indexed by column label
  wm[as.character(ileaf), as.character(unlist(walk))] <- TRUE
}
#  

str(ep)
str(lp)
str(wm)
print(wm)
####

