#' @title A directed graph
#' 
#' @description An R6 class representing a digraph (a directed graph).
#' 
#' @details Encapsulates and provides methods for computation and checking of 
#' directed graphs (digraphs). Inherits from class \code{Graph}. 
#'
#' @references{ 
#'   Gansner ER, Koutsofios E, North SC, Vo K-P. A technique for drawing
#'   directed graphs. \emph{IEEE Transactions on Software Engineering},
#'   1993;\bold{19}:214–30, \doi{10.1109/32.221135}.
#' 
#'   Gross JL, Yellen J, Zhang P. Handbook of Graph Theory. Second edition, 
#'   Chapman and Hall/CRC.; 2013, \doi{10.1201/b16132}.
#'
#'   Kahn AB, Topological Sorting of Large Networks, 
#'   \emph{Communications of the \acronym{ACM}},
#'   1962;\strong{5}:558-562, \doi{10.1145/368996.369025}.
#' }
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Digraph <- R6::R6Class(
  classname = "Digraph",
  lock_class = TRUE,
  inherit = Graph,
  private = list(
    # class variables
    AD = NULL,    # adjacency matrix for digraph
    BD = NULL     # incidence matrix for digraph
  ),
  public = list(
    
    #' @description 
    #' Create a new \code{Digraph} object from sets of nodes and edges. 
    #' @param V A list of Nodes.
    #' @param A A list of Arrows.
    #' @return A Digraph object.
    initialize = function(V, A) {
      # check and set arrows
      abortifnot(
        is.list(A),
        message = "A must be a list", 
        class="non-list_arrows"
      )
      abortifnot(
        all(is_Arrow(A)),
        message = "Each 'A' must be an Arrow",
        class = "non-Arrow_edge"
      )
      # initialize the base Graph class (also checks V)
      super$initialize(V, A)
      # compute and save the adjacency matrix
      private$AD <- self$digraph_adjacency_matrix(FALSE)
      # compute and save the incidence matrix
      private$BD <- self$digraph_incidence_matrix()
      # return new Digraph object
      return(invisible(self))
    },

    #' @description Compute the adjacency matrix for the digraph.
    #' @details Each cell contains the number of edges from the row vertex to
    #' the column vertex, with the convention of self loops being counted once,
    #' unless \code{boolean} is \code{TRUE} when cells are either \code{FALSE}
    #' (not adjacent) or \code{TRUE} (adjacent).
    #' @param boolean If \code{TRUE}, the adjacency matrix is logical, each 
    #' cell is \code{{FALSE,TRUE}}.
    #' @return A square integer matrix with the number of rows and columns
    #' equal to the order of the graph. The rows and columns are in the
    #' same order as \code{V}. If the nodes have defined and unique labels the
    #' dimnames of the matrix are the labels of the nodes. 
    digraph_adjacency_matrix = function(boolean = FALSE) {
      # check argument
      abortifnot(is.logical(boolean),
        message = "Argument 'boolean' must be 'logical'.", 
        class="non-logical_boolean"
      )
      # create if not saved
      if (is.null(private$AD)) {
        # create matrix
        L <- vapply(X = private$V, FUN.VALUE = "x", FUN = function(v) v$label())
        n <- self$order()
        if (anyDuplicated(L) == 0L && all(nchar(L) > 0L)) {
          A <- matrix(
            rep(0L, times=n * n), 
            nrow=n, 
            dimnames=list(out.node = L, in.node = L)
          )
        } else {
          A <- matrix(rep(0L, times=n * n), nrow=n)
        }
        # populate it
        for (e in private$E) {
          s <- self$vertex_index(e$source())
          t <- self$vertex_index(e$target())
          A[[s, t]] <- A[[s, t]] + 1L
        }
        # save it 
        private$AD <- A
      } else {
        # read it
        A <- private$AD
      }
      # convert to logical, if required
      if (boolean) {
        A <- A >= 1L
      }
      return(A)
    },
    
    #' @description Compute the incidence matrix for the digraph. 
    #' @details Each row is a vertex and each column is an edge. Edges leaving
    #' a vertex have value -1 and edges entering have value +1. By convention
    #' self loops have value 0 (1-1). If all vertexes have defined and unique 
    #' labels and all edges have defined and unique labels, the dimnames of the
    #' matrix are the labels of the vertexes and edges.
    #' @return The incidence matrix of integers.
    digraph_incidence_matrix = function() {
      # create, if NULL
      if (is.null(private$BD)) {
        # create matrix
        LV <- vapply(private$V, FUN.VALUE = "x", FUN = function(v) v$label())
        LE <- vapply(private$E, FUN.VALUE = "x", FUN = function(e) e$label())
        nv <- self$order()
        ne <- self$size()
        if (anyDuplicated(LV) == 0L && anyDuplicated(LE) == 0L &&
            all(nchar(LV) > 0L) && all(nchar(LE) > 0L)
        ) {
          B <- matrix(
            rep(0L, times = nv * ne), 
            nrow = nv, 
            dimnames = list(vertex = LV, edge = LE)
          )
        } else {
          B <- matrix(rep(0L, times = nv * ne), nrow = nv)
        }
        # populate it
        for (e in private$E) {
          s <- self$vertex_index(e$source())
          t <- self$vertex_index(e$target())
          ei <- self$edge_index(e)
          if (s == t) {
            # self loop
            B[[s, ei]] <- 0L
          } else {
            # not self loop
            B[[s, ei]] <- -1L
            B[[t, ei]] <- 1L
          }
        }
        # save it
        private$BD <- B
      } else {
        # read it
        B <- private$BD
      }
      # return matrix
      return(B)
    },
    
    #' @description Topologically sort the vertexes in the digraph.
    #' @details Uses Kahn's algorithm (Kahn, 1962).
    #' @return A list of vertexes, topologically sorted. If the digraph has
    #' cycles, the returned ordered list will not contain all the vertexes
    #' in the graph, but no error will be raised.
    topological_sort = function() {
      # get the adjacency matrix (note: only vertex indexes are needed)
      AA <- self$digraph_adjacency_matrix()
      # L is an empty list that will contain the sorted vertexes
      L <- Stack$new()
      # S is a stack of all vertexes with no incoming edge
      S <- Stack$new()
      for (n in seq_len(ncol(AA))) {
        if (sum(AA[,n]) == 0L) {
          S$push(n)
        }
      }
      # while S is not empty
      while (S$size() > 0L) {
        # remove a vertex n from S
        n <- S$pop()
        # add n to tail of L
        L$push(n)
        # for each node m with an edge e from n to m
        for (m in which(AA[n,] > 0L, arr.ind = TRUE)) {
          # remove edge e from graph
          AA[[n,m]] <- 0L
          # if m has no other incoming edges, insert m into S
          if (sum(AA[, m]) == 0L) {
            S$push(m)
          }
        }
      }
      # return list of nodes indexed by L
      LL <- lapply(L$as_list(), function(l) private$V[[l]])
      return(LL)
    },

    #' @description Test whether the graph is connected.
    #' @details For digraphs this will always return \code{FALSE} because
    #' \dfn{connected} is not defined. Function \code{weakly_connected} 
    #' calculates whether the underlying graph is connected.
    #' @return \code{TRUE} if connected, \code{FALSE} if not.
    is_connected = function() {
      return(FALSE)
    },

    #' @description 
    #' Test whether the digraph is weakly connected, i.e. if the
    #' underlying graph is connected.
    #' @return \code{TRUE} if connected, \code{FALSE} if not.
    is_weakly_connected = function() {
      connected <- super$is_connected()
      return(connected)
    },

    #' @description Checks for the presence of a cycle in the graph.
    #' @details Attempts to do a topological sort. If the sort does not contain
    #' all vertexes, the digraph contains at least one cycle. This method 
    #' overrides \code{is_acyclic} in \code{Graph}.
    #' @return \code{TRUE} if no cycles detected.
    is_acyclic = function() {
      L <- self$topological_sort()
      return(length(L)==length(private$V))
    },    

    #' @description Is the digraph's underlying graph a tree?
    #' @details It is a tree if it is connected and acyclic.
    #' @return \code{TRUE} if the underlying graph is a tree; \code{FALSE} 
    #' if not.
    is_tree = function() {
      return(self$is_weakly_connected() && super$is_acyclic())
    },
    
    #' @description Is the digraph's underlying graph a polytree?
    #' @details It is a polytree if it is directed, connected and acyclic.
    #' Because the object is a digraph (directed), this is synonymous with
    #' \code{tree}.
    #' @return \code{TRUE} if the underlying graph is a tree; \code{FALSE}
    #' if not.
    is_polytree = function() {
      return(self$is_tree())
    },

    #' @description Is the digraph an arborescence?
    #' @details An \dfn{arborescence} is a tree with a single root and unique
    #' paths from the root.
    #' @return \code{TRUE} if the digraph is an arborescence; \code{FALSE}
    #' if not.
    is_arborescence = function() {
      # there must be one and only one root vertex
      B <- self$digraph_incidence_matrix()
      u <- which(apply(B, MARGIN = 1L, function(r) !any(r > 0L)), arr.ind=TRUE)
      if (length(u) != 1L) {
        return(FALSE)
      }
      # there must be exactly one path from the root to all other vertexes
      np <- vapply(
        setdiff(seq_along(private$V), u), 
        FUN.VAL = 1L, 
        function(v) {
          P <- self$paths(private$V[[u]], private$V[[v]])
          return(length(P))
        }
      )
      if (any(np != 1L)) {
        return(FALSE)
      }
      # otherwise return TRUE
      return(TRUE)
    },

    #' @description Find the direct successors of a node. 
    #' @param v The index vertex.
    #' @return A list of nodes or an empty list if the specified
    #' node has no successors.
    direct_successors = function(v) {
      iv <- self$vertex_index(v)
      abortifnot(
        !is.na(iv),
        message = "Argument 'v' is not in graph", 
        class = "not_in_graph"
      )
      AA <- self$digraph_adjacency_matrix()
      iw <- which(AA[iv, ] >= 1L, arr.ind = TRUE)
      successors <- private$V[iw]
      return(successors)
    },
      
    #' @description Find the direct predecessors of a node. 
    #' @param v The index vertex.
    #' @return A list of nodes or an empty list if the specified
    #' node has no predecessors.
    direct_predecessors = function(v) {
      iv <- self$vertex_index(v)
      abortifnot(
        !is.na(iv),
        message = "Argument 'v' is not in graph", 
        class = "not_in_graph"
      )
      AA <- self$digraph_adjacency_matrix()
      iw <- which(AA[, iv] >= 1L, arr.ind = TRUE)
      pred <- private$V[iw]
      return(pred)
    },

    #' @description Find all directed simple paths from source to target.
    #' @details In simple paths all vertexes are unique. Uses a recursive 
    #' depth-first search algorithm.
    #' @param s Source node.
    #' @param t Target node.
    #' @return A list of ordered node lists. 
    paths = function(s,t) {
      # check arguments
      is <- self$vertex_index(s)
      if (is.na(is)) {
        rlang::abort("Argument 's' is not in graph", class="not_in_graph")
      }
      it <- self$vertex_index(t)
      if (is.na(it)) {
        rlang::abort("Argument 't' is not in graph", class="not_in_graph")
      }
      # AA is the adjacency matrix
      AA <- self$digraph_adjacency_matrix(boolean=TRUE)
      # P is current path
      P <- Stack$new()
      # PLS is list of paths, held as a stack for convenience
      PLS <- Stack$new()
      # S is a node stack for the DFS
      S <- Stack$new()
      # recursive DFS, avoiding global variables by using Stacks
      dfs <- function(v) {
        P$push(v)
        if (v==it) {
          PLS$push(P$as_list())
        } else {
          for (w in which(AA[v,],arr.ind=TRUE)) {
            # process any successors not already in the path
            if (!(w %in% P$as_list())) {
              dfs(w)
            }
          }
        }
        P$pop()
      }
      dfs(is)
      # convert vertex indices into vertices before returning
      VPL <- lapply(PLS$as_list(), function(p) {
        vp <- lapply(p, function(v) private$V[[v]])
      })
      return(VPL)
    },
    
    #' @description Sequence of edges which join the specified path.
    #' @param P A list of Nodes
    #' @param what One of "edge" or "index".
    #' @return A list of Edges for \code{what = "edge"} or a list of Edge
    #' indices for \code{what = "index"}.
    walk = function(P, what = "edge") {
      # check 'what' argument
      abortifnot(
        what %in% c("edge", "index"),
        message = "'what' must be one of 'edge' or 'index'",
        class = "invalid_argument"
      )
      # check vertexes and get index of each
      p <- vapply(X = P, FUN.VALUE = 1L, FUN = function(n) self$vertex_index(n))
      # check all the vertices are in the graph
      abortif(
        anyNA(p),
        message = "At least one node is not in graph",
        class = "not_in_graph"
      )
      # loop through the ordered vertexes
      W <- list()
      if (length(P) > 1L) {
        pairs <- data.frame(
          s = p[1L : (length(P) - 1L)],
          t = p[2L : length(P)]
        )
        B <- self$digraph_incidence_matrix()
        W <- apply(pairs, MARGIN = 1L, function(r) {
          # look for edges leaving s and entering t
          e <- which((B[r[1L], ] == -1L) & (B[r[2L], ] == 1L), arr.ind = TRUE)
          abortifnot(
            length(e) >= 1L,
            message = "There must be an edge between adjacent nodes in 'P'",
            class = "missing_edge"
          )
          return(e[[1L]])
        })
      }
      # convert edge indices into edges, if required
      if (what == "edge") {
        W <- lapply(W, function(ie) self$edge_at(ie))
      }
      # return the walk 
      return(W)
    },
    
    #' @description Exports the digraph in DOT notation.
    #' @details Writes a representation of the digraph in the 
    #' \code{graphviz} DOT language 
    #' (\url{http://graphviz.org/doc/info/lang.html}) for drawing with one
    #' of the \code{graphviz} tools, including \code{dot} (Gansner, 1993). If
    #' all nodes have labels, these are used in the graph, otherwise the labels
    #' are the node indices.
    #' @param rankdir One of "LR" (default), "TB", "RL" or "BT".
    #' @param width of the drawing, in inches
    #' @param height of the drawing, in inches
    #' @return A character vector. Intended for passing to \code{writeLines}
    #' for saving as a text file.
    as_DOT = function(rankdir = "LR", width = 7.0, height = 7.0) {
      # check whether all nodes have labels
      nodelab <- all(
        vapply(
          private$V, 
          FUN.VALUE = TRUE, 
          FUN = function(v) nchar(v$label()) > 0L
        )
      )
      # check rankdir argument
      abortifnot(
        rankdir %in% c("LR", "TB", "RL", "BT"),
        message = "'rankdir' must be one of 'LR', 'TB', 'RL', 'BT'",
        class = "invalid_rankdir"
      )
      # check width and height
      abortifnot(
        is.numeric(width), width > 0.0, is.numeric(height), height > 0.0,
        message = "'width' and 'height' must be numeric and > 0",
        class = "invalid_size"
      )
      # create stream vector (header+edges+footer)
      indent <- "  "
      o <- vector(mode = "character", length = 0L)
      # write header
      o[[length(o) + 1L]] <- "digraph rdecision {"
      # write graph attributes
      o[[length(o) + 1L]] <- paste0(
        indent,
        "graph [ ",
        'rankdir = "', rankdir, '"', ", ",
        'size = "', width, ",", height, '"',
        " ] ;"
      )
      # write edges
      for (e in private$E) {
        s <- e$source()
        t <- e$target()
        o[[length(o) + 1L]] <- paste(
          indent,
          ifelse(nodelab, paste0('"',s$label(),'"'), self$vertex_index(s)),
          "->",
          ifelse(nodelab, paste0('"',t$label(),'"'), self$vertex_index(t)),
          ifelse(
            nchar(e$label()) > 0L, 
            paste("[", "label = ", paste0('"', e$label(), '"'), "]"),
            ""
          ), 
          ";" 
        )  
      }
      # footer
      o[[length(o) + 1L]] <- "}"
      # return the stream
      return(o)
    }
  ) 
)
