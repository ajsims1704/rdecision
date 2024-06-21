#' @title An undirected graph
#' @description An R6 class to represent a graph (from discrete mathematics).
#' @details Encapsulates and provides methods for computation and checking of
#' undirected graphs. Graphs are systems of vertices connected in pairs by
#' edges. A base class.
#' @references{
#'   Gansner ER, Koutsofios E, North SC, Vo K-P. A technique for drawing
#'   directed graphs. \emph{IEEE Transactions on Software Engineering},
#'   1993;\bold{19}:214–30, \doi{10.1109/32.221135}.
#'
#'   Gross JL, Yellen J, Zhang P. Handbook of Graph Theory. Second edition,
#'   Chapman and Hall/CRC.; 2013, \doi{10.1201/b16132}
#' }
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
Graph <- R6::R6Class(
  classname = "Graph",
  lock_class = TRUE,
  private = list(
    V = NULL,
    E = NULL,
    AG = NULL,
    reorder = TRUE
  ),
  public = list(

    #' @description Create a new \code{Graph} object from sets of nodes
    #' and edges.
    #' @param V An unordered set of Nodes, as a list.
    #' @param E An unordered set of Edges, as a list.
    #' @return A \code{Graph} object.
    initialize = function(V, E) {
      # check and set nodes
      abortifnot(
        is.list(V),
        message = "V must be a list",
        class = "non-list_vertices"
      )
      vapply(V, FUN.VALUE = TRUE, FUN = function(v) {
        abortifnot(
          inherits(v, what = "Node"),
          message = "Each V must be a Node",
          class = "non-Node_vertex"
        )
        return(TRUE)
      })
      abortifnot(
        anyDuplicated(V) == 0L,
        message = "Each V must be unique",
        class = "repeated_nodes"
      )
      private$V <- V
      # check and set edges
      abortifnot(
        is.list(E),
        message = "E must be a list",
        class = "non-list_edges"
      )
      vapply(E, FUN.VALUE = TRUE, FUN = function(e) {
        abortifnot(
          inherits(e, what = "Edge"),
          message = "Each E must be an Edge",
          class = "non-Edge_edge"
        )
        vapply(e$endpoints(), FUN.VALUE = TRUE, FUN = function(w) {
          abortifnot(
            self$has_vertex(w),
            message = "All edge vertexes must be in graph",
            class = "not_in_graph"
          )
          return(TRUE)
        })
        return(TRUE)
      })
      abortifnot(
        anyDuplicated(E) == 0L,
        message = "Each E must be unique",
        class = "repeated_edges"
      )
      private$E <- E
      # reorder the vertices and edges for optimisation of graph algorithms, or
      # during package testing.
      if (private$reorder) {
        private$V <- sample(private$V, size = length(V), replace = FALSE)
        private$E <- sample(private$E, size = length(E), replace = FALSE)
      }
      # calculate the adjacency matrix
      private$AG <- self$graph_adjacency_matrix(FALSE)
      # return new graph object
      return(invisible(self))
    },

    #' @description Return the order of the graph (number of vertices).
    #' @return Order of the graph (integer).
    order = function() {
      return(length(private$V))
    },

    #' @description Return the size of the graph (number of edges).
    #' @return Size of the graph (integer).
    size = function() {
      return(length(private$E))
    },

    #' @description A list of all the Node objects in the graph.
    #' @details The list of Node objects is returned in the same order as their
    #' indexes understood by \code{vertex_index}, \code{vertex_at} and
    #' \code{vertex_along}, which is not necessarily the same order in which
    #' they were supplied in the \code{V} argument to \code{new}.
    vertexes = function() {
      return(private$V)
    },

    #' @description Sequence of vertex indices.
    #' @details Similar to \code{base::seq_along}, this function provides
    #' the indices of the vertices in the graph. It is intended for use by
    #' graph algorithms which iterate vertices.
    #' @return A numeric vector of indices from 1 to the order of the graph.
    #' The vertex at index \eqn{i} is not guaranteed to be the same vertex at
    #' \code{V[[i]]} of the argument \code{V} to \code{new} (i.e., the order in
    #' which the vertices are stored internally within the class may differ
    #' from the order in which they were supplied).
    vertex_along = function() {
      return(seq_along(self$vertexes()))
    },

    #' @description Find the index of a vertex in the graph.
    #' @param v A vertex, or list of vertexes.
    #' @return Index of \var{v}. The index of vertex \code{v} is the one
    #' used internally to the class object, which is not necessarily the same as
    #' the order of vertices in the \code{V} argument of \code{new}. \code{NA}
    #' if \var{v} is not a vertex, or is a vertex that is not in the graph.
    vertex_index = function(v) {
      # coerce v to a list, if it is not
      v <- if (is.list(v)) v else list(v)
      # match
      iv <- vapply(v, FUN.VALUE = 1L, FUN = function(vm) {
        index <- NA_integer_
        for (i in seq_along(private$V)) {
          if (identical(private$V[[i]], vm)) {
            index <- i
            break
          }
        }
        return(index)
      })
      return(iv)
    },

    #' @description Find the vertex at a given index.
    #' @details The inverse of function \code{vertex_index}. The function will
    #' raise an abort signal if all the supplied indexes are not vertexes. The
    #' function is vectorized, but for historical compatibility the return
    #' object is a single \code{Node} if \code{index} is a scalar. The
    #' return object can be guaranteed to be a list if \code{as_list} is set.
    #' @param index Index of vertex in the graph, as an integer, or vector of
    #' integers.
    #' @param as_list Boolean. If TRUE the method returns list of Nodes,
    #' even if the length of \code{index} is 1.
    #' @return Node at \code{index} if \code{index} is a scalar, a list of Nodes
    #' at the values of \code{index} if \code{index} is a vector, or an empty
    #' list if index is an empty array.
    vertex_at = function(index, as_list = FALSE) {
      # check argument
      abortifnot(
        !anyNA(index),
        all(vapply(X = index, FUN.VALUE = TRUE, FUN = is.integer)),
        all(index >= 1L),
        all(index <= self$order()),
        message = "There is no vertex with the supplied index.",
        class = "invalid_index"
      )
      # coerce index to a numeric vector
      iv <- as.integer(index)
      # fetch the vertex object(s); handles iv = int(0) case
      v <- if (length(iv) == 1L && !as_list) private$V[[iv]] else private$V[iv]
      return(v)
    },

    #' @description Test whether a vertex is an element of the graph.
    #' @param v Subject vertex.
    #' @return TRUE if v is an element of V(G).
    has_vertex = function(v) {
      # vertex_index checks if v is a node
      index <- self$vertex_index(v)
      return(!is.na(index))
    },

    #' @description Find label of vertexes at index i.
    #' @param iv Index of vertex, or vector of indexes.
    #' @return Label(s) of vertex at index i
    vertex_label = function(iv) {
      # fetch each vertex object (argument iv is checked in vertex_at)
      vv <- lapply(X = iv, FUN = self$vertex_at)
      # fetch the labels
      vl <- vapply(X = vv, FUN.VALUE = "x", FUN = function(v) {
        return(v$label())
      })
      return(vl)
    },

    #' @description A list of all the Edge objects in the graph.
    #' @details The list of Edge objects is returned in the same order as their
    #' indexes understood by \code{edge_index}, \code{edge_at} and
    #' \code{edge_along}, which is not necessarily the same order in which they
    #' were supplied in the \code{E} argument to \code{new}.
    edges = function() {
      return(private$E)
    },

    #' @description Sequence of edge indices.
    #' @details Similar to \code{base::seq_along}, this function provides
    #' the indices of the edges in the graph. It is intended for use by
    #' graph algorithms which iterate edges. It is equivalent to
    #' \code{seq_along(g$edges())}, where \code{g} is a graph.
    #' @return A numeric vector of indices from 1 to the size of the graph.
    #' The edge at index \eqn{i} is not guaranteed to be the same edge at
    #' \code{E[[i]]} of the argument \code{E} to \code{new} (i.e., the order in
    #' which the edges are stored internally within the class may differ
    #' from the order in which they were supplied).
    edge_along = function() {
      return(seq_along(self$edges()))
    },

    #' @description Find the index of an edge in a graph.
    #' @details The index of edge \code{e} is the one used internally to the
    #' class object, which is not necessarily the same as the
    #' order of edges in the \code{E} argument of \code{new}.
    #' @param e An edge object, or list of edge objects.
    #' @return Index of \code{e}. \code{NA} if \var{e} is not an edge, or is an
    #' edge that is not in the graph.
    edge_index = function(e) {
      # coerce e to a list, if it is not
      e <- if (is.list(e)) e else list(e)
      # match
      ie <- vapply(e, FUN.VALUE = 1L, FUN = function(em) {
        index <- NA_integer_
        for (i in seq_along(private$E)) {
          if (identical(private$E[[i]], em)) {
            index <- i
            break
          }
        }
        return(index)
      })
      return(ie)
    },

    #' @description Find the edge at a given index.
    #' @details The inverse of function \code{edge_index}. The function will
    #' raise an abort signal if the supplied index is not an edge. The
    #' function is vectorized, but for historical compatibility the return
    #' object is a single \code{Edge} if \code{index} is a scalar. The
    #' return object can be guaranteed to be a list if \code{as_list} is set.
    #' @param index Index of edge in the graph, as an integer, vector of
    #' integers, or list of integers.
    #' @param as_list Boolean. If TRUE the method returns list of Edges,
    #' even if the length of \code{index} is 1.
    #' @return The edge, or list of edges, with the specified index.
    edge_at = function(index, as_list = FALSE) {
      # check argument
      abortifnot(
        !anyNA(index),
        all(vapply(X = index, FUN.VALUE = TRUE, FUN = is.integer)),
        all(index >= 1L),
        all(index <= self$size()),
        message = "There is no edge with the supplied index.",
        class = "invalid_index"
      )
      # coerce index to a numeric vector
      ie <- as.integer(index)
      # fetch the edge(s)
      e <- if (length(ie) == 1L && !as_list) private$E[[ie]] else private$E[ie]
      return(e)
    },

    #' @description Test whether an edge is an element of the graph.
    #' @param e Edge or list of edges.
    #' @return Logical vector with each element \code{TRUE} if the corresponding
    #' element of \code{e} is an element of \eqn{E(G)}.
    has_edge = function(e) {
      # edge_index checks argument
      index <- self$edge_index(e)
      return(!is.na(index))
    },

    #' @description Find label of edge at index i
    #' @param ie Index of edge, or vector of indexes.
    #' @return Label of edge at index i, or character vector with the labels at
    #' indexes \code{ie}.
    edge_label = function(ie) {
      # fetch each edge object (argument ie is checked in edge_at)
      ee <- lapply(X = ie, FUN = self$edge_at)
      # fetch the labels
      el <- vapply(X = ee, FUN.VALUE = "x", FUN = function(e) {
        return(e$label())
      })
      return(el)
    },

    #' @description Compute the adjacency matrix for the graph.
    #' @details Each cell contains the
    #' number of edges joining the two vertexes, with the convention of
    #' self loops being counted twice, unless \code{binary} is \code{TRUE} when
    #' cells are either 0 (not adjacent) or 1 (adjacent).
    #' @param boolean If \code{TRUE}, the adjacency matrix is logical, each
    #' cell is \{\code{FALSE}, \code{TRUE}\}.
    #' @return A square integer matrix with the number of rows and columns
    #' equal to the order of the graph. The rows and columns are labelled
    #' with the node labels, if all the nodes in the graph have unique labels,
    #' or the node indices if not.
    graph_adjacency_matrix = function(boolean = FALSE) {
      # check argument
      abortifnot(
        is.logical(boolean),
        message = "Argument 'boolean' must be 'logical'.",
        class = "non-logical_boolean"
      )
      # if the matrix is not null, create it. This assumes the graph is
      # immutable (no edges or vertexes added or removed since its creation)
      if (is.null(private$AG)) {
        # create matrix
        iv <- self$vertex_along()
        L <- vapply(
          iv,
          FUN.VALUE = "x",
          FUN = function(i) {
            v <- self$vertex_at(i)
            v$label()
          }
        )
        if (anyDuplicated(L) != 0L || any(nchar(L) == 0L)) {
          L <- as.character(iv)
        }
        n <- self$order()
        A <- matrix(
          rep(0L, times = n * n),
          nrow = n,
          dimnames = list(out.node = L, in.node = L)
        )
        # populate it
        for (e in self$edges()) {
          W <- e$endpoints()
          iv1 <- self$vertex_index(W[[1L]])
          iv2 <- self$vertex_index(W[[2L]])
          A[[iv1, iv2]] <- A[[iv1, iv2]] + 1L
          A[[iv2, iv1]] <- A[[iv2, iv1]] + 1L
        }
        # save it
        private$AG <- A
      } else {
        A <- private$AG
      }
      # convert to boolean, if required
      if (boolean) {
        A <- A >= 1L
      }
      return(A)
    },

    #' @description Is this a simple graph?
    #' @details A simple graph has no self loops or multi-edges.
    #' @return \code{TRUE} if simple, \code{FALSE} if not.
    is_simple = function() {
      simple <- TRUE
      A <- self$graph_adjacency_matrix()
      if (nrow(A) > 0L) {
        if (sum(diag(A)) > 0L) {
          simple <- FALSE
        }
        if (max(A) > 1L) {
          simple <- FALSE
        }
      }
      return(simple)
    },

    #' @description Test whether the graph is connected.
    #' @details Graphs with no vertices are considered unconnected; graphs with
    #' 1 vertex are considered connected. Otherwise a graph is connected if all
    #' nodes can be reached from an arbitrary starting point. Uses a depth first
    #' search.
    #' @return \code{TRUE} if connected, \code{FALSE} if not.
    is_connected = function() {
      connected <- FALSE
      if (self$order() == 0L) {
        connected <- FALSE
      } else if (self$order() == 1L) {
        connected <- TRUE
      } else {
        # get the adjacency matrix
        A <- self$graph_adjacency_matrix(boolean = TRUE)
        # D marks nodes as discovered
        D <- vector(mode = "logical", length = self$order())
        # S is a stack of nodes being processed
        S <- Stack$new()
        # start with first vertex
        S$push(1L)
        # while S is not empty, do
        while (S$size() > 0L) {
          s <- S$pop()
          # if s is not labelled as discovered then
          if (!D[[s]]) {
            # label s as discovered
            D[[s]] <- TRUE
            # for all edges from s to n
            for (n in which(A[s, ])) {
              S$push(n)
            }
          }
        }
        if (all(D)) {
          connected <- TRUE
        }
      }
      return(connected)
    },

    #' @description Checks for the presence of a cycle in the graph.
    #' @details Uses a depth-first search from each node to detect the
    #' presence of back edges. A back edge is an edge from the current node
    #' joining a previously detected (visited) node, that is not the parent
    #' node of the current one.
    #' @return \code{TRUE} if no cycles detected.
    is_acyclic = function() {
      # acyclic if trivial
      if (self$order() == 0L) {
        return(TRUE)
      }
      # not acyclic if there are self loops or multi-edges
      if (!self$is_simple()) {
        return(FALSE)
      }
      # get the adjacency matrix
      A <- self$graph_adjacency_matrix(boolean = TRUE)
      # DFS from each vertex
      for (v in seq_len(self$order())) {
        # D marks nodes as discovered
        D <- vector(mode = "logical", length = self$order())
        # S is a stack of nodes being processed
        S <- Stack$new()
        S$push(v)
        # P (element p) is a stack of parents of nodes being processed
        P <- Stack$new()
        P$push(NA_integer_)
        # DFS
        while (S$size() > 0L) {
          # get next node to be processed from the stack
          s <- S$pop()
          # and get its parent
          p <- P$pop()
          # if not discovered, mark and process it
          if (!D[[s]]) {
            D[[s]] <- TRUE
            # process neighbours
            for (n in which(A[s, ])) {
              if (!is.na(p) && n != p && D[[n]]) {
                return(FALSE)
              }
              S$push(n)
              P$push(s)
            }
          }
        }
      }
      return(TRUE)
    },

    #' @description Compute whether the graph is connected and acyclic.
    #' @return \code{TRUE} if the graph is a tree; \code{FALSE} if not.
    is_tree = function() {
      return(self$is_connected() && self$is_acyclic())
    },

    #' @description The degree of a vertex in the graph.
    #' @details The number of incident edges.
    #' @param v The subject node.
    #' @return Degree of the vertex, integer.
    degree = function(v) {
      abortifnot(
        self$has_vertex(v),
        message = "Argument 'v' is not in graph",
        class = "invalid_vertex"
      )
      A <- self$graph_adjacency_matrix()
      iv <- self$vertex_index(v)
      d <- sum(A[iv, ])
      return(d)
    },

    #' @description Find the neighbours of a node.
    #' @details A property of the graph, not the node. Does not include self,
    #' even in the case of a loop to self.
    #' @param v The subject node (scalar, not a list).
    #' @return A list of nodes which are joined to the subject.
    neighbours = function(v) {
      iv <- self$vertex_index(v)
      abortif(
        anyNA(iv),
        length(iv) != 1L,
        message = "Argument 'v' must be a single node in the graph",
        class = "invalid_argument"
      )
      A <- self$graph_adjacency_matrix()
      diag(A) <- 0L
      ni <- which(A[iv, ] > 0L)
      n <- self$vertex_at(ni)
      return(n)
    },

    #' @description Export a representation of the graph in DOT format.
    #' @details Writes the representation in the \code{graphviz} DOT language
    #' (\url{https://graphviz.org/doc/info/lang.html}) for drawing with one
    #' of the \code{graphviz} tools including \code{dot} (Gansner, 1993).
    #' @return A character vector. Intended for passing to \code{writeLines}
    #' for saving as a text file.
    as_DOT = function() {
      # check whether all nodes have labels
      vi <- self$vertex_along()
      nl <- self$vertex_label(vi)
      nodelab <- all(nchar(x = nl) > 0L)
      # create stream vector (header+edges+footer)
      indent <- "  "
      o <- vector(mode = "character", length = 0L)
      # write header
      o[[length(o) + 1L]] <- "graph rdecision {"
      o[[length(o) + 1L]] <- paste0(indent, 'size="7,7" ;')
      o[[length(o) + 1L]] <- paste0(indent, "rankdir=LR ;")
      # write edges
      for (e in self$edges()) {
        ep <- e$endpoints()
        s <- ep[[1L]]
        t <- ep[[2L]]
        o[[length(o) + 1L]] <- paste(
          indent,
          ifelse(nodelab, paste0('"', s$label(), '"'), self$vertex_index(s)),
          "--",
          ifelse(nodelab, paste0('"', t$label(), '"'), self$vertex_index(t)),
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
    },
    
    #' @description Exports the digraph as a Graph Modelling Language (GML)
    #' stream.
    #' @details Intended to work with the DiagrammeR package, which is able to
    #' import GML files.
    #' @returns A GML stream as a character vector.
    as_gml = function() {
      # create stream vector
      o <- vector(mode = "character", length = 0L) 
      # open the graph
      o <- append(o, "graph [")
      o <- append(o, "  directed 0")
      # loop through nodes
      for (v in self$vertexes()) {
        o <- append(o, "  node [")
        o <- append(o, paste("    id", self$vertex_index(v)))
        label <- v$label()
        if (nchar(label) > 0L) {
          o <- append(o, paste0('    label ', '"', label, '"'))
        }
        o <- append(o, "  ]")
      }
      # loop through edges
      for (e in self$edges()) {
        o <- append(o, "  edge [")
        ends <- e$endpoints()
        o <- append(o, paste("    source", self$vertex_index(ends[[1L]])))
        o <- append(o, paste("    target", self$vertex_index(ends[[2L]])))
        label <- e$label()
        if (nchar(label) > 0L) {
          o <- append(o, paste0('    label ', '"', label, '"'))
        }
        o <- append(o, "  ]")
      }
      # close the graph
      o <- append(o, "]")
      # return the stream
      return(o)    
    }
  )
)
