#' @title \verb{Graph} class
#' 
#' @description
#' An R6 class to represent a graph (from discrete mathematics).
#' 
#' @details 
#' Encapsulates and provides methods for computation and checking of undirected
#' graphs. Graphs are systems of vertices connected in pairs by edges.
#'
#' @references{ 
#'   Gross JL, Yellen J, Zhang P. Handbook of Graph Theory. Second edition, 
#'   Chapman and Hall/CRC.; 2013, \doi{10.1201/b16132}
#' }
#'  
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Graph <- R6::R6Class(
  classname = "Graph",
  private = list(
    V = NULL,
    E = NULL,
    AG = NULL
  ),
  public = list(
    
    #' @description 
    #' Create a new \verb{Graph} object from sets of nodes and edges. 
    #' @param V A list of Nodes.
    #' @param E A list of Edges.
    #' @return A \verb{Graph} object.
    initialize = function(V, E) {
      # check and set nodes
      if (!is.list(V)) {
        rlang::abort("V must be a list", class="non-list_vertices")
      }
      sapply(V, FUN=function(v) {
        if (!inherits(v, what="Node")) {
          rlang::abort("Each V must be a Node", class="non-Node_vertex")
        }
      })
      if (length(unique(V)) != length(V)) {
        rlang::abort("Each V must be unique", class="repeated_nodes")
      }
      private$V <- V
      # check and set edges
      if (!is.list(E)) {
        rlang::abort("E must be a list", class="non-list_edges")
      }
      sapply(E, FUN=function(e) {
        if (!inherits(e, what="Edge")) {
          rlang::abort("Each E must be an Edge", class="non-Edge_edge")
        }
        sapply(e$endpoints(), function(w){
          if (!self$has_vertex(w)) {
            rlang::abort(
              "All edge vertexes must be in graph", 
              class = "not_in_graph"
            )
          }  
        })
      })
      if (length(unique(E)) != length(E)) {
        rlang::abort("Each E must be unique", class="repeated_edges")
      }
      private$E <- E
      # calculate the adjacency matrix 
      private$AG <- self$graph_adjacency_matrix(FALSE)
      # return new graph object
      return(invisible(self))
    },

    #' @description 
    #' Find the index of a vertex in the graph.
    #' @param v Subject vertex
    #' @return Index of v. The vertexes are normally stored in the same order
    #' they are specified in \code{new}, but this cannot be guaranteed. This
    #' function returns the same index as used in the adjacency matrix and 
    #' \verb{NA} if the vertex is not in the graph.
    vertex_index = function(v) {
      # check argument
      if (!inherits(v, what="Node")) {
        rlang::abort(
          "Argument 'v' must be a Node", 
          class="invalid_vertex"
        )
      }
      # find v in V
      index <- NA
      for (i in 1:length(private$V)) {
        if (identical(private$V[[i]],v)) {
          index <- i
          break
        }
      }
      return(index)
    }, 
  
    #' @description 
    #' Test whether a vertex is an element of the graph.
    #' @param v Subject vertex.
    #' @return TRUE if v is an element of V(G).
    has_vertex = function(v) {
      # vertex_index checks v is a node
      index <- self$vertex_index(v)
      return(!is.na(index))
    },

    #' @description 
    #' Find the index of an edge in a graph.
    #' @param e Subject edge.
    #' @return Index of e. The edges are normally stored in the same order
    #' they are specified in \code{new}, but this cannot be guaranteed. This
    #' function returns the same index returned in other functions and \verb{NA}
    #' if the edge is not in the graph.
    edge_index = function(e) {
      # check argument
      if (!inherits(e, what="Edge")) {
        rlang::abort(
          "Argument 'e' must be an edge", 
          class="invalid_edge"
        )
      } 
      # find e in E
      index <- NA
      for (i in 1:length(private$E)) {
        if (identical(private$E[[i]],e)) {
          index <- i
          break
        }
      }
      return(index)
    },

    #' @description 
    #' Test whether an edge is element of the graph.
    #' @param e Subject edge.
    #' @return TRUE if e is an element of E(G).
    has_edge = function(e) {
      # edge_index checks argument
      index <- self$edge_index(e)
      return(!is.na(index))
    },

    #' @description 
    #' Return the order of the graph (number of vertices).
    #' @return Order of the graph (integer).
    order = function() {
      return(length(private$V))  
    },
    
    #' @description 
    #' Return the size of the graph (number of edges).
    #' @return Size of the graph (integer).
    size = function() {
      return(length(private$E))  
    },

    #' @description 
    #' Compute the adjacency matrix for the graph. Each cell contains the
    #' number of edges joining the two vertexes, with the convention of
    #' self loops being counted twice, unless \verb{binary} is \verb{TRUE} when
    #' cells are either 0 (not adjacent) or 1 (adjacent).
    #' @param boolean If TRUE, the adjacency matrix is logical, each cell is
    #' {FALSE,TRUE}.
    #' @return A square numeric matrix with the number of rows and columns
    #' equal to the order of the graph. The rows and columns are in the
    #' same order as V. If the nodes have defined and unique labels the
    #' \verb{dimnames} of the matrix are the labels of the nodes. 
    graph_adjacency_matrix = function(boolean=FALSE) {
      # check argument
      if (!is.logical(boolean)) {
        rlang::abort(
          "Argument 'boolean' must be 'logical'.",
          class = "non-logical_boolean")
      }
      # if the matrix is not null, create it. This assumes the graph is 
      # immutable (no edges or vertexes added or removed since its creation)
      if (is.null(private$AG)) {
        # create matrix
        L <- sapply(private$V,function(v){v$label()})
        n <- self$order()
        if (length(unique(L))==length(L) && all(nchar(L)>0)) {
          A <- matrix(rep(0,times=n*n), nrow=n, 
                      dimnames=list(out.node=L,in.node=L))
        } else {
          A <- matrix(rep(0,times=n*n), nrow=n)
        }
        # populate it
        vapply(X = private$E, FUN.VALUE=TRUE, FUN = function(e) {
          W <- e$endpoints()
          iv1 <- self$vertex_index(W[[1]])
          iv2 <- self$vertex_index(W[[2]])
          A[iv1,iv2] <<- A[iv1,iv2]+1
          A[iv2,iv1] <<- A[iv2,iv1]+1
          return(TRUE)
        })
        # save it
        private$AG <- A
      } else {
        A <- private$AG
      }
      # convert to boolean, if required
      if (boolean) {
        A <- A>=1
      }
      return(A)
    },
    
    #' @description 
    #' A simple graph has no self loops or multi-edges.
    #' @return TRUE if simple, FALSE if not.    
    is_simple = function() {
      simple <- TRUE
      A <- self$graph_adjacency_matrix()
      if (nrow(A) > 0) {
        if (sum(diag(A))>0) {
          simple <- FALSE
        }
        if (max(A)>1) {
          simple <- FALSE
        }
      }
      return(simple)
    },
    
    #' @description 
    #' Test whether the graph is connected. Graphs with no vertices are 
    #' considered unconnected; graphs with 1 vertex are considered
    #' connected. Otherwise a graph is connected if all nodes can be 
    #' reached from an arbitrary starting point. Uses a depth first
    #' search.
    #' @return TRUE if connected, FALSE if not.
    is_connected = function() {
      connected <- FALSE
      if (self$order()==0) {
        connected <- FALSE
      } else if (self$order()==1) {
        connected <- TRUE
      } else {
        # get the adjacency matrix
        A <- self$graph_adjacency_matrix(boolean=TRUE)
        # D marks nodes as discovered
        D <- vector(mode="logical", length=self$order())
        # S is a stack of nodes being processed
        S <- Stack$new()
        # start with first vertex
        S$push(1)
        # while S is not empty, do
        while (S$size()>0) {
          s <- S$pop()
          # if s is not labelled as discovered then
          if (!D[s]) {
            # label s as discovered
            D[s] <- TRUE
            # for all edges from s to n
            for (n in which(A[s,], arr.ind=TRUE)) {
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

    #' @description 
    #' Checks for the presence of a cycle in the graph using a depth-first
    #' search from each node to detect the presence of back edges. A back
    #' edge is an edge from the current node joining a previously detected 
    #' (visited) node, that is not the parent node of the current one.
    #' @return TRUE if no cycles detected.
    is_acyclic = function() {
      # acyclic if trivial
      if (self$order()==0) {
        return(TRUE)
      }
      # not acyclic if there are self loops or multi-edges
      if (!self$is_simple()) {
        return(FALSE)
      }
      # get the adjacency matrix
      A <- self$graph_adjacency_matrix(boolean=TRUE)
      # DFS from each vertex
      for (v in 1:self$order()) {
        # D marks nodes as discovered
        D <- vector(mode="logical", length=self$order())
        # S is a stack of nodes being processed
        S <- Stack$new()
        S$push(v)
        # P (element p) is a stack of parents of nodes being processed
        P <- Stack$new()
        P$push(as.integer(NA))
        # DFS
        while (S$size()>0) {
          # get next node to be processed from the stack
          s <- S$pop()
          # and get its parent
          p <- P$pop()
          # if not discovered, mark and process it
          if (!D[s]) {
            D[s] <- TRUE
            # process neighbours
            for (n in which(A[s,],arr.ind=TRUE)) {
              if (!is.na(p) && n!=p) {
                if (D[n]) {
                  return(FALSE)
                }
              }
              S$push(n)
              P$push(s)
            }
          }
        }
      }
      return(TRUE)
    },    
    
    #' @description 
    #' Compute whether the graph is connected and acyclic.
    #' @return TRUE if the graph is a tree; FALSE if not.
    is_tree = function() {
      return(self$is_connected() && self$is_acyclic())
    },

    #' @description 
    #' The degree of a vertex in the graph, or number of incident edges.
    #' @param v The subject node.
    #' @return Degree of the vertex, integer.
    degree = function(v) {
      d <- NA
      if (self$has_vertex(v)) {
        A <- self$graph_adjacency_matrix()
        iv <- self$vertex_index(v)
        d <- sum(A[iv,])
      } else {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }
      return(d)
    },

    #' @description 
    #' Find the neighbours of a node. A property of the graph, not the node.
    #' Does not include self, even in the case of a loop to self.
    #' @param v The subject node. 
    #' @return A list of nodes which are joined to the subject.
    neighbours = function(v) {
      n <- list()
      if (self$has_vertex(v)) {
        A <- self$graph_adjacency_matrix()
        diag(A) <- 0
        iv <- self$vertex_index(v)
        ni <- which(A[iv,]>0, arr.ind=TRUE)
        n <- private$V[ni]
      } else {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }     
      return(n)
    }
  
  )
)

    