#' @title 
#' Graph
#' 
#' @description
#' An R6 class to represent a graph (from discrete mathematics).
#' 
#' @details 
#' Encapulates and provides methods for computation and checking of undirected
#' graphs. 
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Graph <- R6::R6Class(
  classname = "Graph",
  private = list(
    V = NULL,
    E = NULL
  ),
  public = list(
    
    #' @description 
    #' Create a new Graph object from sets of nodes and edges. 
    #' @param V A list of Nodes.
    #' @param E A list of Edges.
    #' @return A Graph object.
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
      nodelabels <- sapply(V, function(v) {v$get_label()})
      nodelabels <- nodelabels[nchar(nodelabels)>0]
      if (length(unique(nodelabels))!=length(nodelabels)) {
        rlang::abort("Each defined node label must be unique", class="repeated_node_labels")
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
        W <- e$endpoints()
        sapply(W, function(w){
          if (!any(sapply(private$V, function(v) {return(v$is_same_node(W[[1]]))}))) {
            rlang::abort("Edge vertices must be in graph", class="not_in_graph")
          }  
          if (!any(sapply(private$V, function(v) {return(v$is_same_node(W[[2]]))}))) {
            rlang::abort("Edge vertices must be in graph", class="not_in_graph")
          }  
        })
      })
      if (length(unique(E)) != length(E)) {
        rlang::abort("Each E must be unique", class="repeated_edges")
      }
      edgelabels <- sapply(E, function(e) {e$label()})
      edgelabels <- edgelabels[nchar(edgelabels)>0]
      if (length(unique(edgelabels))!=length(edgelabels)) {
        rlang::abort("Each defined edge label must be unique", class="repeated_edge_labels")
      }
      private$E <- E
      return(invisible(self))
    },
    
    #' @description 
    #' Test whether a vertex or an edge is an element of the graph.
    #' @param x Subject vertex or edge
    #' @return TRUE if x is an element of V(G), the vertex set,
    #' or x is an element of E(G), the edge set.
    is_element = function(x) {
      member <- FALSE
      if (inherits(x, what="Node")) {
        member <- any(sapply(private$V, function(v) {return(x$is_same_node(v))}))
      } else if (inherits(x, what="Edge")) {
        member <- any(sapply(private$E, function(e) {return(x$is_same_edge(e))}))
      } else {
        rlang::abort("Argument 'x' is not a Node or an Edge", class="incorrect_type")
      }
      return(member)
    },
    
    #' @description 
    #' Find the index of vertex v in the vertices of the graph. The vertices
    #' will normally stored internally in the same order they were defined
    #' in the call to $new(), but this cannot be guaranteed. The index returned
    #' by this function will be same as the index of the vertex returned by 
    #' other methods, e.g. adjacancy_matrix.
    #' @param v The subject node.
    #' @return The index of the vertex (integer).
    vertex_index = function(v) {
      if (!inherits(v, what="Node")) {
        rlang::abort("Argument 'v' is not a Node", class="non-Node_node")
      }
      iv <- which(sapply(private$V,function(w){w$is_same_node(v)}), arr.ind=TRUE)      
      if (length(iv)==0) {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }
      return(iv)
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

#    #' @description 
#    #' A simple graph has no self loops or multi-edges.
#    #' @return TRUE if simple, FALSE if not.    
#    is_simple = function() {
#      
#    },
    
    #' @description 
    #' Compute the adjacency matrix for the graph. Each cell contains the
    #' number of edges joining the two vertexes, with the convention of
    #' self loops being counted twice, unless 'binary' is TRUE when cells are
    #' either 0 (not adjacent) or 1 (adjacent).
    #' @param binary If TRUE, the adjacency matrix is logical, each cell is
    #' {0,1}.
    #' @return A square numeric matrix with the number of rows and columns
    #' equal to the order of the graph. The rows and columns are in the
    #' same order as V. If all the nodes have labels the
    #' dimnames of the matrix are the labels of the nodes. 
    adjacency_matrix = function(binary=FALSE) {
      # check argument
      if (!is.logical(binary)) {
        rlang::abort("Argument 'binary' must be 'logical'.", class="non-logical_boolean")
      }
      # create matrix
      L <- sapply(private$V,function(v){v$get_label()})
      n <- self$order()
      if (all(nchar(L)>0)) {
        A <- matrix(rep(0,times=n*n), nrow=n, dimnames=list(out.node=L,in.node=L))
      } else {
        A <- matrix(rep(0,times=n*n), nrow=n)
      }
      # populate it
      sapply(private$E, function(e) {
        W <- e$endpoints()
        iv1 <- self$vertex_index(W[[1]])
        iv2 <- self$vertex_index(W[[2]])
        A[iv1,iv2] <<- A[iv1,iv2]+1
        A[iv2,iv1] <<- A[iv2,iv1]+1
      })
      # convert to binary, if required
      if (binary) {
        A <- apply(A, MARGIN=c(1,2), FUN=function(c){ifelse(c>1,1,c)})
      }
      return(A)
    },
    
    #' @description 
    #' The degree of a vertex in the graph, or number of incident edges.
    #' @param v The subject node.
    #' @return Degree of the vertex, integer.
    degree = function(v) {
      if (!inherits(v, what="Node")) {
        rlang::abort("Argument 'v' is not a Node", class="non-Node_node")
      }
      if (!any(sapply(private$V, function(n) {return(n$is_same_node(v))}))) {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }
      A <- self$adjacency_matrix()

      d <- 0
      sapply(private$E, function(e) {
        sapply(e$endpoints(), function(w) {
          if (w$is_same_node(v)) {
            d <<- d + 1  
          }
        })
      })
      return(d)
    },
    
    #' @description 
    #' Find the neigbours of a node. A property of the graph, not the node.
    #' Does not include self, even in the case of a loop to self.
    #' @param v The subject node. 
    #' @return A list of nodes which are joined to the subject.
    neighbours = function(v) {
      if (!inherits(v, what="Node")) {
        rlang::abort("Argument 'v' is not a Node", class="non-Node_node")
      }
      if (!any(sapply(private$V, function(n) {return(n$is_same_node(v))}))) {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }
      n <- list()
      sapply(private$E, function(e) {
        W <- e$endpoints()
        if (v$is_same_node(W[[1]])) {
          if (!v$is_same_node(W[[2]])) {
            n <<- c(n, W[[2]])
          }
        }
        if (v$is_same_node(W[[2]])) {
          if (!v$is_same_node(W[[1]])) {
            n <<- c(n, W[[1]])
          }
        }
      })
      # count each neighbour once
      n <- unique(n)
      return(n)
    },
    
    #' @description 
    #' Non-recursive depth-first search. Starts with a specified node and
    #' finds all the nodes reachable from it.
    #' @return List of reachable nodes, including self.
    DFS = function(v) {
      # check argument
      if (!inherits(v, what="Node")) {
        rlang::abort("Argument 'v' is not a Node", class="non-Node_node")
      }
      if (!any(sapply(private$V, function(n) {return(n$is_same_node(v))}))) {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }
      # List of discovered nodes, and a stack
      D <- list()
      S <- list()
      # S.push(v)
      S <- c(S,v)
      # while S is not empty do
      while (length(S)>0) {
        # v = S.pop()
        v <- S[[length(S)]]
        S[[length(S)]] <- NULL
        # if v is not labelled as discovered then
        if (!any(sapply(D,function(n){return(n$is_same_node(v))}))) {
          # label v as discovered
          D <- c(D,v)
          # for all edges from v to w in G.adjacentEdges(v) do
          for (w in self$neighbours(v)) {
            # S.push(w)
            S <- c(S,w)
          }
        }
      }
      # return discovered nodes
      return(D)
    }  
    
    
  )
)

    