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
      private$V <- V
      # check and set edges
      if (!is.list(E)) {
        rlang::abort("E must be a list", class="non-list_edges")
      }
      sapply(E, FUN=function(e) {
        if (!inherits(e, what="Edge")) {
          rlang::abort("Each E must be an Edge", class="non-Edge_edge")
        }
      })
      private$E <- E
      return(invisible(self))
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
    #' number of edges linking the two vertexes, with the convention of
    #' self loops being counted twice, unless 'logical' is TRUE when cells are
    #' either 0 (not adjacent) or 1 (adjacent).
    #' @param logical If TRUE, the adjacency matrix is logical, each cell is
    #' {0,1}.
    #' @return A square numeric matrix with the number of rows and columns
    #' equal to the order of the graph. The rows and columns are in the
    #' same order as V. If all the nodes have labels the
    #' dimnames of the matrix are the labels of the vertexes. 
    adjacency_matrix = function(logical=FALSE) {
      
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
    #' @return A list of nodes which are joined by an edge to the subject.
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

    