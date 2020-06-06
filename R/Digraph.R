#' @title 
#' Digraph
#' 
#' @description
#' An R6 class to represent a digraph (a directed graph, from discrete
#' mathematics).
#' 
#' @details 
#' Encapulates, and provides methods for computation and checking of directed
#' graphs (digraphs). Inherits from class Graph.
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Digraph <- R6::R6Class(
  classname = "Digraph",
  inherit = Graph,
  private = list(
    A = NULL
  ),
  public = list(
    
    #' @description 
    #' Create a new Digraph object from sets of nodes and edges. 
    #' @param V A list of Nodes.
    #' @param A A list of Arrows.
    #' @return A Digraph object.
    initialize = function(V, A) {
      # check and set arrows
      if (!is.list(A)) {
        rlang::abort("A must be a list", class="non-list_arrows")
      }
      sapply(A, FUN=function(a) {
        if (!inherits(a, what="Arrow")) {
          rlang::abort("Each A must be an Arrow", class="non-Arrow_edge")
        }
      })
      private$A <- A
      # initialize the base Graph class (also checks V)
      super$initialize(V, A)
      # return new Digraph object
      return(invisible(self))
    },

    #' @description
    #' Find the direct successors of a node. 
    #' @return A list of nodes or an empty list if the specified
    #' node has no successors.
    direct_successors = function(v) {
      successors <- list()
      if (!inherits(v, what="Node")) {
        rlang::abort("Argument 'v' is not a Node", class="non-Node_node")
      }
      if (!any(sapply(private$V, function(n) {return(n$is_same_node(v))}))) {
        rlang::abort("Node 'v' is not in graph", class="not_in_graph")
      }
      sapply(private$A, function(a) {
        if (v$is_same_node(a$get_source())) {
          successors <<- c(successors, a$get_target())
        }
      })
      return(successors)
    },
      
    #' @description
    #' Find the direct predecessors of a node. 
    #' @return A list of nodes or an empty list if the specified
    #' node has no predecessors.
    direct_predecessors = function(v) {
      pred <- list()
      if (!inherits(v, what="Node")) {
        rlang::abort("Argument 'v' is not a Node", class="non-Node_node")
      }
      if (!any(sapply(private$V, function(n) {return(n$is_same_node(v))}))) {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }
      sapply(private$A, function(a) {
        if (v$is_same_node(a$get_target())) {
          pred <<- c(pred, a$get_source())
        }
      })
      return(pred)
    },
    
    #' @description 
    #' Establish whether the graph is weakly connected, i.e. whether it is
    #' a connected undirected graph. 
    #' @return TRUE if graph is connected, assuming the edges are undirected.
    weakly_connected = function() {
      
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
          for (w in self$direct_successors(v)) {
            # S.push(w)
            S <- c(S,w)
          }
        }
      }
      # return discovered nodes
      return(D)
    }  
        
#    procedure DFS-iterative(G, v) is
#    let S be a stack
#    S.push(v)
#    while S is not empty do
#        v = S.pop()
#        if v is not labeled as discovered then
#            label v as discovered
#            for all edges from v to w in G.adjacentEdges(v) do 
#                S.push(w)
    
#    Find the vertex with no incoming edges (if there is more than one or no such vertex, fail).
#    
#    Do a breadth-first or depth-first search from that vertex. If you encounter an already visited vertex, it's not a tree.
#
# If you're done and there are unexplored vertices, it's not a tree - the graph is not connected.

#Otherwise, it's a tree.    
    
    
    
        
  ) 
)
