#' @title 
#' Digraph
#' 
#' @description
#' An R6 class to represent a digraph (a directed graph, from discrete
#' mathematics).
#' 
#' @details 
#' The types of health economic model supported by `rdecision` are forms of
#' digraph, insofar as they are a set of objects in which some pairs of objects
#' are related. in `rdecision` terminology, the objects (vertices) are Nodes
#' and the relationships are directed edges (arrows). Formally, each type of
#' model is a particular form of digraph, as follows:
#' \describe{
#' \item{Markov Model}{A directed multigraph permitting loops (a loop 
#' multidigraph),
#' optionally labelled, or 'quiver'. It is a multigraph because there are 
#' two edges between each pair of nodes {A,B} representing the transition
#' probabilities from A to B and vice-versa. And it is a directed graph because
#' the transition probabilities refer to transitions in one direction. Each
#' edge can be optionally labelled. It permits loops (edges whose source and
#' target are the same node) to represent patients that remain in the same 
#' state between cycles.}
#' \item{DecisionTree}{A k-ary directed rooted tree, or 'arborescence'. This is 
#' considered a form of directed graph (digraph) by several authors. In 
#' `rdecision` the root is a DecisionNode and in decision trees used in health
#' economics there is an implied directionality, from root to leaf nodes.}
#' }
#' It is not expected that modellers will create objects of type Digraph
#' directly. It functions as a base class for the model types supported
#' by `rdecision`, to provide methods for digraph computation and checking.
#' #'
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Digraph <- R6::R6Class(
  classname = "Digraph",
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
    }
    
#    Find the vertex with no incoming edges (if there is more than one or no such vertex, fail).
#    
#    Do a breadth-first or depth-first search from that vertex. If you encounter an already visited vertex, it's not a tree.
#
# If you're done and there are unexplored vertices, it's not a tree - the graph is not connected.

#Otherwise, it's a tree.    
    
    
    
        
  ) 
)
