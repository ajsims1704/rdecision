#' @title 
#' Arborescence
#' 
#' @description
#' An R6 class to represent an arborescence (rooted directed tree).
#' 
#' @details 
#' Class to encapsulate a directed rooted tree specialization of a digraph.
#' An arboresecence must be a directed tree with exactly one root and the
#' directed paths from the root must be unique.
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Arborescence <- R6::R6Class(
  classname = "Arborescence",
  inherit = Digraph,
  private = list(
  ),
  public = list(
    
    #' @description 
    #' Create a new Arborescence object from sets of nodes and edges. 
    #' @param V A list of Nodes.
    #' @param A A list of Arrows.
    #' @return An Arborescence object.
    initialize = function(V,A) {
      # initialize the base Digraph class (checks V, A)
      super$initialize(V,A)
      # check that the graph is an arborescence
      if (!self$is_arborescence()) {
        rlang::abort("The graph must be an arborescence", class="not_arborescence")
      }
      # return new Arborescence object
      return(invisible(self))
    },
    
    #' @description 
    #' Test whether the given node is a parent (has child nodes).
    #' @param v Node to test
    #' @return TRUE if v has one or more child nodes, FALSE otherwise.
    is_parent = function(v) {
      # check if this vertex has direct successors (also checks v)
      C <- self$direct_successors(v)
      return(length(C)>0)
    },
    
    #' @description 
    #' Test whether the given vertex is a leaf. In an arborescence,
    #' \code{is_parent()} and \code{is_leaf()} will be mutually exclusive.
    #' @param v Vertex to test.
    #' @return TRUE if v has no child nodes, FALSE otherwise. 
    is_leaf = function(v) {
      # check if this vertex has direct successors (also checks v)
      C <- self$direct_successors(v)
      return(length(C)==0)
    },

    #' @description 
    #' Find the root vertex of the arborescence.
    #' @return The root vertex.
    root = function() {
      # vertex with no incoming edges (only one, checked in initialize)
      u <- which(apply(private$B, MARGIN=1, function(r){!any(r>0)}),arr.ind=TRUE)
      return(private$V[[u]])
    },
    
    #' @description 
    #' Find all directed paths from the root of the tree to the leaves.
    #' @return A list of ordered node lists. 
    root_to_leaf_paths = function() {
      # Find the root
      r <- self$root()
      P <- list()
      sapply(private$V, function(v){
        if (self$is_leaf(v)) {
          pp <- self$paths(r,v)
          P[[length(P)+1]] <<- pp[[1]]    
        }
      })
      # return the paths
      return(P)
    }
  )
  
)
