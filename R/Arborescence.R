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
    }
  
  )
  
)
