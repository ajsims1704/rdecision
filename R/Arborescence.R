#' @title 
#' Arborescence
#' 
#' @description
#' An R6 class to represent a rooted tree (arborescence).
#' 
#' @details 
#' Class to encapsulate a rooted tree specialization of a digraph.
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Arborescence <- R6::R6Class(
  classname = "Arborescence",
  inherit = Graph,
  private = list(
    A = NULL
  ),
  public = list(
    
    #' @description 
    #' Create a new Arborescence object from sets of nodes and edges. 
    #' @param V A list of Nodes.
    #' @param A A list of Arrows.
    #' @return An Arborescence object.
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
      # initialize the base Digraph class (also checks V)
      super$initialize(V, A)
      # return new Arborescence object
      return(invisible$self)
    }
  
  )
  
)
