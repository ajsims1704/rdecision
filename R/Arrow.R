#' @title 
#' Arrow
#' 
#' @description
#' An R6 class to represent an directed edge in a digraph.
#' 
#' @details Arrows are the formal term for links between pairs of nodes in a
#' directed graph. Inherits from class Edge.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
Arrow <- R6::R6Class(
  classname = "Arrow",
  inherit = Edge,
  private = list(
    source = NULL,
    target = NULL
  ),
  public = list(
    
    #' @description
    #' Create an object of type 'Arrow'.
    #' @param source Node from which the arrow emerges.
    #' @param target second Node to which the arrow points.
    #' @param label Character string containing the arrow label.
    #' @return A new `Arrow` object.
    initialize = function(source, target, label="") {
      # check and set source
      if (!inherits(source, what="Node")) {
        rlang::abort("'source' must be a 'Node'", class="non-Node_source")
      }
      else {
        private$source <- source
      }
      # check and set target
      if (!inherits(target, what="Node")) {
        rlang::abort("'target' must be a 'Node'", class="non-Node_target")
      }
      else {
        private$target <- target
      }
      # create base class object
      super$initialize(source, target, label)
    },

    #' @description
    #' Access source node.
    #' @return `Node` from which the arrow leads.
    get_source = function() {
      return(private$source)
    },
    
    #' @description
    #' Access target node.
    #' @return `Node` to which the arrow points.
    get_target = function() {
      return(private$target)
    }
  )
)
