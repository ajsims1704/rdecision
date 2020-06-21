#' @title 
#' Action
#' 
#' @description
#' An R6 class to represent an action (choice) edge in a decision tree.
#' 
#' @details A specialism of class Arrow which is used in a decision tree to
#' represent edges with source nodes joined to \code{DecisionNode}s.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
Action <- R6::R6Class(
  classname = "Action",
  inherit = Arrow,
  private = list(
  ),
  public = list(
    
    #' @description
    #' Create an object of type 'Action'.
    #' @param source Decision node from which the arrow leaves.
    #' @param target Node which the arrow enters.
    #' @param label Character string containing the arrow label.
    #' @return A new \code{Action} object.
    initialize = function(source, target, label="") {
      # initialize base class
      super$initialize(source=source, target=target, label=label)
      # check that source inherits from DecisionNode
      if (!inherits(source, what="DecisionNode")) {
        rlang::abort("Node 'source' must be a DecisionNode", class="non-Decision_source")
      }
      # Return Action node
      return(invisible(self))
    }
  
  )
)
