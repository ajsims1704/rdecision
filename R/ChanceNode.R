#' @title
#' ChanceNode
#'
#' @description 
#' An R6 class to represent a chance node in a decision tree.
#' 
#' @details
#' An R6 class to represent a chance node in a decision tree. 
#' The node is associated with at least two branches to other nodes, each 
#' of which has a conditional probability (the probability of following
#' that branch given that the node has been reached). 
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
ChanceNode <- R6::R6Class(
  classname = "ChanceNode",
  inherit = Node,
  private = list(
  ),
  public = list(
    
    #' @description
    #' Create a new ChanceNode object
    #' @param label An optional label for the chance node.
    #' @return A new `ChanceNode` object
    initialize = function(label="") {
      # ensure base class fields are initialized
      super$initialize(label)
      # return a ChanceNode object
      return(invisible(self))
    }
  )
)
