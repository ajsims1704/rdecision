#' @title A chance node in a decision tree
#'
#' @description An R6 class representing a chance node in a decision tree.
#'
#' @details A chance node is associated with at least two branches to other
#' nodes, each of which has a conditional probability (the probability of
#' following that branch given that the node has been reached). Inherits from
#' class \code{Node}.
#'
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'
ChanceNode <- R6::R6Class(
  classname = "ChanceNode",
  lock_class = TRUE,
  inherit = Node,
  private = list(
  ),
  public = list(

    #' @description Create a new \code{ChanceNode} object
    #' @param label An optional label for the chance node.
    #' @return A new \code{ChanceNode} object
    initialize = function(label = "") {
      # ensure base class fields are initialized
      super$initialize(label)
      # return a ChanceNode object
      return(invisible(self))
    }
  )
)
