#' @title A directed edge in a digraph
#'
#' @description An R6 class representing an directed edge in a digraph.
#'
#' @details An arrow is the formal term for an edge between pairs of nodes in a
#' directed graph. Inherits from class \code{Edge}.
#'
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'
Arrow <- R6::R6Class(
  classname = "Arrow",
  inherit = Edge,
  private = list(
  ),
  public = list(

    #' @description Create an object of type \code{Arrow}.
    #' @param source_node Node from which the arrow leaves.
    #' @param target_node Node to which the arrow points.
    #' @param label Character string containing the arrow label.
    #' @return A new \code{Arrow} object.
    initialize = function(source_node, target_node, label = "") {
      # base class
      super$initialize(v1 = source_node, v2 = target_node, label = label)
      return(invisible(self))
    },

    #' @description Access source node.
    #' @return Node from which the arrow leads.
    source = function() {
      return(private$v1)
    },

    #' @description Access target node.
    #' @return Node to which the arrow points.
    target = function() {
      return(private$v2)
    }
  )
)
