#' @title 
#' Reaction
#' 
#' @description
#' An R6 class to represent a reaction (chance) edge in a decision tree.
#' 
#' @details A specialism of class Arrow which is used in a decision tree to
#' represent edges with source nodes joined to \code{ChanceNode}s.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
Reaction <- R6::R6Class(
  classname = "Reaction",
  inherit = Arrow,
  private = list(
    p = NULL
  ),
  public = list(
    
    #' @description
    #' Create an object of type 'Reaction'.
    #' @param source Chance node from which the arrow leaves.
    #' @param target Node which the arrow enters.
    #' @param p Probability
    #' @param label Character string containing the arrow label.
    #' @return A new \code{Reaction} object.
    initialize = function(source, target, p, label="") {
      # initialize base class
      super$initialize(source=source, target=target, label=label)
      # check that source inherits from ChanceNode
      if (!inherits(source, what="ChanceNode")) {
        rlang::abort("Node 'source' must be a ChanceNode", class="non-Chance_source")
      }
      # Check and save p value
      if (is.numeric(p)){
      } else if (inherits(p,what="ModVar")){
      } else {
        rlang::abort("Argument 'p' must be of type 'numeric' or 'ModVar'.",
                     class = "incorrect_p_type")
      }
      private$p <- p
      # Return reaction node
      return(invisible(self))
    }
  
  )
)
