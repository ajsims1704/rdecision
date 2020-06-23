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
    edge.cost = NULL,
    edge.benefit = 0
  ),
  public = list(
    
    #' @description
    #' Create an object of type 'Action'. Optionally, a cost and a benefit may 
    #' be associated with traversing the edge. A \dfn{payoff} (benefit-cost) 
    #' is sometimes used in edges of decision trees; the parametrization used 
    #' here is more general.
    #' @param source Decision node from which the arrow leaves.
    #' @param target Node which the arrow enters.
    #' @param label Character string containing the arrow label. This
    #' @param cost Cost associated with traversal of this edge.
    #' @param benefit Benefit associated with traversal of the edge.
    #' must be defined for an action because the label is used in
    #' tabulation of strategies.
    #' @return A new \code{Action} object.
    initialize = function(source, target, label, cost=0, benefit=0) {
      # check label
      if (!is.character(label)) {
        rlang::abort("Argument label must be a string", class="non-string_label")
      }
      if (nchar(label)==0) {
        rlang::abort("Argument label must be defined", class="empty_label")
      }
      # initialize base class
      super$initialize(source=source, target=target, label=label)
      # check that source inherits from DecisionNode
      if (!inherits(source, what="DecisionNode")) {
        rlang::abort("Node 'source' must be a DecisionNode", class="non-Decision_source")
      }
      # check and set cost
      if (!inherits(cost, what=c("numeric", "ModVar"))){
        rlang::abort("Argument 'cost' must be of type 'numeric' or 'ModVar'.",
                     class = "incorrect_type")
      }
      private$edge.cost <- cost
      # check and set benefit
      if (!inherits(benefit, what=c("numeric", "ModVar"))){
        rlang::abort("Argument 'benefit' must be of type 'numeric' or 'ModVar'.",
                     class = "incorrect_type")
      }
      private$edge.benefit <- benefit
      # Return Action node
      return(invisible(self))
    }
  
  )
)
