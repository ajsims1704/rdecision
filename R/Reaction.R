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
    edge.cost = NULL,
    edge.benefit = NULL,
    edge.p = NULL
  ),
  public = list(
    
    #' @description
    #' Create an object of type 'Reaction'. A probability must be assigned
    #' to the edge. Optionally, a cost and a benefit may be associated
    #' with traversing the edge. A \dfn{payoff} (benefit-cost) is sometimes
    #' used in edges of decision trees; the parametrization used here is more
    #' general.
    #' @param source Chance node from which the arrow leaves.
    #' @param target Node which the arrow enters.
    #' @param p Probability
    #' @param cost Cost associated with traversal of this edge.
    #' @param benefit Benefit associated with traversal of the edge.
    #' @param label Character string containing the arrow label.
    #' @return A new \code{Reaction} object.
    initialize = function(source, target, p, cost=0, benefit=0, label="") {
      # initialize base class
      super$initialize(source=source, target=target, label=label)
      # check that source inherits from ChanceNode
      if (!inherits(source, what="ChanceNode")) {
        rlang::abort("Node 'source' must be a ChanceNode", class="non-Chance_source")
      }
      # Check and save p value
      if (!inherits(p, what=c("numeric", "ModVar"))){
        rlang::abort("Argument 'p' must be of type 'numeric' or 'ModVar'.",
                     class = "incorrect_type")
      }
      private$edge.p <- p
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
      # Return reaction node
      return(invisible(self))
    },
    
    #' @description
    #' Return the current value of the edge probability.
    #' @return Numeric value in range [0,1].
    p = function() {
      prob <- 0
      if (inherits(private$edge.p, what="ModVar")) {
        prob <- private$edge.p$value()
      } else {
        prob <- private$edge.p
      }
      return(prob)
    }
  
  )
)
