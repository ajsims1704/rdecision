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
        rlang::abort(
          "Node 'source' must be a ChanceNode", 
          class="invalid_source"
        )
      }
      # check and set p, ensuring initialization
      if (inherits(p, what="numeric")) {
        private$edge.p <- p
      } else if (inherits(p, "ModVar")) {
        private$edge.p <- p
      } else {
        rlang::abort("Argument 'p' must be of type 'numeric' or 'ModVar'.",
                     class = "invalid_p")
      }
      # check and set cost, ensuring initialization
      if (inherits(cost, what="numeric")) {
        private$edge.cost <- cost
      } else if (inherits(cost, "ModVar")) {
        private$edge.cost <- cost
      } else {
        rlang::abort("Argument 'cost' must be of type 'numeric' or 'ModVar'.",
                     class = "invalid_cost")
      }
      # check and set benefit, ensuring initialization
      if (inherits(benefit, what="numeric")) {
        private$edge.benefit <- benefit
      } else if (inherits(benefit, "ModVar")) {
        private$edge.benefit <- benefit
      } else {
        rlang::abort(
          "Argument 'benefit' must be of type 'numeric' or 'ModVar'.",
          class = "invalid_benefit"
        )
      }
      # Return reaction node
      return(invisible(self))
    },

    #' @description 
    #' Find all the model variables of type ModVar that have been specified
    #' as values associated with this Action. Includes operands of these
    #' \code{ModVar}s, if they are expressions.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # create lists of input variables and output ModVars
      iv <- c(private$edge.cost, private$edge.benefit, private$edge.p)
      ov <- list()
      sapply(iv, function(v) {
        if (inherits(v, what="ModVar")) {
          ov <<- c(ov, v)
          if (inherits(v, what="ExprModVar")) {
            ov <<- c(ov, v$operands())
          } 
        }
      })
      # return the unique list
      return(unique(ov))
    },

    #' @description
    #' Return the current value of the edge probability, i.e. the conditional
    #' probability of traversing the edge.
    #' @return Numeric value in range [0,1].
    p = function() {
      prob <- 0
      if (inherits(private$edge.p, what="ModVar")) {
        prob <- private$edge.p$get()
      } else {
        prob <- private$edge.p
      }
      return(prob)
    },
    
    #' @description 
    #' Return the cost associated with traversing the edge.
    #' @return Cost.
    cost = function() {
      if (inherits(private$edge.cost, what="ModVar")) {
        rv <- private$edge.cost$get()
      } else {
        rv <- private$edge.cost
      }
      return(rv)
    },
    
    #' @description 
    #' Return the benefit associated with traversing the edge.
    #' @return Benefit.
    benefit = function() {
      if (inherits(private$edge.benefit, what="ModVar")) {
        rv <- private$edge.benefit$get()
      } else {
        rv <- private$edge.benefit
      }
      return(rv)
    }  
  )
)
