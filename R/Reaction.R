#' @title A reaction (chance) edge in a decision tree
#' @description An R6 class representing a reaction (chance) edge in a decision 
#' tree.
#' @details A specialism of class \code{Arrow} which is used in a decision tree
#' to represent edges whose source nodes are \code{ChanceNode}s.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
Reaction <- R6::R6Class(
  classname = "Reaction",
  lock_class = TRUE,
  inherit = Arrow,
  private = list(
    edge.cost = NULL,
    edge.benefit = NULL,
    edge.p = NULL
  ),
  public = list(
    
    #' @description Create an object of type \code{Reaction}. A probability 
    #' must be assigned
    #' to the edge. Optionally, a cost and a benefit may be associated
    #' with traversing the edge. A \dfn{pay-off} (benefit-cost) is sometimes
    #' used in edges of decision trees; the parametrization used here is more
    #' general.
    #' @param source_node Chance node from which the reaction leaves.
    #' @param target_node Node which the reaction enters.
    #' @param p Probability
    #' @param cost Cost associated with traversal of this edge.
    #' @param benefit Benefit associated with traversal of the edge.
    #' @param label Character string containing the reaction label.
    #' @return A new \code{Reaction} object.
    initialize = function(source_node, target_node, p, cost = 0.0, 
                          benefit = 0.0, label = "") {
      # initialize base class
      super$initialize(
        source_node = source_node, target_node = target_node, label = label
      )
      # check that source inherits from ChanceNode
      abortifnot(inherits(source_node, what = "ChanceNode"),
        message = "Node 'source' must be a ChanceNode", 
        class = "invalid_source"
      )
      # check and set p, ensuring initialization
      abortifnot(inherits(p, what = c("numeric", "ModVar")),
        message = "Argument 'p' must be of type 'numeric' or 'ModVar'.",
        class = "invalid_p"
      )
      private$edge.p <- p
      # check and set cost, ensuring initialization
      abortifnot(inherits(cost, what = c("numeric", "ModVar")),
        message = "Argument 'cost' must be of type 'numeric' or 'ModVar'.",
        class = "invalid_cost"
      )
      private$edge.cost <- cost
      # check and set benefit, ensuring initialization
      abortifnot(inherits(benefit, what = c("numeric", "ModVar")),
        message = "Argument 'benefit' must be of type 'numeric' or 'ModVar'.",
        class = "invalid_benefit"
      )
      private$edge.benefit <- benefit
      # Return reaction node
      return(invisible(self))
    },
    
    #' @description Find all the model variables of type \code{ModVar} that 
    #' have been specified as values associated with this Action. Includes 
    #' operands of these \code{ModVar}s, if they are expressions.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # create lists of input variables and output Modvars
      iv <- c(private$edge.cost, private$edge.benefit, private$edge.p)
      ov <- list()
      for (v in iv) {
        if (inherits(v, what="ModVar")) {
          ov <- c(ov, v)
          if (inherits(v, what="ExprModVar")) {
            for (o in v$operands()) {
              ov <- c(ov, o)
            }
          } 
        }
      }
      # return the unique list
      return(unique(ov))
    },

    #' @description Return the current value of the edge probability, i.e. the
    #' conditional' probability of traversing the edge.
    #' @return Numeric value in range [0,1].
    p = function() {
      prob <- as_numeric(private$edge.p)
      return(prob)
    },
    
    #' @description 
    #' Return the cost associated with traversing the edge.
    #' @return Cost.
    cost = function() {
      rv <- as_numeric(private$edge.cost)
      return(rv)
    },
    
    #' @description 
    #' Return the benefit associated with traversing the edge.
    #' @return Benefit.
    benefit = function() {
      rv <- as_numeric(private$edge.benefit)
      return(rv)
    }  
  )
)
