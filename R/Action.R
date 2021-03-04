#' @title \verb{Action} class
#' 
#' @description
#' An R6 class to represent an action (choice) edge in a decision tree.
#' 
#' @details A specialism of class Arrow which is used in a decision tree to
#' represent edges with source nodes joined to \verb{DecisionNode}s.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
Action <- R6::R6Class(
  classname = "Action",
  lock_class = TRUE,
  inherit = Arrow,
  private = list(
    edge.cost = NULL,
    edge.benefit = NULL
  ),
  public = list(
    
    #' @description
    #' Create an object of type 'Action'. Optionally, a cost and a benefit may 
    #' be associated with traversing the edge. A \dfn{pay-off} (benefit minus 
    #' cost)  is sometimes used in edges of decision trees; the parametrization
    #' used here is more general.
    #' @param source Decision node from which the arrow leaves.
    #' @param target Node which the arrow enters.
    #' @param label Character string containing the arrow label. This
    #' must be defined for an action because the label is used in
    #' tabulation of strategies.
    #' @param cost Cost associated with traversal of this edge.
    #' @param benefit Benefit associated with traversal of the edge.
    #' @return A new \verb{Action} object.
    initialize = function(source, target, label, cost=0, benefit=0) {
      # check label
      if (!is.character(label)) {
        rlang::abort(
          "Argument label must be a string", 
          class="invalid_label"
        )
      }
      if (nchar(label)==0) {
        rlang::abort("Argument label must be defined", class="empty_label")
      }
      # initialize base class
      super$initialize(source=source, target=target, label=label)
      # check that source inherits from DecisionNode
      if (!inherits(source, what="DecisionNode")) {
        rlang::abort(
          "Node 'source' must be a DecisionNode", 
          class="invalid_source"
        )
      }
      # check and set cost, ensuring initialization
      if (inherits(cost, what="numeric")) {
        private$edge.cost <- cost
      } else if (inherits(cost, "ModVar")) {
        private$edge.cost <- cost
      } else {
        rlang::abort(
          "Argument 'cost' must be of type 'numeric' or 'ModVar'.",
          class = "invalid_cost"
        )
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
      # Return Action node
      return(invisible(self))
    },
    
    #' @description 
    #' Find all the model variables of type \verb{ModVar} that have been 
    #' specified
    #' as values associated with this Action. Includes operands of these
    #' \verb{ModVar}s, if they are expressions.
    #' @return A list of \verb{ModVar}s.
    modvars = function() {
      # create lists of input variables and output ModVars
      iv <- c(private$edge.cost, private$edge.benefit)
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
    #' @return Numeric value equal to 1.
    p = function() {
      return(1)
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
