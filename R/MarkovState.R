#' @title A state in a Markov model
#' @description An R6 class representing a state in a Markov model.
#' @details  Represents a single state in a Markov model. A Markov model is 
#' a digraph in which states are nodes and transitions are arrows. Inherits 
#' from class \code{Node}.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
MarkovState <- R6::R6Class(
  classname = "MarkovState", 
  inherit = Node,
  lock_class = TRUE,
  private = list(
    state.cost = NULL,
    state.utility = NULL
  ),
  public = list(
    
    #' @description Create an object of type \code{MarkovState}.
    #' @param name The name of the state (character string).
    #' @param cost The annual cost of state occupancy (numeric or 
    #' \code{ModVar}). Default 0.
    #' @param utility The utility associated with being in the state (numeric
    #' or \code{ModVar}).
    #' @details Utility must be in the range \code{[-Inf,1]}. If it is of type 
    #' numeric, the range is checked on object creation. 
    #' @return An object of type \code{MarkovState}.
    initialize = function(name, cost = 0.0, utility = 1.0) {
      # check and set the name
      abortifnot(
        !(missing(name) || is.na(name)),
        message = "State name must not be missing", 
        class = "missing_state_name"
      )
      abortifnot(
        is.character(name), 
        message = "State name must be a string", 
        class = "non-string_state_name"
      )
      # ensure base class is initialized
      super$initialize(name)
      # check that annual cost is numeric, then set it
      self$set_cost(cost)
      # check the utility, and in range[-Inf,1] if numeric
      abortifnot(
        (is.numeric(utility) && utility <= 1.0) || is_ModVar(utility),
        message = "If utility is numeric it must be in the range [-Inf,1]",
        class = "invalid_utility"
      )
      private$state.utility <- utility
      # return invisible MarkovState object
      return(invisible(self))
    },

    #' @description Accessor function to retrieve the state name.
    #' @return State name.
    name = function() {
      return(self$label())
    },
    
    #' @description Set the annual occupancy cost
    #' @param cost The annual cost of state occupancy
    #' @returns Updated \code{MarkovState} object
    set_cost = function(cost) {
      # check that annual cost, then set it
      abortifnot(
        is.numeric(cost) || is_ModVar(cost),
        message = "'cost' must be of type 'numeric' or 'ModVar'",
        class = "invalid_annual_cost"
      )
      private$state.cost <- cost
      return(invisible(self))
    },
    
    #' @description Gets the annual cost of state occupancy.
    #' @return Annual cost; numeric. 
    cost = function() {
      rv <- as_numeric(private$state.cost)
      return(rv)
    },
    
    #' @description Gets the utility associated with the state.
    #' @return Utility; numeric.
    utility = function() {
      rv <- as_numeric(private$state.utility)
      return(rv)
    },
    
    #' @description Find all the model variables.
    #' @details Find variables of type \code{ModVar} that have been 
    #' specified as values associated with this \code{MarkovState}. 
    #' Includes operands of these \code{ModVar}s, if they are expressions.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # create lists of input variables and output Modvars
      iv <- c(private$state.cost, private$state.utility)
      ov <- list()
      for (v in iv) {
        if (is_ModVar(v)) {
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
    }
  )
)
