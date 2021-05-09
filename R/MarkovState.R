#' @title \verb{MarkovState} class
#' 
#' @description
#' An R6 class for a state in a Markov model.
#' 
#' @details 
#' Represents a single state in a Markov model. A Markov model is a digraph
#' in which states are nodes and transitions are arrows. 
#' 
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
    #' @param cost The annual cost of state occupancy.
    #' @param utility The utility associated with being in the state.
    #' @return An object of type \code{MarkovState}.
    initialize = function(name, cost=0, utility=1) {
      # set the name
      if (missing(name) || is.na(name)) {
        rlang::abort(
          "State name must not be missing", 
          class="missing_state_name"
        )
      }
      if (!is.character(name)) {
        rlang::abort(
          "State name must be a string", 
          class="non-string_state_name"
        )
      }
      # ensure base class is initialized
      super$initialize(name)
      # check that annual cost is numeric, then set it
      if (!is.numeric(cost)){
        rlang::abort(
          "'cost' must be of type 'numeric'",
          class="non-numeric_annual_cost"
        )
      }
      private$state.cost <- cost
      # check the utility is numeric, and in range[-Inf,1], and set it
      if (!is.numeric(utility)) {
        rlang::abort(
          "Argument 'utility; must be numeric", 
          class="non-numeric_utility"
        )
      }
      if (utility > 1) {
        rlang::abort("Utility must be in the range [-Inf,1]",
                     class="utility_out_of_range")
      }
      private$state.utility <- utility
      # return invisible MarkovState object
      return(invisible(self))
    },
    
    #' @description Accessor function to retrieve the state name.
    #' @return State name.
    get_name = function() {
      return(self$label())
    },
    
    #' @description Sets the annual cost of state occupancy.
    #' @param cost Annual cost of occupying the state; numeric.
    #' @return Updated MarkovState object.
    set_cost = function(cost) {
      # check that cost is numeric, then set it
      if (!is.numeric(cost)){
        rlang::abort("`cost` must be of type `numeric`",
                     class = "non_numeric_annual_cost")
      }
      private$state.cost <- cost
      return(invisible(self))
    },
    
    #' @description 
    #' Gets the annual cost of state occupancy.
    #' @return Annual cost; numeric.
    get_cost = function() {
      return(private$state.cost)
    },
    
    #' @description 
    #' Gets the utility associated with the state.
    #' @return Utility; numeric.
    get_utility = function() {
      return(private$state.utility)
    }
  )
)
