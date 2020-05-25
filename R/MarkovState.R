#' @title 
#' MarkovState
#' 
#' @description
#' An R6 class for a state in a Markov model
#' 
#' @details 
#' A class to represent a single state in a Markov model. 
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
MarkovState <- R6::R6Class(
  classname = "MarkovState", 
  private = list(
    name = "",
    isTemporaryState = "logical",
    cycleLimit = "numeric",
    annualCost = "numeric",
    entryCost = "numeric"
  ),
  public = list(
    
    #' @description 
    #' Create a Markov state object.
    #' @param name The name of the state (character string).
    #' @param isTemporaryState Logical; TRUE if the state is a tunnel state.
    #' @param cycleLimit The maximum length of stay (1 for cohort tunnel states); numeric.
    #' @param entryCost The cost to enter the state.
    #' @param hasCycleLimit Whether the state has a cycle limit; logical.
    #' @param annualCost The annual cost of state occupancy.
    #' @return An object of type MarkovState.
    initialize = function(name, annualCost=0, entryCost=0, hasCycleLimit=F, cycleLimit=NA) {
      # set the name
      if (is.na(name)) {
        rlang::abort("State name must not be missing", class="missing_state_name")
      }
      if (!is.character(name)) {
        rlang::abort("State name must be a string", class="non-string_state_name")
      }
      private$name <- name 
      # set the cycle limit state and value (for tunnel states)
      private$isTemporaryState <- hasCycleLimit
      private$cycleLimit <- cycleLimit
      if (private$isTemporaryState & is.na(cycleLimit)) {
        private$cycleLimit <- 1
      }
      # check that annual cost is numeric, then set it
      if (!is.numeric(annualCost)){
        stop("`annualCost` must be of type `numeric`")
      }
      private$annualCost<- annualCost
      # check that entry cost is numeric, then set it
      if (!is.numeric(entryCost)){
        stop("`entryCost` must be of type `numeric`")
      }
      private$entryCost <- entryCost
    },
    
    #' @description 
    #' Accessor function to retrieve the state name.
    #' @return State name.
    get_name = function() {
      return(private$name)
    },
    
    #' @description
    #' Accessor function to identify if the state has a cycle limit.
    #' @return TRUE if temporary state; false otherwise.
    hasCycleLimit = function() {
      return(private$isTemporaryState)
    },
    
    #' @description 
    #' Accessor function to find the cycle limit.
    #' @return The cycle limit; integer.
    getCycleLimit = function() {
      return(private$cycleLimit)
    },
    
    #' @description 
    #' Sets the annual cost of state occupancy.
    #' @param annualCost Annual cost of occupying the state; numeric.
    #' @return Updated MarkovState object.
    setAnnualCost = function(annualCost) {
      # check that cost is numeric, then set it
      if (!is.numeric(annualCost)){
        stop("`annualCost` must be of type `numeric`")
      }
      private$annualCost <- annualCost
      return(invisible(self))
    },
    
    #' @description 
    #' Gets the annual cost of state occupancy.
    #' @return Annual cost; numeric.
    getAnnualCost = function() {
      return(private$annualCost)
    },
    
    #' @description 
    #' Sets the entry cost of state occupancy.
    #' @param entryCost Cost to enter the state; numeric.
    #' @return Updated MarkovState object.
    setEntryCost = function(entryCost) {
      # check that cost is numeric, then set it
      if (!is.numeric(entryCost)){
        stop("`entryCost` must be of type `numeric`")
      }
      private$entryCost <- entryCost
      return(invisible(self))
    },
    
    #' @description 
    #' Gets the entry cost of state occupancy.
    #' @return Entry cost; numeric.
    getEntryCost = function() {
      return(private$entryCost)
    }
  )
)