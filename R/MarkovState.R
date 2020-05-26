#' @title 
#' MarkovState
#' 
#' @description
#' An R6 class for a state in a Markov model
#' 
#' @details 
#' Class to represent a single state in a Markov model. 
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
MarkovState <- R6::R6Class(
  classname = "MarkovState", 
  private = list(
    name = "",
    isTemporary = FALSE,
    isAbsorbing = FALSE,
    cycleLimit = NA,
    annualCost = 0,
    entryCost = 0
  ),
  public = list(
    
    #' @description 
    #' Create a Markov state object.
    #' @param name The name of the state (character string).
    #' @param entryCost The cost to enter the state.
    #' @param annualCost The annual cost of state occupancy.
    #' @param cycleLimit The maximum length of stay;
    #' numeric. Leave unset for no cycle limit (normal behaviour).
    #' @param absorbing Logical; TRUE if the state is an absorbing state,
    #' FALSE otherwise. Must be FALSE (default) if cycleLimit is set.
    #' @return An object of type MarkovState.
    initialize = function(name, annualCost=0, entryCost=0, cycleLimit=NA,
                          absorbing=FALSE) {
      # set the name
      if (is.na(name)) {
        rlang::abort("State name must not be missing", class="missing_state_name")
      }
      if (!is.character(name)) {
        rlang::abort("State name must be a string", class="non-string_state_name")
      }
      private$name <- name 
      # set the cycle limit status and value (for temporary states)
      if (!is.na(cycleLimit)) {
        if (!is.numeric(cycleLimit)) {
          rlang::abort("Argument 'cycleLimit' must be numeric", 
                       class="non-numeric_cycle_limit")
        }
        if (abs(cycleLimit-round(cycleLimit)) > .Machine$double.eps^0.5) {
          rlang::abort("Argument 'cycleLimit' must be a whole number",
                       class="non-integer_cycle_limit")
        }
        private$cycleLimit <- cycleLimit
        private$isTemporary <- TRUE
      }
      # check and set absorbing status
      if (!is.logical(absorbing)) {
        rlang::abort("Argument 'absorbing' must be logical",
                     class="non-logical_absorbing_status")  
      }
      if (absorbing && private$isTemporary) {
        rlang::abort("Must not define a state as temporary *and* absorbing",
                     class="temporary_absorbing_conflict")
      }
      private$isAbsorbing <- absorbing
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
    has_cycle_limit = function() {
      return(private$isTemporary)
    },
    
    #' @description 
    #' Accessor function to find the cycle limit.
    #' @return The cycle limit; integer.
    get_cycle_limit = function() {
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