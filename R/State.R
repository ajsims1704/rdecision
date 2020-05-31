#' @title
#' State
#' 
#' @description 
#' An R6 class for a leaf node in a decision tree representing a clinical state.
#'
#' @details A State is a clinical outcome, the end point (leaf node or terminal node)
#' of a decision tree and the base class for a Markov state. It represents a state of
#' being, and is associated with an annual cost and an incremental utility. It 
#' inherits from class Node so that it can be part of a decision tree, either as
#' a leaf node with no children or a Markov state, in the construction of a 
#' Markov cycle tree. 
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' 
#' @export
#' 
State <- R6::R6Class(
  classname = "State",
  inherit = Node,
  private = list(
    name = "",
    cost = 0,
    utility = 1,
    interval = as.difftime(tim=365.25, units="days")
  ),
  public = list(
    
    #' @description
    #' Create a new `State` object; synonymous with a clinical outcome.
    #' @param name Character string; a label for the state.
    #' @param cost The cost of being in the state over the interval.
    #' @param utility the incremental utility that a user associates with
    #' being in the health state (range -Inf to 1) for the interval.
    #' @param interval The time interval, over which the 'cost' and
    #' 'utility' parameters apply, expressed as an R 'difftime' object;
    #' default 1 year.
    #' @return A new `State` object
    initialize = function(name, cost=0, utility=1, 
                          interval=as.difftime(365.25, units="days")) {
      super$initialize()
      private$name <- name
      # check and set cost
      if (!is.numeric(cost) && !inherits(cost, what="ModVar")) {
        rlang::abort("Argument 'cost' must be of type 'numeric' or 'ModVar'")
      }
      private$cost <- cost    
      # check and set utility
      if (!is.numeric(utility)) {
        rlang::abort("Argument 'utility' must be a numeric value.")
      }
      if (utility > 1) {
        rlang.abort("Argument 'utility' must be in the range [-Inf,1].")
      }
      private$utility <- utility
      # check and set the interval
      if (class(interval) != 'difftime') {
        rlang::abort("Argument 'interval' must be of class 'difftime'.")
      }
      private$interval <- interval
    },
    
    #' @description 
    #' Return the label of the state; the name of the clinical outcome.
    #' @return Name of the clinical outcome or state; character string.
    get_name = function() {
      return(private$name)
    },
    
    #' @description 
    #' Return the cost incurred by being in the state for the interval.
    #' @return Cost, as a numeric value.
    get_cost = function() {
      rv <- private$cost
      if (inherits(rv, what="ModVar")) {
        rv <- rv$value()
      } 
      return(rv)
    },
    
    #' @description 
    #' Return the incremental utility associated with being in the state for
    #' the interval.
    #' @return Incremental utility (numeric value).
    get_utility = function() {
      return(private$utility)
    }
  )
 )
