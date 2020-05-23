#' @title 
#' TransitionMatrix
#' 
#' @description 
#' An R6 class for a transition matrix in Markov model
#' 
#' @details 
#' A class to represent a matrix of annual transition rates in a Markov
#'  model. It wraps a regular numeric matrix, and provides methods for 
#' convenience and runtime checks. States are referred to by name, rather 
#' than by index name. Name them well!
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
TransitionMatrix <- R6::R6Class(
  classname = "TransitionMatrix",
  
  private = list(
    statenames = vector(mode="character", length=0),
    i = NULL # effective annual transition rates
  ),
  
  public = list(

    #' @description 
    #' Create a new state transition matrix.
    #' @param statenames A character vector of state names.
    #' @return A new TransitionMatrix object.     
    initialize = function(statenames) {
      # check that statenames is a character vector  
      if (any(is.na(statenames))) {
        rlang::abort("TransitionMatrix$new(): no state name can be missing")
      }
      if (!is.character(statenames)) {
        rlang::abort("TransitionMatrix$new(): state names must be a character vector")
      } 
      private$statenames <- statenames
      # create matrix of effective annual transition rates
      nstates <- length(private$statenames)
      private$i <- matrix(
        data = vector(mode="list", length=nstates*nstates),
        nrow = nstates,
        ncol = nstates,
        byrow = TRUE,
        dimnames = list(private$statenames, private$statenames)
      )
      # return object 
      return(invisible(self))
    },
    
    #' @description 
    #' Return list of state names
    #' @return Character vector of state names.
    get_statenames = function() {
      return(private$statenames)
    },
    
    #' @description 
    #' Set the annual transition rate between two states
    #' @param from Name of the 'from' state.
    #' @param to Name of the 'to' state.
    #' @param rate Annual transition rate between the states. Must be a numeric
    #' value or a ModVar.
    #' @return Updated TransitionMatrix object.
    set_transition = function(from, to, rate) {
      # check that 'from' and 'to' are state names
      if (is.na(from) || !is.character(from) || !(from %in% private$statenames)) {
        rlang::abort("TransitionMatrix$set_transition(): 'from' must be a state name")
      }
      if (is.na(to) || !is.character(to) || !(to %in% private$statenames)) {
        rlang::abort("TransitionMatrix$set_transition(): 'to' must be a state name")
      }
      # check that rate is numeric or ModVar
      if (!is.numeric(rate) && !inherits(rate, what='ModVar')) {
        rlang::abort("TransitionMatrix$set_transition(): 'rate' must be numeric or ModVar")
      }
      private$i[from,to] <- rate
      # return updated object
      return(invisible(self))
    }
    
  )
)
