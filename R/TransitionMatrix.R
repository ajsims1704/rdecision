#' @title 
#' TransitionMatrix
#' 
#' @description 
#' An R6 class for a transition matrix in Markov model
#' 
#' @details 
#' A class to represent a matrix of annual transition rates in a Markov
#' model. It wraps a regular numeric matrix, and provides methods for 
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
      # check that state names are non-missing and characters  
      sapply(statenames, function(n) {
        if (is.na(n)) {
          rlang::abort("State names must not be missing.", 
                       class="missing_state_name")
        }
        if (!is.character(n)) {
          rlang::abort("All elements of statenames must be strings.", 
                       class="non-string_state_name")
        }
      })
      private$statenames <- statenames
      # create matrix (list with attributes) of effective annual transition rates
      nstates <- length(private$statenames)
      private$i <- matrix(
        data = vector(mode="list", length=nstates*nstates),
        nrow = nstates,
        ncol = nstates,
        byrow = TRUE,
        dimnames = list(from=private$statenames, to=private$statenames)
      )
      # set initial matrix with leading diagonals as NA, others as zero
      for (i in statenames) {
        for (j in statenames) {
          private$i[i,j] <- ifelse(i==j, NA, 0)
        }
      }
      # return object 
      return(invisible(self))
    },

    #' @description 
    #' Return the number of rows in the matrix
    #' @return Row count.
    nrow = function() {
      return(nrow(private$i))      
    },
    
    #' @description 
    #' Return the number of columns in the matrix.
    #' @return Row count.
    ncol = function() {
      return(ncol(private$i))      
    },
    
    #' @description 
    #' Return list of state names (as per matrix dimnames).
    #' @return Named list of length 2 ('from' and 'to').
    dimnames = function() {
      return(dimnames(private$i))
    },
    
    #' @description 
    #' Set the annual transition rate between two states
    #' @param from Name of the 'from' state.
    #' @param to Name of the 'to' state.
    #' @param rate Annual transition rate between the states. Must be a numeric
    #' value or a ModVar.
    #' @return Updated TransitionMatrix object.
    set_rate = function(from, to, rate) {
      # check that 'from' and 'to' are state names
      if (is.na(from) || !is.character(from) || !(from %in% private$statenames)) {
        rlang::abort("Argument 'from' must be a state name.", class="undefined_state_name")
      }
      if (is.na(to) || !is.character(to) || !(to %in% private$statenames)) {
        rlang::abort("Argument 'to' must be a state name.", class="undefined_state_name")
      }
      # check that rate is numeric or ModVar
      if (!is.numeric(rate) && !inherits(rate, what='ModVar')) {
        rlang::abort("Argument 'rate' must be numeric or ModVar.", class="non-numeric_rate")
      }
      # if rate is numeric, check if in range
      if (is.numeric(rate) && ((rate<0) || (rate>1))) {
        rlang::abort("Numeric 'rate' must be [0,1].", class="numeric_rate_out_of_range")
      } 
      # set the rate
      private$i[from,to] <- rate
      # return updated object
      return(invisible(self))
    },
    
    #' @description 
    #' Calculate and return the current numeric representation of the 
    #' transition matrix. If the transition matrix was defined with one or
    #' more transitions as model variables, the current value of these
    #' is retrieved using 'value()'. The single NA in each row is replaced
    #' with 1-p where P is the sum of the other values in the row.
    #' @return A numeric matrix with rows and columns labelled with 
    #' state names.
    value = function() {
      # fetch numeric representation of the transition matrix
      ML <- sapply(private$i, FUN=function(cell) {
        rv <- NA
        if (inherits(cell, what="ModVar")) {
          rv <- cell$value()
        } else {
          rv <- cell
        }
        return(rv)
      })
      str(ML)
      # convert to matrix
      MM <- matrix(ML, nrow=self$nrow(), ncol=self$ncol(), byrow=TRUE, 
                   dimnames=self$dimnames()) 
      print(MM)
      # replace each NA to ensure sum of probabilities is one
      # return numeric matrix
      return(MM)
    }
    
  )
)
