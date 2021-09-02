#' @title A transition in a Markov model
#' 
#' @description An R6 class representing a transition in a Markov model.
#' 
#' @details A specialism of class \code{Arrow} which is used in a Markov model
#' to represent a transition between two \code{MarkovState}s. The transition
#' is associated with an instantaneous hazard rate and optionally, a cost. The
#' hazard rate is the rate of failure at the next instant given survival up to
#' the current time. 
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
MarkovTransition <- R6::R6Class(
  classname = "MarkovTransition",
  lock_class = TRUE,
  inherit = Arrow,
  private = list(
    transition.rate = NULL,
    transition.cost = NULL
  ),
  public = list(
    
    #' @description Create an object of type \code{MarkovTransition}. 
    #' @details A hazard rate must be assigned to the transition. Optionally,
    #' a cost may be associated with the transition.
    #' @param source \code{MarkovState} from which the transition starts.
    #' @param target \code{MarkovState} to which the transition ends.
    #' @param r Instantaneous hazard rate in units of per patient per year.
    #' To meet the condition that the sum of the outgoing transition rates from
    #' each state is zero, one transition rate from a state may be set to NA
    #' when the model is defined. The default is NA. 
    #' @param cost Cost associated with the transition. 
    #' @param label Character string containing a label for the transition (the
    #' name of the event).
    #' @return A new \code{MarkovTransition} object.
    initialize = function(source, target, r=as.numeric(NA), cost=0, label="") {
      # initialize base class
      super$initialize(source=source, target=target, label=label)
      # check that source inherits from MarkovState
      if (!inherits(source, what="MarkovState")) {
        rlang::abort(
          "Node 'source' must be a MarkovState", 
          class="invalid_source"
        )
      }
      # check that target inherits from MarkovState
      if (!inherits(target, what="MarkovState")) {
        rlang::abort(
          "Node 'target' must be a MarkovState", 
          class="invalid_target"
        )
      }
      # check and set r, ensuring initialization
      self$set_rate(r)
      # check and set cost, ensuring initialization
      self$set_cost(cost)
      # Return reaction node
      return(invisible(self))
    },

    #' @description Find all the model variables.
    #' @details Find variables of type \code{ModVar} that have been 
    #' specified as values associated with this \code{MarkovTransition}. 
    #' Includes operands of these \code{ModVar}s, if they are expressions.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # create lists of input variables and output Modvars
      iv <- c(private$transition.cost, private$transition.rate)
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

    #' @description Set the value of the transition rate
    #' @param r Instantaneous hazard rate in units of per patient per year,
    #' or NA (the default).
    #' @return Updated \code{MarkovTransition} object.
    set_rate = function(r=as.numeric(NA)) {
      # check r and set private variable
      if (inherits(r, what="numeric")) {
        private$transition.rate <- r
      } else if (inherits(r, "ModVar")) {
        private$transition.rate <- r
      } else {
        rlang::abort("Argument 'r' must be of type 'numeric' or 'ModVar'.",
                     class = "invalid_rate")
      }
      return(invisible(self))
    },
    
    #' @description Return the value of the hazard rate.
    #' @return Numeric value.
    rate = function() {
      if (inherits(private$transition.rate, what="ModVar")) {
        r <- private$transition.rate$get()
      } else {
        r <- private$transition.rate
      }
      return(r)
    },
    
    #' @description Set the cost associated with the transition.
    #' @param c Cost associated with the transition.
    #' @return Updated \code{MarkovTransition} object.
    set_cost = function(c=0) {
      # check c and set private variable
      if (inherits(c, what="numeric")) {
        private$transition.cost <- c
      } else if (inherits(c, "ModVar")) {
        private$transition.cost <- c
      } else {
        rlang::abort("Argument 'c' must be of type 'numeric' or 'ModVar'.",
                     class = "invalid_cost")
      }
      return(invisible(self))
    },
    
    #' @description Return the cost associated with traversing the edge.
    #' @return Cost.
    cost = function() {
      if (inherits(private$transition.cost, what="ModVar")) {
        rv <- private$transition.cost$get()
      } else {
        rv <- private$transition.cost
      }
      return(rv)
    }
    
  )
)
