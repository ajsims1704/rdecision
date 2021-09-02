#' @title A transition in a semi-Markov model
#' 
#' @description An R6 class representing a transition in a semi-Markov model.
#' 
#' @details A specialism of class \code{Arrow} which is used in a semi-Markov
#' model to represent a transition between two \code{MarkovState}s. The 
#' transition is optionally associated with a cost. The transition probability
#' is associated with the model (\code{SemiMarkovModel}) rather than the
#' transition.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
Transition <- R6::R6Class(
  classname = "Transition",
  lock_class = TRUE,
  inherit = Arrow,
  private = list(
    transition.cost = NULL
  ),
  public = list(
    
    #' @description Create an object of type \code{MarkovTransition}. 
    #' @param source \code{MarkovState} from which the transition starts.
    #' @param target \code{MarkovState} to which the transition ends.
    #' @param cost Cost associated with the transition. 
    #' @param label Character string containing a label for the transition (the
    #' name of the event).
    #' @return A new \code{Transition} object.
    initialize = function(source, target, cost=0, label="") {
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
      iv <- c(private$transition.cost)
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

    #' @description Set the cost associated with the transition.
    #' @param c Cost associated with the transition.
    #' @return Updated \code{Transition} object.
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
