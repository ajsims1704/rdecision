#' @title \verb{ModVar} class
#' 
#' @description An R6 class for a variable in an health economic model
#' 
#' @details Base class for a variable used in a health economic model. The base 
#' class, which is not intended to be directly instantiated by model
#' applications, wraps a numerical value which is used in calculations.
#' The base class provides a framework for creating classes of model
#' variables whose uncertainties are described by statistical distributions
#' parametrized with hyperparameters.
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export 
#' 
ModVar <- R6::R6Class(
  classname = "ModVar",
  lock_class = TRUE,
  private = list(
    .description = NULL,
    .units = NULL,
    .whats = NULL,
    .whatnext = NULL,
    .D = NULL,
    .k = NULL,
#    .val = NA,
    .value = NULL
  ),
  public = list(
    
    #' @description Create an object of type \code{ModVar}.
    #' @details A ModVar is associated with an uncertainty distribution (a
    #' "has-a" relationship in object-oriented terminology). There can be a
    #' 1-1 mapping of \code{ModVar}s to \code{Distribution}s, or several
    #' model variables can be linked to the same distribution in a
    #' many-1 mapping, e.g. when each transition probability from a Markov state
    #' is represented as a \code{ModVar} and each can be linked to the \code{k}
    #' dimensions of a common multivariate Dirichlet distribution.
    #' @param description A character string description of the variable
    #' and its role in the model. This description will be used in a
    #' tabulation of the variables linked to a model.
    #' @param units A character string description of the units, e.g. 
    #' \code{"GBP"}, \code{"per year"}.
    #' @param D The distribution representing the uncertainty in the variable.
    #' Should inherit from class \code{Distribution}, or NULL if none is
    #' defined.
    #' @param k The index of the dimension of the multivariate distribution
    #' that applies to this model variable. 
    #' @return A new \verb{ModVar} object.
    initialize = function(description, units, D=NULL, k=as.integer(1)) {
      # test and set description
      if (!is.character(description)) {
        rlang::abort("Argument 'description' must be a string", 
                     class="description_not_string")
      }
      private$.description <- description
      # test and set units
      if (!is.character(units)) {
        rlang::abort("Argument 'units' must be a string", 
                     class="units_not_string")
      }
      private$.units <- units
      # test and set distribution
      if (!is.null(D)) {
        if (!inherits(D, what="Distribution")) {
          rlang::abort(
            "'D' must inherit from Distribution",
            class = "invalid_distribution"
          )
        }
        private$.D <- D
      } else {
        private$.D <- Distribution$new("Undefined")
      }
      # test and set dimension
      if (!is.integer(k)) {
        rlang::abort(
          "'k' must be an integer",
          class = "invalid_index"
        )
      }
      if ((k <= 0) || (k > private$.D$order())) {
        rlang::abort(
          "'k' must not exceed the order of 'D'",
          class = "invalid_index"
        )
      }
      private$.k <- k
      # set possible "what" values for get()
      private$.whats <- c(
        "random", "expected", "q2.5", "q50", "q97.5", "current", "value"
      )
      # set the .value variable members
      private$.value <- rep(as.numeric(NA), times=length(private$.whats))
      names(private$.value) <- private$.whats
      private$.whatnext <- "expected"
      # return new object
      return(invisible(self))
    },

    #' @description 
    #' Is this \code{ModVar} an expression?
    #' @return \code{TRUE} if it inherits from \code{ExprModVar}, \code{FALSE}
    #' otherwise.
    is_expression = function() {
      return(inherits(self, what="ExprModVar"))
    },

    #' @description 
    #' Tests whether the model variable is probabilistic, i.e. a random
    #' variable that follows a distribution, or an expression involving
    #' random variables, some of which follow distributions. 
    #' @return \code{TRUE} if probabilistic
    is_probabilistic = function() {
      return(as.logical(NA))
    },
    
    #' @description Accessor function for the description.
    #' @return Description of model variable as character string.
    description = function() {
      return(private$.description)
    },
    
    #' @description Accessor function for units.
    #' @return Description of units as character string.
    units = function() {
      return(private$.units)
    },
    
    #' @description Accessor function for the name of the uncertainty 
    #' distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      return(private$.D$distribution())
#      return(as.character(NA))
    },
    
    #' @description 
    #' Draw a random sample from the model variable. 
    #' @param n Number of samples to draw.
    #' @return A sample drawn at random.
    r = function(n=1) {
      # return the sample
      return(rep(as.numeric(NA),n))
    },

    #' @description 
    #' Return the mean value of the distribution. 
    #' @return Mean value as a numeric value.
    mean = function() {
      return(as.numeric(NA))
    },
    
    #' @description 
    #' Return the mode of the variable. By default returns \code{NA}, which 
    #' will be the case for most \code{ExprModVar} variables, because an 
    #' arbitrary expression is not guaranteed to be unimodal.
    #' @return Mode as a numeric value.
    mode = function() {
      return(as.numeric(NA))
    },

    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    SD = function() {
      return(as.numeric(NA))
    },
    
    #' @description 
    #' Find quantiles of the uncertainty distribution. 
    #' @param probs Numeric vector of probabilities, each in range [0,1].
    #' @return Vector of numeric values of the same length as \code{probs}.
    quantile = function(probs) {
      # test argument
      sapply(probs, FUN=function(x) {
        if (is.na(x)) {
          rlang::abort("All elements of 'probs' must be defined",
                       class="probs_not_defined")
        }
        if (!is.numeric(x)) {
          rlang::abort("Argument 'probs' must be a numeric vector",
                       class="probs_not_numeric")
        }
        if (x<0 || x>1) {
          rlang::abort("Elements of 'probs' must be in range[0,1]",
                       class="probs_out_of_range")
        }
      })
      return(rep(as.numeric(NA), length(probs)))
    },

    #' @description
    #' Sets the value of the \code{ModVar} that will be returned by subsequent
    #' calls to \code{get()} until \code{set()} is called again. 
    #' @param what Character: one of \code{"random"} (samples from the 
    #' uncertainty distribution), \code{"expected"} (mean), \code{"q2.5"}
    #' (lower 95\% confidence limit), \code{"q50"} (median), \code{"q97.5"}
    #' (upper 95\% confidence limit), \code{"current"} (leaves
    #' the value unchanged), \code{"value"} (sets the value explicitly). 
    #' @param val A numeric value, only used with \code{what}=\code{"value"}, 
    #' ignored otherwise.
    #' @details The \code{"current"} option is provided to support having common 
    #' functions to set (or leave alone) sets of model variables, depending on
    #' their use case and avoids additional if statements. Option \code{"value"}
    #' is not recommended for normal usage because it allows the model variable
    #' to be set to an implausible value, based on its defined uncertainty. An 
    #' example of where this may be needed is in threshold finding. 
    #' @return Updated \code{ModVar}.
    set = function(what="random", val=NULL) {
      # check argument
      if (!is.character(what)) {
        rlang::abort(
          "'what' must be a a character string", 
          class="what_not_character"
        )
      }
      # options
      v <- NA
      if (what == "random") {
        v <- self$r()
        private$.value[what] <- v
      } else if (what == "expected") {
        v <- self$mean()
        private$.value[what] <- v
      } else if (what == "q2.5") {
        v <- self$quantile(c(0.025))
        private$.value[what] <- v
      } else if (what == "q50") {
        v <- self$quantile(c(0.5))
        private$.value[what] <- v
      } else if (what == "q97.5") {
        v <- self$quantile(c(0.975))
        private$.value[what] <- v
      } else if (what == "current") {
        #v <- private$.val
        v <- private$.value[private$.whatnext]
        private$.value["current"] <- v
      } else if (what == "value") {
        if (is.null(val) | !is.numeric(val)) {
          rlang::abort("'v' must be numeric", class = "invalid_val")
        } else {
          v <- val
          private$.value[what] <- val
        }
      }
      else {
        rlang::abort(
          paste("'what' must be one of", "'random',", "'expected',", 
                "'q2.5',", "'q50',", "'97.5',", "'current',", "'value'"), 
          class ="what_not_supported"
        )
      }
#      private$.val <- v
      private$.whatnext <- what
      # silently return updated object
      return(invisible(self))
    },
    
    #' @description
    #' Gets the value of the \code{ModVar} that was set by the most recent call
    #' to \code{set()}.
    #' @return Value determined by last \code{set()}.
    get = function() {
      #v <- private$.val
      v <- unname(private$.value[private$.whatnext])
      return(v)
    }

    
  )
)
