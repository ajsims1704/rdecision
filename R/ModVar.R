#' @title
#' ModVar
#' 
#' @description
#' An R6 class for a variable in an health economic model
#' 
#' @details 
#' Base class for a variable used in a health economic model. The base 
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
  private = list(
    .description = NULL,
    .units = NULL,
    .val = NA
  ),
  public = list(
    
    #' @description 
    #' Create an object of type `ModVar`
    #' @param description A character string description of the variable
    #' and its role in the model. This description will be used in a
    #' tabulation of the variables linked to a model.
    #' @param units A character string description of the units, e.g. 'GBP',
    #' 'per year'.
    #' @return A new ModVar object.
    initialize = function(description, units) {
      # test and set description
      if (!is.character(description)) {
        rlang::abort("Argument 'description' must be a string", 
                     class="description_not_string")
      }
      private$.description <- description
      if (!is.character(units)) {
        rlang::abort("Argument 'units' must be a string", 
                     class="units_not_string")
      }
      private$.units <- units
      return(invisible(self))
    },

    #' @description 
    #' Is this ModVar an expression?
    #' @return TRUE if it inherits from ExprModVar, FALSE otherwise.
    is_expression = function() {
      return(inherits(self, what="ExprModVar"))
    },

    #' @description 
    #' Tests whether the model variable is probabilistic, i.e. a random
    #' variable that follows a distribution, or an expression involving
    #' random variables, some of which follow distributions. 
    #' @return TRUE if probabilistic
    is_probabilistic = function() {
      return(as.logical(NA))
    },
    
    #' @description
    #' Accessor function for the description.
    #' @return Description of model variable as character string.
    description = function() {
      return(private$.description)
    },
    
    #' @description
    #' Accessor function for units.
    #' @return Description of units as character string.
    units = function() {
      return(private$.units)
    },
    
    #' @description 
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      return(as.character(NA))
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
    #' Return the mode of the variable. By default returns NA, which will be
    #' the case for most ExprModVar variables, because an arbitrary expression
    #' is not guaranteed to be unimodel.
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
    #' @return Vector of numeric values of the same length as `probs`.
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
    #' Sets the value of the ModVar that will be returned by subsequent
    #' calls to get() until set() is called again. 
    #' @param what Character: one of "random" (samples from the uncertainty
    #' distribution), "expected" (mean), "q2.5" (lower 95% confidence limit),
    #' "q50" (median), "q97.5" (upper 95% confidence limit), "current" (leaves
    #' the value unchanged). The "current" option is provided to support having
    #' common functions to set (or leave alone) sets of model variables, 
    #' depending on their use case.
    #' @return Updated ModVar.
    set = function(what="random") {
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
      } else if (what == "expected") {
        v <- self$mean()
      } else if (what == "q2.5") {
        v <- self$quantile(c(0.025))
      } else if (what == "q50") {
        v <- self$quantile(c(0.5))
      } else if (what == "q97.5") {
        v <- self$quantile(c(0.975))
      } else if (what == "current") {
        v <- private$.val
      }
      else {
        rlang::abort(
          "'what' must be one of 'random', 'expected', 'q2.5', 'q50', '97.5'", 
          class ="what_not_supported"
        )
      }
      private$.val <- v
      # silently return updated object
      return(invisible(self))
    },
    
    #' @description
    #' Gets the value of the ExprModVar that was set by the most recent call
    #' to set().
    #' @return Value determined by last set().
    get = function() {
      return(private$.val)
    }

    
  )
)
