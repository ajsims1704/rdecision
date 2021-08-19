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
    .value = NULL
  ),
  public = list(
    
    #' @description Create an object of type \code{ModVar}.
    #' @details A \code{ModVar} is associated with an uncertainty distribution
    #' (a "has-a" relationship in object-oriented terminology). There can be a
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
        "random", "expected", "q2.5", "q50", "q97.5", "value"
      )
      # set the .value vector members
      private$.value <- rep(as.numeric(NA), times=length(private$.whats))
      names(private$.value) <- private$.whats
      # value to return on first get
      private$.whatnext <- "expected"
      # return new object
      return(invisible(self))
    },

    #' @description Is this \code{ModVar} an expression?
    #' @return \code{TRUE} if it inherits from \code{ExprModVar}, \code{FALSE}
    #' otherwise.
    is_expression = function() {
      return(inherits(self, what="ExprModVar"))
    },

    #' @description Is the model variable probabilistic?
    #' @details Tests whether the model variable is probabilistic, i.e. a 
    #' random variable that follows a distribution, or an expression involving
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
    
    #' @description Name and parameters of the uncertainty distribution.
    #' @details If \eqn{K > 1} the dimension of the distribution associated 
    #' with this model variable is appended, e.g. \code{Dir(2,3)[1]} 
    #' means that the model variable is associated with the first dimension 
    #' of a 2D Dirichlet distribution with alpha parameters 2 and 3.
    #' @return Distribution name as character string.
    distribution = function() {
      dname <- private$.D$distribution()
      if (private$.D$order() > 1) {
        dname <- paste(dname,"[",private$.k,"]",sep='')
      }
      return(dname)
    },
    
    #' @description Mean value of the model variable. 
    #' @return Mean value as a numeric value.
    mean = function() {
      mvmean <- private$.D$mean()
      return(mvmean[private$.k])
    },
    
    #' @description The mode of the variable.
    #' @details By default returns \code{NA}, which will be the case for 
    #' most \code{ExprModVar} variables, because an arbitrary expression is 
    #' not guaranteed to be unimodal.
    #' @return Mode as a numeric value.
    mode = function() {
      mvmode <- private$.D$mode()
      return(mvmode[private$.k])
    },

    #' @description Standard deviation of the model variable. 
    #' @return Standard deviation as a numeric value
    SD = function() {
      mvsd <- private$.D$SD()
      return(mvsd[private$.k])
    },
    
    #' @description Quantiles of the uncertainty distribution. 
    #' @param probs Numeric vector of probabilities, each in range [0,1].
    #' @return Vector of numeric values of the same length as \code{probs}.
    quantile = function(probs) {
      # matrix for multivariate, vector for univariate distributions; argument
      # is checked in .D$quantiles()
      q <- private$.D$quantile(probs)
      if (private$.D$order() > 1) {
        rv <- q[,private$.k]
      } else {
        rv <- q
      }
      return(rv)
    },

    #' @description Draw a random sample from the model variable. 
    #' @details The same random sample will be returned until \code{set} is
    #' called to force a resample.
    #' @return A sample drawn at random.
    r = function() {
      # fetch the sample from the distribution
      rd <- private$.D$r()
      # get random sample for this dimension
      if (private$.D$order() > 1) {
        rv <- rd[private$.k]
      } else {
        rv <- rd
      }
      # return the sample
      return(rv)
    },

    #' @description Sets the value of the \code{ModVar}.
    #' @details Defines what will be returned' by subsequent
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
      if (!(what %in% c(private$.whats, "current"))) {
        rlang::abort(
          paste("'what' must be one of", "'random',", "'expected',", 
                "'q2.5',", "'q50',", "'97.5',", "'current',", "'value'"), 
          class ="what_not_supported"
        )
      }
      # if random, make a new draw from the distribution
      if (what == "random") {
        private$.D$sample()
      # if value, check and save the supplied number
      } else if (what == "value") {
        if (is.null(val) | !is.numeric(val)) {
          rlang::abort("'v' must be numeric", class = "invalid_val")
        } else {
          private$.value["value"] <- val
        }
      }
      # set whatnext for next call to get(), unless required to leave unchanged
      if (what != "current") {
        private$.whatnext <- what
      }
      # silently return updated object
      return(invisible(self))
    },
    
    #' @description Get the value of the \code{ModVar}.
    #' @details Returns the value defined by the most recent call
    #' to \code{set()}.
    #' @return Value determined by last \code{set()}.
    get = function() {
      # if random, save the sample
      if (private$.whatnext == "random") {
        private$.value["random"] <- self$r()
      } else if (private$.whatnext == "expected") {
        private$.value["expected"] <- self$mean()
      } else if (private$.whatnext == "q2.5") {
        private$.value["q2.5"] <- self$quantile(0.025)
      } else if (private$.whatnext == "q50") {
        private$.value["q50"] <- self$quantile(0.5)
      } else if (private$.whatnext == "q97.5") {
        private$.value["q97.5"] <- self$quantile(0.975)
      }
      # return the required value
      v <- unname(private$.value[private$.whatnext])
      return(v)
    }

    
  )
)
