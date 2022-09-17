#' @title A Dirac delta function
#' @description An R6 class representing a Dirac Delta function.
#' @details A distribution modelled by a Dirac delta function \eqn{\delta(x-c)}
#' where \eqn{c} is the hyperparameter (value of the constant). It has 
#' probability 1 that the value will be equal to \eqn{c} and zero otherwise. 
#' The mode, mean, quantiles and random samples are all equal to \eqn{c}. It is
#' acknowledged that there is debate over whether Dirac delta functions are 
#' true distributions, but the assumption makes little practical difference in 
#' this case. Inherits from class \code{Distribution}.
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
DiracDistribution <- R6::R6Class(
  classname = "DiracDistributon",
  lock_class = TRUE,
  inherit = Distribution,
  private = list(
    c = NULL
  ),
  public = list(
    
    #' @description Create a new Dirac Delta function distribution.
    #' @param const The value at which the distribution is centred.
    #' @return A new \code{DiracDistribution} object.
    initialize = function(const) {
      # initialize the base class
      super$initialize("Dirac", K = 1L)
      # check the argument
      abortifnot(is.numeric(const),
        message = "Argument 'const' must be numeric", 
        class = "const_not_numeric"
      )
      private$c <- const
      # initial sample
      self$sample(expected = TRUE)
      # return object
      return(invisible(self))
    },
    
    #' @description Accessor function for the name of the distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- paste0(
        "Const(", format(private$c, digits = 4L, scientific = FALSE), ")"
      )
      return(rv)
    },
    
    #' @description Return the mode of the distribution.
    #' @return Numeric Value where the distribution is centred.
    mode = function() {
      return(private$c)
    },
    
    #' @description Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    mean = function() {
      return(private$c)
    },
    
    #' @description  Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    SD = function() {
      return(0.0)
    },
    
    #' @description Quantiles of the distribution.
    #' @details For a Dirac Delta Function all quantiles are returned as the
    #' value at which the distribution is centred.
    #' @param probs Numeric vector of probabilities, each in range [0,1].
    #' @return Vector of numeric values of the same length as \code{probs}.
    quantile = function(probs) {
      # test argument
      vapply(probs, FUN.VALUE = TRUE, FUN=function(x) {
        abortifnot(!is.na(x),
          message = "All elements of 'probs' must be defined",
          class = "probs_not_defined"
        )
        abortifnot(is.numeric(x),
          message = "Argument 'probs' must be a numeric vector",
          class = "probs_not_numeric"
        )
        abortifnot(x >= 0.0 && x <= 1.0,
          message = "Elements of 'probs' must be in range[0,1]",
          class = "probs_out_of_range"
        )
        return(TRUE)
      })
      q <- rep(private$c, times = length(probs))
      return(q)
    },
    
    #' @description Draw and hold a random sample from the model variable. 
    #' @param expected If TRUE, sets the next value retrieved by a call to
    #' \code{r()} to be the mean of the distribution.
    #' @return Updated distribution.
    sample = function(expected = FALSE) {
      if (!expected) {
        private$.r[[1L]] <- private$c
      } else {
        private$.r[[1L]] <- self$mean()
      }
      return(invisible(self))
    }

  )
)
