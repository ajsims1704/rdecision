#' @title \verb{DiracDistribution} class
#' 
#' @description An R6 class for a Dirac Delta function distribution.
#' 
#' @details A distribution modelled by a Dirac delta function \eqn{\delta(x-c)}
#' where \eqn{c} is the hyperparameter (value of the constant). It has 
#' probability 1 that the value will be equal to \eqn{c} and zero otherwise. 
#' The mode, mean, quantiles and random samples are all equal to \eqn{c}. It is
#' acknowledged that there is debate over whether Dirac delta functions are 
#' true distributions, but the assumption makes little practical difference in 
#' this case. 
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
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
      super$initialize("Dirac", K=as.integer(1))
      # check the argument
      if (!is.numeric(const)) {
        rlang::abort(
          "Argument 'const' must be numeric", 
          class="const_not_numeric"
        )
      }
      private$c <- const
      # initial sample
      self$sample()
      # return object
      return(invisible(self))
    },
    
    #' @description Accessor function for the name of the distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- paste('Const(', 
                  format(private$c, digits=4, scientific=F),
                  ')', sep='')
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
      return(0)
    },
    
    #' @description Quantiles of the distribution.
    #' @details For a Dirac Delta Function all quantiles are returned as the
    #' value at which the distribution is centred.
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
      q <- rep(private$c, times=length(probs))
      return(q)
    },
    
    #' @description Draw and hold a random sample from the model variable. 
    #' @return Updated distribution.
    sample = function() {
      private$.r[1] <- private$c
      return(invisible(self))
    }

  )
)
