#' @title 
#' NormalModelVariable
#' 
#' @description 
#' An R6 class for a model variable with Normal uncertainty
#' 
#' @details 
#' A model variable for which the uncertainty in the point estimate can
#' be modelled with a Normal distribution. The hyperparameters of the
#' distribution are the mean (`mu`) and the standard deviation (`sd`) of
#' the uncertainty distribution. The value of `mu` is the expected value
#' of the variable.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims5@nhs.net}
#' @export
#' 
NormalModelVariable <- R6::R6Class(
  classname = "NormalModelVariable",
  inherit = ModelVariable,
  private = list(
    mu = 'numeric',
    sigma = 'numeric'
  ),
  public = list(

    #' @description
    #' Create a model variable with normal uncertainty. 
    #' @param description A character string describing the variable.
    #' @param units Units of the quantity; character string.
    #' @param mu Hyperparameter with mean of the Normal distribution for 
    #'        the uncertainty of the variable.
    #' @param sigma Hyperparameter equal to the standard deviation of the
    #'        normal distribution for the uncertainty of the variable.
    #' @return A NormalModelVariable object.
    initialize = function(description, units, mu, sigma) {
      super$initialize(description, units)
      private$mu <- mu
      private$sigma <- sigma
      private$val <- mu
    },
    
    #' @description
    #' Set the value of the model variable from its uncertainty distribution.
    #' Nothing is returned; the sampled value is returned at the next
    #' call to `value()`.
    #' @param expected Logical; if TRUE sets the value of the model variable
    #'        returned at subsequent calls to `value()` to be equal to the 
    #'        expectation of the variable. Default is FALSE.
    #' @return Updated NormalModelVariable object.
    sample = function(expected=F) {
      if (expected) {
        private$val <- self$getMean()
      }
      else {
        private$val <- rnorm(1, mean=private$mu, sd=private$sigma)
      }
      invisible(self)
    },
    
    #' @description 
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string.
    getDistribution = function() {
      rv <- paste('N(', 
                  format(private$mu, digits=4, scientific=F),
                  ',', 
                  format(private$sigma, digits=4, scientific=F), 
                  ')', sep='')
      return(rv)
    },
    
    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    getMean = function() {
      return(private$mu)
    },

    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    getSD = function() {
      return(private$sigma)
    },

    #' @description
    #' Return the quantiles of the Normal uncertainty distribution.
    #' @param probs Vector of probabilities, in range [0,1].    
    #' @return Vector of quantiles.
    getQuantile = function(probs) {
      sapply(probs, FUN=function(x) {
        if (!is.numeric(probs)) {
          stop("NormalModelVariable$getQuantile: argument must be a numeric vector")
        }
      })
      q <- qnorm(probs, mean=private$mu, sd=private$sigma)
      return(q)
    }
    
  )
)
