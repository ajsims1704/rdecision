#' @title 
#' BetaModelVariable
#' 
#' @description
#' An R6 class for a model variable with Beta function uncertainty
#' 
#' @details 
#' A model variable for which the uncertainty in the point estimate can
#' be modelled with a Beta distribution. The hyperparameters of the
#' distribution are the shape (`alpha`) and the shape (`beta`) of
#' the uncertainty distribution. 
#'
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims5@nhs.net}
#' @export
#' 
BetaModelVariable <- R6::R6Class(
  classname = "BetaModelVariable",
  inherit = ModelVariable,
  private = list(
    alpha = 'numeric',
    beta = 'numeric'
  ),
  public = list(
    
    #' @description 
    #' Create an object of class BetaModelVariable.
    #' @param label A character string label for the variable. It is advised
    #' to make this the same as the variable name which helps when tabulating
    #' model variables involving ExpressionModelVariables.
    #' @param description A character string describing the variable.
    #' @param units Units of the variable, as character string.
    #' @param alpha parameter of the Beta distribution.
    #' @param beta parameter of the Beta distribution.
    #' @return An object of class BetaModelVariable. 
    initialize = function(description, units, alpha, beta) {
      super$initialize(description, units)
      private$alpha <- alpha
      private$beta <- beta
      private$val <- self$getMean()
    },
    
    #' @description
    #' Set the value of the model variable from its uncertainty distribution.
    #' Nothing is returned; the sampled value is returned at the next
    #' call to `value()`.
    #' @param expected Logical; if TRUE sets the value of the model variable
    #'        returned at subsequent calls to `value()` to be equal to the 
    #'        expectation of the variable. Default is FALSE.
    #' @return Updated BetaModelVariable object.
    sample = function(expected=F) {
      private$val <- NA
      if (expected) {
        private$val <- self$getMean()
      }
      else {
        private$val <- rbeta(1, shape1=private$alpha, shape2=private$beta)
      }
      return(invisible(self))
    },
    
    #' @description 
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string.
    getDistribution = function() {
      rv <- paste('Be(', private$alpha, ',', private$beta, ')', sep='')
      return(rv)
    },
    
    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    getMean = function() {
      return(private$alpha/(private$alpha+private$beta))
    },
    
    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    getSD = function() {
      a <- private$alpha
      b <- private$beta
      v <- (a*b) / ( (a+b)^2 * (a+b+1) )
      return(sqrt(v))
    },
    
    #' @description
    #' Return the quantiles of the Gamma uncertainty distribution.
    #' @param probs Vector of probabilities, in range [0,1].    
    #' @return Vector of quantiles.
    getQuantile = function(probs) {
      sapply(probs, FUN=function(x) {
        if (!is.numeric(probs)) {
          stop("BetaModelVariable$getQuantile: argument must be a numeric vector")
        }
      })
      q <- qbeta(probs, shape1=private$alpha, shape2=private$beta)
      return(q)
    }
    
  )
)
