#' @title 
#' GammaModVar
#' 
#' @description
#' An R6 class for a model variable with Gamma function uncertainty
#' 
#' @details 
#' A model variable for which the uncertainty in the point estimate can
#' be modelled with a Gamma distribution. The hyperparameters of the
#' distribution are the shape (`alpha`) and the scale (`beta`) of
#' the uncertainty distribution. Note that this variable naming 
#' convention follos Briggs et al; `beta` is more usually used to
#' describe the rate parameter (reciprocal of scale.)
#'
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims5@nhs.net}
#' @export
#' 
GammaModVar <- R6::R6Class(
  classname = "GammaModVar",
  inherit = ModVar,
  private = list(
    alpha = 'numeric',
    beta = 'numeric'
  ),
  public = list(
    
    #' @description 
    #' Create an object of class GammaModVar.
    #' @param description A character string describing the variable.
    #' @param units Units of the variable, as character string.
    #' @param alpha shape parameter of the Gamma distribution.
    #' @param beta scale parameter of the Gamma distribution.
    #' @return An object of class GammaModVar. 
    initialize = function(description, units, alpha, beta) {
      super$initialize(description, units)
      private$alpha <- alpha
      private$beta <- beta
      private$val <- private$alpha * private$beta
    },
    
    #' @description
    #' Set the value of the model variable from its uncertainty distribution.
    #' Nothing is returned; the sampled value is returned at the next
    #' call to `value()`.
    #' @param expected Logical; if TRUE sets the value of the model variable
    #'        returned at subsequent calls to `value()` to be equal to the 
    #'        expectation of the variable. Default is FALSE.
    #' @return Updated GammaModVar object.
    sample = function(expected=FALSE) {
      private$val <- NA
      if (expected) {
        private$val <- self$getMean()
      }
      else {
        private$val <- rgamma(1, shape=private$alpha, scale=private$beta)
      }
      return(invisible(self))
    },
    
    #' @description 
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string.
    getDistribution = function() {
      rv <- paste('Ga(', round(private$alpha,3), ',', round(private$beta,3), ')', sep='')
      return(rv)
    },
    
    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    getMean = function() {
      return(private$alpha*private$beta)
    },
    
    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    getSD = function() {
      return(sqrt(private$alpha)*private$beta)
    },

    #' @description
    #' Return the quantiles of the Gamma uncertainty distribution.
    #' @param probs Vector of probabilities, in range [0,1].    
    #' @return Vector of quantiles.
    getQuantile = function(probs) {
      sapply(probs, FUN=function(x) {
        if (!is.numeric(probs)) {
          stop("GammaModVar$getQuantile: argument must be a numeric vector",
               call. = FALSE)
        }
      })
      q <- qgamma(probs, shape=private$alpha, scale=private$beta)
      return(q)
    }

  )
)
