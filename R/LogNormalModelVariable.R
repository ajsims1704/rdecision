#' @title 
#' LogNormalModelVariable
#' 
#' @description 
#' An R6 class for a model variable with logNormal uncertainty
#' 
#' @details 
#' A model variable for which the uncertainty in the point estimate can
#' be modelled with a logNormal distribution. The hyperparameters of the
#' distribution are the mean (`mu`) and the standard deviation (`sd`) of
#' the uncertainty distribution. 
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims5@nhs.net}
#' @export
#' 
LogNormalModelVariable <- R6::R6Class(
  classname = "LogNormalModelVariable",
  inherit = ModelVariable,
  private = list(
    mu = 'numeric',
    sigma = 'numeric'
  ),
  public = list(
    
    #' @description
    #' Create a model variable with log normal uncertainty. 
    #' @param description A character string describing the variable.
    #' @param units Units of the quantity; character string.
    #' @param mu Hyperparameter with mean of the log normal distribution for 
    #'        the uncertainty of the variable.
    #' @param sigma Hyperparameter equal to the standard deviation of the
    #'        log normal distribution for the uncertainty of the variable.
    #' @return A LogNormalModelVariable object.
    initialize = function(description, units, mu, sigma) {
      super$initialize(description, units)
      private$mu <- mu
      private$sigma <- sigma
      private$val <- self$getMean()
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
      private$val = ifelse(
        expected,
        private$mu,
        rlnorm(1, meanlog=private$mu, sdlog=private$sigma)
      )
      invisible(self)
    },
    
    #' @description 
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string.
    getDistribution = function() {
      rv <- paste('logN(', private$mu, ',', private$sigma, ')', sep='')
      return(rv)
    },
    
    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    getMean = function() {
      mu <- private$mu
      sigma <- private$sigma
      E <- exp(mu + (sigma^2)/2)
      return(E)
    },
    
    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    getSD = function() {
      mu <- private$mu
      sigma <- private$sigma
      V <- (exp(sigma^2) - 1) * exp(2*mu + sigma^2)
      return(sqrt(V))
    }
  )
)
