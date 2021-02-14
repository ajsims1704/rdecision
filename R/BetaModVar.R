#' @title 
#' BetaModVar
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
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
BetaModVar <- R6::R6Class(
  classname = "BetaModVar",
  inherit = ModVar,
  private = list(
    alpha = NULL,
    beta = NULL
  ),
  public = list(
    
    #' @description 
    #' Create an object of class BetaModVar.
    #' @param description A character string describing the variable.
    #' @param units Units of the variable, as character string.
    #' @param alpha parameter of the Beta distribution.
    #' @param beta parameter of the Beta distribution.
    #' @return An object of class BetaModVar. 
    initialize = function(description, units, alpha, beta) {
      super$initialize(description, units)
      # check alpha parameter
      if (!is.numeric(alpha)) {
        rlang::abort(
          "Argument 'alpha' must be numeric", 
          class="alpha_not_numeric"
        )
      }
      if (alpha <= 0) {
        rlang::abort(
          "Argument 'alpha' must be > 0", 
          class="alpha_not_supported"
        )
      }
      private$alpha <- alpha
      # check beta parameter
      if (!is.numeric(beta)) {
        rlang::abort(
          "Argument 'beta must be numeric", 
          class="beta_not_numeric"
        )
      }
      if (beta <= 0) {
        rlang::abort(
          "Argument 'beta' must be > 0", 
          class="beta_not_supported"
        )
      }
      private$beta <- beta
      # return BetaModVar
      return(invisible(self))
    },

    #' @description 
    #' Tests whether the model variable is probabilistic, i.e. a random
    #' variable that follows a distribution, or an expression involving
    #' random variables, some of which follow distributions. 
    #' @return TRUE if probabilistic
    is_probabilistic = function() {
      return(TRUE)
    },
    
    #' @description
    #' Set the value of the model variable from its uncertainty distribution.
    #' Nothing is returned; the sampled value is returned at the next
    #' call to `value()`.
    #' @param expected Logical; if TRUE sets the value of the model variable
    #'        returned at subsequent calls to `value()` to be equal to the 
    #'        expectation of the variable. Default is FALSE.
    #' @return Updated BetaModVar object.
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
    distribution = function() {
      rv <- paste('Be(', private$alpha, ',', private$beta, ')', sep='')
      return(rv)
    },
    
    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    mean = function() {
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
          stop("BetaModVar$getQuantile: argument must be a numeric vector",
               call.=FALSE)
        }
      })
      q <- qbeta(probs, shape1=private$alpha, shape2=private$beta)
      return(q)
    }
    
  )
)
