#' @title 
#' NormModVar
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
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
NormModVar <- R6::R6Class(
  classname = "NormModVar",
  inherit = ModVar,
  private = list(
    mu = NULL,
    sigma = NULL
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
    #' @return A NormModVar object.
    initialize = function(description, units, mu, sigma) {
      super$initialize(description, units)
      if (!is.numeric(mu)) {
        rlang::abort("Argument 'mu' must be numeric", class="mu_not_numeric")
      }
      private$mu <- mu
      if (!is.numeric(sigma)) {
        rlang::abort("Argument 'sigma' must be numeric", class="sigma_not_numeric")
      }
      private$sigma <- sigma
      # initialize next get() call
      self$set(TRUE)
      # return
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
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- paste('N(', 
                  format(private$mu, digits=4, scientific=F),
                  ',', 
                  format(private$sigma, digits=4, scientific=F), 
                  ')', sep='')
      return(rv)
    },

    #' @description 
    #' Draw a random sample from the model variable. Normally accessed by a 
    #' call to value(what="r").
    #' @param n Number of samples to draw.
    #' @return A sample drawn at random.
    r = function(n=1) {
      rv <- rnorm(n=n, mean=private$mu, sd=private$sigma)
      # return the samples
      return(rv)
    },
        
    #' @description
    #' Return the mean value of the distribution.
    #' @return Expected value as a numeric value.
    mean = function() {
      return(private$mu)
    },

    #' @description
    #' Return the standard deviation of the distribution.
    #' @return Standard deviation as a numeric value
    SD = function() {
      return(private$sigma)
    },

    #' @description
    #' Return the quantiles of the Normal uncertainty distribution.
    #' @param probs Vector of probabilities, in range [0,1].
    #' @return Vector of quantiles.
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
      # quantiles of the normal distribution      
      q <- qnorm(probs, mean=private$mu, sd=private$sigma)
      return(q)
    }
    
  )
)
