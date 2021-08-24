#' @title A parametrized Normal distribution
#' 
#' @description An R6 class representing a parametrized Normal distribution.
#' 
#' @details A Normal distribution with hyperparameters mean (\code{mu}) and 
#' standard deviation (\code{sd}). Inherits from class \code{Distribution}.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
NormalDistribution <- R6::R6Class(
  classname = "NormalDistribution",
  lock_class = TRUE,
  inherit = Distribution,
  private = list(
    mu = NULL,
    sigma = NULL
  ),
  public = list(
    
    #' @description Create a parametrized normal distribution. 
    #' @param mu Mean of the Normal distribution.
    #' @param sigma Standard deviation of the Normal distribution.
    #' @return A \code{NormalDistribution} object.
    initialize = function(mu, sigma) {
      # initialize the base class
      super$initialize("Normal", K=as.integer(1))
      # check the parameters
      if (!is.numeric(mu)) {
        rlang::abort(
          "Argument 'mu' must be numeric", 
          class="mu_not_numeric"
        )
      }
      private$mu <- mu
      if (!is.numeric(sigma)) {
        rlang::abort(
          "Argument 'sigma' must be numeric", 
          class="sigma_not_numeric"
        )
      }
      private$sigma <- sigma
      # initial sample
      self$sample()
      # return
      return(invisible(self))
    },
    
    #' @description Accessor function for the name of the distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- paste('N(', 
                  format(private$mu, digits=4, scientific=F),
                  ',', 
                  format(private$sigma, digits=4, scientific=F), 
                  ')', sep='')
      return(rv)
    },
    
    #' @description Draw a random sample from the model variable. 
    #' @return A sample drawn at random.
    sample = function() {
      private$.r <- rnorm(n=1, mean=private$mu, sd=private$sigma)
      # return the updated object
      return(invisible(self))
    },
    
    #' @description Return the mean value of the distribution.
    #' @return Expected value as a numeric value.
    mean = function() {
      return(private$mu)
    },
    
    #' @description Return the standard deviation of the distribution.
    #' @return Standard deviation as a numeric value
    SD = function() {
      return(private$sigma)
    },
    
    #' @description Return the quantiles of the Normal uncertainty distribution.
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
