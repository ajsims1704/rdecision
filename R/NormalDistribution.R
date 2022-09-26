#' @title A parametrized Normal distribution
#' @description An R6 class representing a parametrized Normal distribution.
#' @details A Normal distribution with hyperparameters mean (\code{mu}) and 
#' standard deviation (\code{sd}). Inherits from class \code{Distribution}.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
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
      super$initialize("Normal", K = 1L)
      # check the parameters
      abortifnot(is.numeric(mu),
        message = "Argument 'mu' must be numeric", 
        class = "mu_not_numeric"
      )
      private$mu <- mu
      abortifnot(is.numeric(sigma),
        message = "Argument 'sigma' must be numeric", 
        class = "sigma_not_numeric"
      )
      private$sigma <- sigma
      # initial sample
      self$sample(TRUE)
      # return
      return(invisible(self))
    },
    
    #' @description Accessor function for the name of the distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- paste0(
        "N(", format(private$mu, digits = 4L, scientific = FALSE),
        ",",  format(private$sigma, digits = 4L, scientific = FALSE), ")"
      )
      return(rv)
    },
    
    #' @description Draw a random sample from the model variable. 
    #' @param expected If TRUE, sets the next value retrieved by a call to
    #' \code{r()} to be the mean of the distribution.
    #' @return A sample drawn at random.
    sample = function(expected = FALSE) {
      if (!expected) {
        private$.r[[1L]] <- rnorm(n = 1L, mean = private$mu, sd = private$sigma)
      } else {
        private$.r[[1L]] <- self$mean()
      }
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
      vapply(probs, FUN.VALUE = TRUE, FUN = function(x) {
        abortif(is.na(x),
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
      # quantiles of the normal distribution      
      q <- qnorm(probs, mean=private$mu, sd=private$sigma)
      return(q)
    }
  )
)
