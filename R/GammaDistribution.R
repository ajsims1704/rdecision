#' @title A parametrized Gamma distribution
#' @description An R6 class representing a Gamma distribution.
#' @details An object representing a Gamma distribution with hyperparameters
#' shape (\code{k}) and scale (\code{theta}). In econometrics this
#' parametrization is more common but in Bayesian statistics the shape
#' (\code{alpha}) and rate (\code{beta}) parametrization is more usual. Note,
#' however, that although Briggs \emph{et al} (2006) use the shape, scale
#' formulation, they use \code{alpha}, \code{beta} as parameter names. Inherits
#' from class \code{Distribution}.
#' @references{
#'   Briggs A, Claxton K, Sculpher M. Decision modelling for health
#'   economic evaluation. Oxford, UK: Oxford University Press; 2006.
#' }
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'
GammaDistribution <- R6::R6Class(
  classname = "GammaDistribution",
  lock_class = TRUE,
  inherit = Distribution,
  private = list(
    shape = NULL,
    scale = NULL
  ),
  public = list(

    #' @description Create an object of class \code{GammaDistribution}.
    #' @param shape shape parameter of the Gamma distribution.
    #' @param scale scale parameter of the Gamma distribution.
    #' @return An object of class \code{GammaDistribution}.
    initialize = function(shape, scale) {
      # initialize the base class
      super$initialize("Gamma", K = 1L)
      # check the parameters
      abortifnot(is.numeric(shape),
        message = "Argument 'shape' must be numeric",
        class = "shape_not_numeric"
      )
      abortifnot(shape > 0.0,
        message = "Argument 'shape' must be > 0",
        class = "shape_not_supported"
      )
      private$shape <- shape
      abortifnot(is.numeric(scale),
        message = "Argument 'scale' must be numeric",
        class = "scale_not_numeric"
      )
      abortifnot(scale > 0.0,
        message = "Argument 'scale' must be > 0",
        class = "scale_not_supported"
      )
      private$scale <- scale
      # initial sample
      self$sample(TRUE)
      # return object
      return(invisible(self))
    },

    #' @description Accessor function for the name of the distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- paste0(
        "Ga(",
        round(private$shape, digits = 3L), ",",
        round(private$scale, digits = 3L), ")"
      )
      return(rv)
    },

    #' @description
    #' Return the expected value of the distribution.
    #' @return Expected value as a numeric value.
    mean = function() {
      return(private$shape * private$scale)
    },

    #' @description Return the mode of the distribution (if \code{shape} >= 1)
    #' @return mode as a numeric value.
    mode = function() {
      rv <- NA_real_
      if (private$shape >= 1.0) {
        rv <- (private$shape - 1.0) * private$scale
      }
      return(rv)
    },

    #' @description Return the standard deviation of the distribution.
    #' @return Standard deviation as a numeric value
    SD = function() {
      return(sqrt(private$shape) * private$scale)
    },

    #' @description Draw and hold a random sample from the distribution.
    #' @param expected If TRUE, sets the next value retrieved by a call to
    #' \code{r()} to be the mean of the distribution.
    #' @return Updated distribution.
    sample = function(expected = FALSE) {
      if (expected) {
        private$.r[[1L]] <- self$mean()
      } else {
        private$.r[[1L]] <- rgamma(
          n = 1L,
          shape = private$shape,
          scale = private$scale
        )
      }
      return(invisible(self))
    },

    #' @description Return the quantiles of the Gamma uncertainty distribution.
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
      q <- qgamma(probs, shape = private$shape, scale = private$scale)
      return(q)
    }
  )
)
