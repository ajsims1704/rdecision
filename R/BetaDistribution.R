#' @title A parametrized Beta Distribution
#' @description An R6 class representing a Beta distribution with parameters.
#' @details A Beta distribution with hyperparameters for shape (\code{alpha}
#' and \code{beta}). Inherits from class \code{Distribution}.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
BetaDistribution <- R6::R6Class(
  classname = "BetaDistribution",
  lock_class = TRUE,
  inherit = Distribution,
  private = list(
    alpha = NULL,
    beta = NULL
  ),
  public = list(

    #' @description Create an object of class \code{BetaDistribution}.
    #' @param alpha parameter of the Beta distribution.
    #' @param beta parameter of the Beta distribution.
    #' @return An object of class \code{BetaDistribution}.
    initialize = function(alpha, beta) {
      # initialize the base class
      super$initialize("Beta", K = 1L)
      # check alpha parameter
      abortifnot(is.numeric(alpha),
        message = "Argument 'alpha' must be numeric",
        class = "alpha_not_numeric"
      )
      abortifnot(alpha > 0.0,
        message = "Argument 'alpha' must be > 0",
        class = "alpha_not_supported"
      )
      private$alpha <- alpha
      # check beta parameter
      abortifnot(is.numeric(beta),
        message = "Argument 'beta must be numeric",
        class = "beta_not_numeric"
      )
      abortifnot(beta > 0.0,
        message = "Argument 'beta' must be > 0",
        class = "beta_not_supported"
      )
      private$beta <- beta
      # initial sample
      self$sample(expected = TRUE)
      # return BetaDistribution
      return(invisible(self))
    },

    #' @description Accessor function for the name of the uncertainty
    #' distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- paste0("Be(", private$alpha, ",", private$beta, ")")
      return(rv)
    },

    #' @description The expected value of the distribution.
    #' @return Expected value as a numeric value.
    mean = function() {
      return(private$alpha / (private$alpha + private$beta))
    },

    #' @description The mode of the distribution (if
    #' \code{alpha}, \code{beta} > 1)
    #' @return mode as a numeric value.
    mode = function() {
      rv <- NA_real_
      if (private$alpha == 1.0 && private$beta == 1.0) {
        rv <- 0.5
      } else if (private$alpha < 1.0 && private$beta < 1.0) {
        rv <- NA_real_ # bimodal
      } else if (private$alpha <= 1.0 && private$beta > 1.0) {
        rv <- 0.0
      } else if (private$alpha > 1.0 && private$beta <= 1.0) {
        rv <- 1.0
      } else {
        rv <- (private$alpha - 1.0) / (private$alpha + private$beta - 2.0)
      }
      return(rv)
    },

    #' @description The standard deviation of the distribution.
    #' @return Standard deviation as a numeric value
    SD = function() {
      a <- private$alpha
      b <- private$beta
      v <- (a * b) / ((a + b) ^ 2L * (a + b + 1L))
      return(sqrt(v))
    },

    #' @description Draw and hold a random sample from the model variable.
    #' @param expected If TRUE, sets the next value retrieved by a call to
    #' \code{r()} to be the mean of the distribution.
    #' @return Updated distribution.
    sample = function(expected = FALSE) {
      if (!expected) {
        private$.r[[1L]] <- rbeta(
          n = 1L, shape1 = private$alpha, shape2 = private$beta
        )
      } else {
        private$.r[[1L]] <- self$mean()
      }
      return(invisible(self))
    },

    #' @description The quantiles of the Beta distribution.
    #' @param probs Vector of probabilities, in range [0,1].
    #' @return Vector of quantiles.
    quantile = function(probs) {
      # test argument
      vapply(probs, FUN.VALUE = TRUE, FUN = function(x) {
        abortifnot(!is.na(x),
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
      q <- qbeta(probs, shape1 = private$alpha, shape2 = private$beta)
      names(q) <- probs
      return(q)
    }
  )
)
