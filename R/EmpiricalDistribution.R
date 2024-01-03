#' @title An empirical distribution
#' @description An R6 class representing an empirical (1D) distribution.
#' @details An object representing an empirical distribution. It inherits
#' from class \code{Distribution}.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
EmpiricalDistribution <- R6::R6Class(
  classname = "EmpiricalDistribution",
  lock_class = TRUE,
  inherit = Distribution,
  private = list(
    x = NULL,
    interpolate.sample = NULL
  ),
  public = list(

    #' @description Create an object of class \code{EmpiricalDistribution}.
    #' @details Empirical distributions based on very small sample sizes are
    #' supported, but not recommended.
    #' @param x a sample of at least 1 numerical value from the population
    #' of interest.
    #' @param interpolate.sample Logical; if true, each call to \code{sample()}
    #' make a random draw from \eqn{U_{0,1}} to find a \eqn{p} value, then
    #' finds that quantile of the sample, using the \code{quantile} function
    #' in R, via interpolation from the eCDF. If false, the \code{sample()}
    #' function makes a random draw from \code{x}.
    #' @return An object of class \code{EmpiricalDistribution}.
    initialize = function(x, interpolate.sample = TRUE) {
      # initialize the base class
      super$initialize("Empirical", K = 1L)
      # check the sample
      abortifnot(is.numeric(x),
        message = "Argument 'x' must be numeric",
        class = "x_not_numeric"
      )
      abortif(anyNA(x),
        message = "Argument 'x' must have no missing values",
        class = "x_not_supported"
      )
      abortifnot(length(x) >= 1L,
        message = "Argument x must not have at least 1 element",
        class = "x_too_small"
      )
      private$x <- x
      # check the sample method
      abortifnot(is.logical(interpolate.sample),
        message = "Argument 'interpolate.sample' must be logical",
        class = "interpolate.sample_not_supported"
      )
      private$interpolate.sample <- interpolate.sample
      # initial sample
      self$sample(TRUE)
      # return object
      return(invisible(self))
    },

    #' @description Accessor function for the name of the distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- "Emp"
      return(rv)
    },

    #' @description Return the expected value of the distribution.
    #' @return Expected value as a numeric value.
    mean = function() {
      rv <- base::mean(private$x)
      return(rv)
    },

    #' @description Return the mode of the distribution,
    #' @return NA because an empirical distribution is not guaranteed to be
    #' unimodal.
    mode = function() {
      rv <- NA_real_
      return(rv)
    },

    #' @description Return the standard deviation of the distribution.
    #' @return Standard deviation as a numeric value
    SD = function() {
      rv <- stats::sd(private$x)
      return(rv)
    },

    #' @description Draw and hold a random sample from the distribution.
    #' @details Samples with interpolation or by random draw from the
    #' supplied distribution (see parameter \code{interpolate.sample} in
    #' \code{new()}).
    #' @param expected If TRUE, sets the next value retrieved by a call to
    #' \code{r()} to be the mean of the distribution.
    #' @return Updated distribution.
    sample = function(expected = FALSE) {
      if (!expected) {
        if (private$interpolate.sample) {
          private$.r[[1L]] <- stats::quantile(
            x = private$x,
            probs = stats::runif(n = 1L),
            type = 7L
          )
        } else {
          private$.r[[1L]] <- base::sample(x = private$x, size = 1L)
        }
      } else {
        private$.r[[1L]] <- self$mean()
      }
      return(invisible(self))
    },

    #' @description
    #' Return the quantiles of the empirical uncertainty distribution.
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
      q <- stats::quantile(x = private$x, probs, names = FALSE)
      return(q)
    }
  )
)
