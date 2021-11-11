#' @title An empirical distribution
#' 
#' @description An R6 class representing an empirical (1D) distribution.
#' 
#' @details An object representing an empirical distribution. It inherits
#' from class \code{Distribution}.
#'  
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
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
    initialize = function(x, interpolate.sample=TRUE) {
      # initialize the base class
      super$initialize("Empirical", K=as.integer(1))
      # check the sample
      if (!is.numeric(x)) {
        rlang::abort(
          "Argument 'x' must be numeric", class="x_not_numeric"
        )
      }
      if (any(is.na(x))) {
        rlang::abort(
          "Argument 'x' must have no missing values", 
          class = "x_not_supported"
        )
      }
      if (length(x) < 1) {
        rlang::abort(
          "Argument x must not have at least 1 element", 
          class="x_too_small"
        )
      }
      private$x <- x
      # check the sample method
      if (!is.logical(interpolate.sample)) {
        rlang::abort(
          "Argument 'interpolate.sample' must be logical", 
          class = "interpolate.sample_not_supported"
        )
      }
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
      rv <- as.numeric(NA)
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
    sample = function(expected=FALSE) {
      if (!expected) {
        if (private$interpolate.sample) {
          p <- stats::runif(n=1)
          private$.r[1] <- stats::quantile(x=private$x, probs=p, type=7)
        } else {
          private$.r[1] <- base::sample(x=private$x, size=1)
        }
      } else {
        private$.r[1] <- self$mean()
      }
      return(invisible(self))
    },
    
    #' @description
    #' Return the quantiles of the empirical uncertainty distribution.
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
      q <- stats::quantile(x=private$x, probs, names=FALSE)
      return(q)
    }
    
  )
)
