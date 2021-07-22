#' @title \verb{Distribution} class
#' 
#' @description An R6 class for a multivariate distribution.
#' 
#' @details This class is not intended to be instantiated by end-users of the
#' package. It is the base class for particular univariate or multivariate
#' distributions. 
#'
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Distribution <- R6::R6Class(
  classname = "Distribution",
  lock_class = TRUE,
  private = list(
    name = NULL,
    K = NULL,
    .r = NULL
  ),
  public = list(
    
    #' @description Create an object of class \code{Distribution}.
    #' @param name Name of the distribution ("Beta" etc.)
    #' @param K Order of the distribution (1=univariate, 2=bivariate etc.). 
    #' Must be an integer; use \code{as.integer()} to avoid an error.
    #' @return An object of class \code{Distribution}. 
    initialize = function(name, K=as.integer(1)) {
      # check name parameter
      if (base::missing(name)) {
        rlang::abort(
          "Argument 'name' must be a character", 
          class = "invalid_name"
        )
      }
      if (!is.character(name)) {
        rlang::abort(
          "Argument 'name' must be a character", 
          class = "invalid_name"
        )
      }
      private$name <- name
      # check dimension parameter
      if (!is.integer(K)) {
        rlang::abort(
          "Argument 'K must be an integer", 
          class = "invalid_order"
        )
      }
      if (K <= 0) {
        rlang::abort(
          "Argument 'K' must be > 0", 
          class="order_not_supported"
        )
      }
      private$K <- K
      # create space for a random draw and populate it
      private$.r <- vector(mode="numeric", length=K)
      # return Distribution
      return(invisible(self))
    },
    
    #' @description Order of the distribution
    #' @returns Order (\code{K}).
    order = function() {
      return(private$K)
    }, 

    #' @description Description of the uncertainty distribution.
    #' @details Includes the distribution name and its parameters.
    #' @return Distribution name and parameters as character string.
    distribution = function() {
      return(as.character(NA))
    },
    
    #' @description Draw and hold a random sample from the distribution.
    #' @returns Void
    sample = function() {
       return(invisible(self)) 
    },
    
    #' @description Return a random sample drawn from the distribution.
    #' @details Returns the sample generated at the last call to \code{sample}. 
    #' @returns A vector of length \code{K} representing one sample.
    r = function() {
      # return the sample
      return(private$.r)
    },
    
    #' @description Mean value of the distribution. 
    #' @return Mean value as a numeric scalar (\code{K=1}) or vector of 
    #' length \code{K}.
    mean = function() {
      rv <- rep(as.numeric(NA), times=private$K)
      return(rv)
    },
    
    #' @description Return the mode of the distribution. By default returns
    #' \code{NA}, which will be the case for most \code{ExprModVar} variables,
    #' because an arbitrary expression is not guaranteed to be unimodal.
    #' @return Mode as a numeric scalar (\code{K=1}) or vector of 
    #' length \code{K}.
    mode = function() {
      rv <- rep(as.numeric(NA), times=private$K)
      return(rv)
    },
    
    #' @description Return the standard deviation of a univariate distribution.
    #' @details Only defined for univariate (\code{K=1}) distributions; for 
    #' multivariate distributions, function \code{varcov} returns the 
    #' variance-covariance matrix. 
    #' @return Standard deviation as a numeric value.
    SD = function() {
      if (private$K != 1) {
        rlang::abort(
          "Function 'SD' not defined for multivariate distributions",
          class = "SD_undefined"
        )
      }
      return(as.numeric(NA))
    },
    
    #' @description Variance-covariance matrix.
    #' @returns A positive definite symmetric matrix of size \code{K} by 
    #' \code{K}, or a scalar for \code{K=1}, equal to the variance.
    varcov = function() {
      rv <- NULL
      if (private$K==1) {
        rv <- as.numeric(NA)
      } else {
        rv <- matrix(data=as.numeric(NA), nrow=private$K, ncol=private$K)
      }
      return(rv)
    },
    
    #' @description Quantiles of a univariate distribution. 
    #' @param probs Numeric vector of probabilities, each in range [0,1].
    #' @return Vector of numeric values of the same length as \code{probs}.
    #' The quantiles for \code{K>1} are not not single values (they are 
    #' lines for \code{K=2}, surfaces for \code{K=3}, etc.).
    quantile = function(probs) {
      # throw error for K>1
      if (private$K > 1) {
        rlang::abort(
          "Function 'quantile' not defined for multivariate distributions",
          class = "quantile_undefined"
        )
      }
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
      return(rep(as.numeric(NA), length(probs)))
    }

  )
)
