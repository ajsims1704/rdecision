#' @title A probability distribution
#' @description An R6 class representing a (possibly multivariate) distribution.
#' @details The base class for particular univariate or multivariate 
#' distributions. 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
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
    #' @param K Order of the distribution (1 = univariate, 2 = bivariate etc.). 
    #' Must be an integer; use 1L, 3L etc. to avoid an error.
    #' @return An object of class \code{Distribution}. 
    initialize = function(name, K = 1L) {
      # check name parameter
      abortifnot(!base::missing(name),
        message = "Argument 'name' must be a character", 
        class = "invalid_name"
      )
      abortifnot(is.character(name),
        message = "Argument 'name' must be a character", 
        class = "invalid_name"
      )
      private$name <- name
      # check dimension parameter
      abortifnot(is.integer(K),
        message = "Argument 'K' must be an integer", 
        class = "invalid_order"
      )
      abortifnot(K > 0L,
        message = "Argument 'K' must be > 0", 
        class = "order_not_supported"
      )
      private$K <- K
      # create space for a random draw and populate it with NA
      private$.r <- rep(NA_real_, times = K)
      # return Distribution
      return(invisible(self))
    },
    
    #' @description Order of the distribution
    #' @return Order (\code{K}).
    order = function() {
      return(private$K)
    }, 

    #' @description Description of the uncertainty distribution.
    #' @details Includes the distribution name and its parameters.
    #' @return Distribution name and parameters as character string.
    distribution = function() {
      return(NA_character_)
    },
    
    #' @description Mean value of the distribution. 
    #' @return Mean value as a numeric scalar (\code{K = 1L}) or vector of 
    #' length \code{K}.
    mean = function() {
      rv <- rep(NA_real_, times = private$K)
      return(rv)
    },
    
    #' @description Return the mode of the distribution. 
    #' @details By default returns \code{NA}, which will be the case for most
    #' because an arbitrary distribution is not guaranteed to be unimodal.
    #' @return Mode as a numeric scalar (\code{K = 1L}) or vector of 
    #' length \code{K}.
    mode = function() {
      rv <- rep(NA_real_, times = private$K)
      return(rv)
    },
    
    #' @description Return the standard deviation of a univariate distribution.
    #' @details Only defined for univariate (\code{K = 1L}) distributions; for 
    #' multivariate distributions, function \code{varcov} returns the 
    #' variance-covariance matrix. 
    #' @return Standard deviation as a numeric value.
    SD = function() {
      rv <- rep(NA_real_, times = private$K)
      return(rv)
    },
    
    #' @description Variance-covariance matrix.
    #' @return A positive definite symmetric matrix of size \code{K} by 
    #' \code{K}, or a scalar for \code{K = 1L}, equal to the variance.
    varcov = function() {
      rv <- NA_real_
      if (private$K > 1L) {
        rv <- matrix(data = NA_real_, nrow = private$K, ncol = private$K)
      }
      return(rv)
    },
    
    #' @description Marginal quantiles of the distribution. 
    #' @details If they are defined, this function returns the marginal 
    #' quantiles of the multivariate distribution; i.e. the quantiles of each
    #' univariate marginal distribution of the multivariate distribution. For
    #' example, the univariate marginal distributions of a multivariate
    #' normal are univariate normals, and the univariate marginal distributions
    #' of a Dirichlet distribution are Beta distributions. Note that these are 
    #' not the true quantiles of a multivariate distribution, which are contours
    #' for \code{K = 2L}, surfaces for \code{K = 3L}, etc. For example, the
    #' 2.5\% and 97.5\% marginal quantiles of a bivariate normal distribution
    #' define a rectangle in \eqn{x_1, x_2} space that will include more than
    #' 95\% of the distribution, whereas the contour containing 95\% of the
    #' distribution is an ellipse.
    #' @param probs Numeric vector of probabilities, each in range [0,1].
    #' @return For \code{K = 1L} a numeric vector of length equal to the length
    #' of \code{probs}, with each entry labelled with the quantile. For 
    #' \code{K > 1L} a matrix of numeric values with the number of rows equal
    #' to the length of \code{probs}, the number of columns equal to the order;
    #' rows are labelled with probabilities and columns with the dimension
    #' (1, 2, etc).
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
      # create output object
      if (private$K == 1L) {
        rv <- rep(NA_real_, times = length(probs))
        names(rv) <- probs
      } else {
        rv <- matrix(
          rep(NA_real_, times = length(probs)*private$K),
          nrow = length(probs), 
          ncol = private$K,
          dimnames = list(probs, seq_len(private$K))
        )
      }
      return(rv)
    },
    
    #' @description Draw and hold a random sample from the distribution.
    #' @param expected If TRUE, sets the next value retrieved by a call to
    #' \code{r()} to be the mean of the distribution.
    #' @return Void
    sample = function(expected = FALSE) {
      abortifnot(is.logical(expected),
        message = "'expected' must be logical", 
        class = "invalid_expected"
      )
      return(invisible(self)) 
    },
    
    #' @description Return a random sample drawn from the distribution.
    #' @details Returns the sample generated at the last call to \code{sample}. 
    #' @return A vector of length \code{K} representing one sample.
    r = function() {
      # return the sample
      return(private$.r)
    }
  )
)
