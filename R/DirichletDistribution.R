#' @title A parametrized Dirichlet distribution
#' 
#' @description An R6 class representing a multivariate Dirichlet distribution.
#' 
#' @details A multivariate Dirichlet distribution. See 
#' \url{https://en.wikipedia.org/wiki/Dirichlet_distribution} for details. 
#' Inherits from class \code{Distribution}.
#'
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
DirichletDistribution <- R6::R6Class(
  classname = "DirichletDistribution",
  lock_class = TRUE,
  inherit = Distribution,
  private = list(
    alpha = NULL
  ),
  public = list(
    
    #' @description 
    #' Create an object of class \code{DirichletDistribution}.
    #' @param alpha Parameters of the distribution; a vector of \code{K} numeric
    #' values each > 0, with \eqn{K > 1}.
    #' @return An object of class \code{DirichletDistribution}. 
    initialize = function(alpha) {
      # check alpha parameter
      if (length(alpha) < 2) {
        rlang::abort(
          "'alpha' must have at least 2 elements", 
          class="alpha_unsupported"
        )
      }
      sapply(alpha, FUN=function(x) {
        if (is.na(x)) {
          rlang::abort("All elements of 'alpha' must be defined",
                       class="alpha_not_defined")
        }
        if (!is.numeric(x)) {
          rlang::abort("Argument 'alpha' must be a numeric vector",
                       class="alpha_not_numeric")
        }
        if (x<=0) {
          rlang::abort("Elements of 'alpha' must be > 0",
                       class="alpha_unsupported")
        }
      })
      private$alpha <- alpha
      # create subclass object and check parameter
      super$initialize("Dir", K=length(alpha))
      # return Dirichlet distribution object
      return(invisible(self))
    },
    
    #' @description Accessor function for the name of the distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- paste('Dir(', paste(private$alpha, collapse=','), ')', sep='')
      return(rv)
    },
    
    #' @description Mean value of each dimension of the distribution.
    #' @return A numerical vector of length K.
    mean = function() {
      alpha0 <- sum(private$alpha)
      return(private$alpha/alpha0)
    },

    #' @description Return the mode of the distribution. 
    #' @details Undefined if any alpha is \eqn{\le 1}.
    #' @return Mode as a vector of length \code{K}.
    mode = function() {
      rv <- rep(as.numeric(NA), times=private$K)
      if (all(private$alpha > 1)) {
        alpha0 <- sum(private$alpha)
        rv <- (private$alpha - 1) / (alpha0 - private$K)
      }
      return(rv)
    },
    
    #' @description Quantiles of the univariate marginal distributions.
    #' @details The univariate marginal distributions of a Dirichlet
    #' distribution are Beta distributions. This function returns the
    #' quantiles of each marginal. Note that these are not the true
    #' quantiles of the multivariate Dirichlet.
    #' @param probs Numeric vector of probabilities, each in range [0,1].
    #' @return A matrix of numeric values with the number of rows equal to the 
    #' length of \code{probs}, the number of columns equal to the order; rows 
    #' are labelled with quantiles and columns with the dimension (1, 2, etc).
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
      # create output object
      rv <- matrix(
        rep(as.numeric(NA), times=length(probs)*private$K),
        nrow = length(probs), 
        ncol = private$K,
        dimnames = list(probs, seq(1:private$K))
      )
      # populate it
      alpha0 <- sum(private$alpha)
      for (k in 1:private$K) {
        q <- stats::qbeta(
          probs, 
          shape1 = private$alpha[k], 
          shape2 = alpha0-private$alpha[k]
        )
        rv[,k] <- q
      }
      return(rv)      
    },

    #' @description Variance-covariance matrix.
    #' @return A positive definite symmetric matrix of size \code{K} by 
    #' \code{K}.
    varcov = function() {
      VC <- matrix(data=as.numeric(NA), nrow=private$K, ncol=private$K)
      alpha0 <- sum(private$alpha)
      alphabar <- private$alpha / alpha0
      for (i in 1:private$K) {
        for (j in 1:private$K) {
          VC[i,j] <- ifelse(i==j,alphabar[i],0) - (alphabar[i]*alphabar[j])
          VC[i,j] <- VC[i,j] / (alpha0 + 1)
        }
      }
      return(VC)
    },
    
    #' @description Draw and hold a random sample from the distribution.
    #' @param expected If TRUE, sets the next value retrieved by a call to
    #' \code{r()} to be the mean of the distribution.
    #' @return Void; sample is retrieved with call to \code{r()}.
    sample = function(expected=FALSE) {
      if (!expected){
        # sample from gamma distributions
        y <- sapply(1:private$K, FUN=function(i) {
          stats::rgamma(n=1, shape=private$alpha[i], rate=1)
        })
        # normalize and hold
        private$.r <- y / sum(y)
      } else {
        private$.r <- self$mean()
      }
      return(invisible(self)) 
    }

  )
)
    