#' @title 
#' GammaModVar
#' 
#' @description
#' An R6 class for a model variable with Gamma function uncertainty
#' 
#' @details 
#' A model variable for which the uncertainty in the point estimate can
#' be modelled with a Gamma distribution. The hyperparameters of the
#' distribution are the shape (`alpha`) and the rate (`beta`) of
#' the uncertainty distribution. Note that this is the conventional
#' parametrization used in Bayesian statistics; in econometrics the
#' shape/scale (`k`/`theta`) parametrization is more common (and the one
#' used in this implementation). Note, however, that although Briggs 
#' \emph{et al} (2006) use the shape/scale formulation, they use `alpha`/`beta`
#' as parameter names.
#'  
#' @references 
#' \itemize{
#'   \item Briggs A, Claxton K, Sculpher M. Decision modelling for health
#'   economic evaluation. Oxford, UK: Oxford University Press; 2006. 
#' }
#' 
#' @note 
#' The Gamma model variable class can be used to model the uncertainty of
#' the mean of a count quantity which follows a Poisson distribution. The Gamma
#' distribution is the conjugate prior of a Poisson distribution, and the shape
#' and scale relate directly to the number of intervals from which the mean
#' count has been estimated. Specifically, the shape (\eqn{k}) is equal to the 
#' total count of events in \eqn{1/\theta} intervals, where \eqn{\theta} is the
#' scale. For example, if 200 counts were observed in a sample of 100 intervals, 
#' setting \code{shape=200} and \code{scale=1/100} gives a Gamma distribution 
#' with a mean of 2 and a 95\% confidence interval from 1.73 to 2.29. 
#'
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
GammaModVar <- R6::R6Class(
  classname = "GammaModVar",
  inherit = ModVar,
  private = list(
    shape = NULL,
    scale = NULL
  ),
  public = list(
    
    #' @description 
    #' Create an object of class GammaModVar.
    #' @param description A character string describing the variable.
    #' @param units Units of the variable, as character string.
    #' @param shape shape parameter of the Gamma distribution.
    #' @param scale scale parameter of the Gamma distribution.
    #' @return An object of class GammaModVar. 
    initialize = function(description, units, shape, scale) {
      super$initialize(description, units)
      if (!is.numeric(shape)) {
        rlang::abort(
          "Argument 'shape' must be numeric", 
          class="shape_not_numeric"
        )
      }
      if (shape <= 0) {
        rlang::abort(
          "Argument 'shape' must be > 0", 
          class="shape_not_supported"
        )
      }
      private$shape <- shape
      if (!is.numeric(scale)) {
        rlang::abort(
          "Argument 'scale' must be numeric", 
          class="scale_not_numeric"
        )
      }
      if (scale <= 0) {
        rlang::abort(
          "Argument 'scale' must be > 0", 
          class="scale_not_supported"
        )
      }
      private$scale <- scale
      # ensure first call to get() is valid
      self$set("expected")
      # return object
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
      rv <- paste(
        'Ga(', 
        round(private$shape,3), ',', 
        round(private$scale,3), ')', 
        sep=''
      )
      return(rv)
    },

    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    mean = function() {
      return(private$shape*private$scale)
    },

    #' @description 
    #' Return the mode of the distribution (if shape >= 1) 
    #' @return mode as a numeric value.
    mode = function() {
      rv <- as.numeric(NA)
      if (private$shape>=1) {
        rv <- (private$shape-1)*private$scale 
      }
      return(rv)
    },

    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    SD = function() {
      return(sqrt(private$shape)*private$scale)
    },

    #' @description 
    #' Draw a random sample from the model variable. 
    #' @param n Number of samples to draw.
    #' @return Samples drawn at random.
    r = function(n=1) {
      rv <- rgamma(n, shape=private$shape, scale=private$scale)
      return(rv)
    },

    #' @description
    #' Return the quantiles of the Gamma uncertainty distribution.
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
      q <- qgamma(probs, shape=private$shape, scale=private$scale)
      return(q)
    }

  )
)
