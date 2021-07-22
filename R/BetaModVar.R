#' @title \verb{BetaModVar} class
#' 
#' @description
#' An R6 class for a model variable with Beta function uncertainty
#' 
#' @details 
#' A model variable for which the uncertainty in the point estimate can
#' be modelled with a Beta distribution. The hyperparameters of the
#' distribution are the shape parameters (\code{alpha} and \code{beta}) of
#' the uncertainty distribution. 
#'
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
BetaModVar <- R6::R6Class(
  classname = "BetaModVar",
  lock_class = TRUE,
  inherit = ModVar,
  private = list(
    alpha = NULL,
    beta = NULL
  ),
  public = list(
    
    #' @description 
    #' Create an object of class \code{BetaModVar}.
    #' @param description A character string describing the variable.
    #' @param units Units of the variable, as character string.
    #' @param alpha parameter of the Beta distribution.
    #' @param beta parameter of the Beta distribution.
    #' @return An object of class \code{BetaModVar}. 
    initialize = function(description, units, alpha, beta) {
      # check alpha parameter
      if (!is.numeric(alpha)) {
        rlang::abort(
          "Argument 'alpha' must be numeric", 
          class="alpha_not_numeric"
        )
      }
      if (alpha <= 0) {
        rlang::abort(
          "Argument 'alpha' must be > 0", 
          class="alpha_not_supported"
        )
      }
      private$alpha <- alpha
      # check beta parameter
      if (!is.numeric(beta)) {
        rlang::abort(
          "Argument 'beta must be numeric", 
          class="beta_not_numeric"
        )
      }
      if (beta <= 0) {
        rlang::abort(
          "Argument 'beta' must be > 0", 
          class="beta_not_supported"
        )
      }
      private$beta <- beta
      # create Beta distribution
      D <- BetaDistribution$new(alpha=alpha, beta=beta)
      # create BetaModVar
      super$initialize(description, units, D=D, k=as.integer(1))
      # ensure first call to get() is valid
      #self$set("expected")
      # return BetaModVar
      return(invisible(self))
    },

    #' @description 
    #' Tests whether the model variable is probabilistic, i.e. a random
    #' variable that follows a distribution, or an expression involving
    #' random variables, some of which follow distributions. 
    #' @return TRUE if probabilistic
    is_probabilistic = function() {
      return(TRUE)
    }
    
    #' #' @description 
    #' #' Accessor function for the name of the uncertainty distribution.
    #' #' @return Distribution name as character string.
    #' distribution = function() {
    #'   rv <- paste('Be(', private$alpha, ',', private$beta, ')', sep='')
    #'   return(rv)
    #' },
    #' 
    #' #' @description 
    #' #' Return the expected value of the distribution. 
    #' #' @return Expected value as a numeric value.
    #' mean = function() {
    #'   return(private$alpha/(private$alpha+private$beta))
    #' },
    #' 
    #' #' @description 
    #' #' Return the mode of the distribution (if \code{alpha}, \code{beta} > 1) 
    #' #' @return mode as a numeric value.
    #' mode = function() {
    #'   rv <- as.numeric(NA)
    #'   if (private$alpha==1 && private$beta==1) {
    #'     rv <- 0.5
    #'   } else if (private$alpha<1 && private$beta < 1) {
    #'     rv <- as.numeric(NA) # bimodal
    #'   } else if (private$alpha<=1 && private$beta>1) {
    #'     rv <- 0
    #'   } else if (private$alpha>1 && private$beta<=1) {
    #'     rv <- 1
    #'   } else {
    #'     rv <- (private$alpha-1)/(private$alpha+private$beta-2) 
    #'   }
    #'   return(rv)
    #' },
    #' 
    #' #' @description Return the standard deviation of the distribution. 
    #' #' @return Standard deviation as a numeric value
    #' SD = function() {
    #'   a <- private$alpha
    #'   b <- private$beta
    #'   v <- (a*b) / ( (a+b)^2 * (a+b+1) )
    #'   return(sqrt(v))
    #' },
    #' 
    #' #' @description Draw a random sample from the model variable. 
    #' #' @param n Number of samples to draw.
    #' #' @return Samples drawn at random.
    #' r = function(n=1) {
    #'   rv <- rbeta(n, shape1=private$alpha, shape2=private$beta)
    #'   return(rv)
    #' },
    #' 
    #' #' @description
    #' #' Return the quantiles of the Beta uncertainty distribution.
    #' #' @param probs Vector of probabilities, in range [0,1].    
    #' #' @return Vector of quantiles.
    #' quantile = function(probs) {
    #'   # test argument
    #'   sapply(probs, FUN=function(x) {
    #'     if (is.na(x)) {
    #'       rlang::abort("All elements of 'probs' must be defined",
    #'                    class="probs_not_defined")
    #'     }
    #'     if (!is.numeric(x)) {
    #'       rlang::abort("Argument 'probs' must be a numeric vector",
    #'                    class="probs_not_numeric")
    #'     }
    #'     if (x<0 || x>1) {
    #'       rlang::abort("Elements of 'probs' must be in range[0,1]",
    #'                    class="probs_out_of_range")
    #'     }
    #'   })
    #'   q <- qbeta(probs, shape1=private$alpha, shape2=private$beta)
    #'   return(q)
    #' }
    
  )
)
