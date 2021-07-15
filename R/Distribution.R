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
    k = NULL
  ),
  public = list(
    
    #' @description Create an object of class \code{Distribution}.
    #' @param name Name of the distribution ("Beta" etc.)
    #' @param k Order of the distribution (1=univariate etc.). Must be an 
    #' integer; use \code{as.integer()} to avoid an error.
    #' @return An object of class \code{Distribution}. 
    initialize = function(name, k=as.integer(1)) {
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
      if (!is.integer(k)) {
        rlang::abort(
          "Argument 'k must be an integer", 
          class = "invalid_order"
        )
      }
      if (k <= 0) {
        rlang::abort(
          "Argument 'k' must be > 0", 
          class="order_not_supported"
        )
      }
      private$k <- k
      # return Distribution
      return(invisible(self))
    }
    
    #' #' @description 
    #' #' Tests whether the model variable is probabilistic, i.e. a random
    #' #' variable that follows a distribution, or an expression involving
    #' #' random variables, some of which follow distributions. 
    #' #' @return TRUE if probabilistic
    #' is_probabilistic = function() {
    #'   return(TRUE)
    #' },
    #' 
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
