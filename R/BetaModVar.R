#' @title A model variable whose uncertainty follows a Beta distribution
#' 
#' @description An R6 class representing a model variable whose uncertainty
#' is described by a Beta distribution.
#' 
#' @details A model variable for which the uncertainty in the point estimate can
#' be modelled with a Beta distribution. The hyperparameters of the
#' distribution are the shape parameters (\code{alpha} and \code{beta}) of
#' the uncertainty distribution. Inherits from class \code{ModVar}.
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
  ),
  public = list(
    
    #' @description Create an object of class \code{BetaModVar}.
    #' @param description A character string describing the variable.
    #' @param units Units of the variable, as character string.
    #' @param alpha parameter of the Beta distribution.
    #' @param beta parameter of the Beta distribution.
    #' @return An object of class \code{BetaModVar}. 
    initialize = function(description, units, alpha, beta) {
      # create Beta distribution (also checks arguments)
      D <- BetaDistribution$new(alpha=alpha, beta=beta)
      # create BetaModVar
      super$initialize(description, units, D=D, k=as.integer(1))
      # return BetaModVar
      return(invisible(self))
    },

    #' @description Tests whether the model variable is probabilistic, 
    #' i.e. a random variable that follows a distribution, or an expression
    #' involving random variables, some of which follow distributions. 
    #' @return TRUE if probabilistic
    is_probabilistic = function() {
      return(TRUE)
    }
    
  )
)
