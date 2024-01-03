#' @title A model variable whose uncertainty follows a Normal distribution
#' @description An R6 class representing a model variable with Normal
#' uncertainty.
#' @details A model variable for which the uncertainty in its point estimate can
#' be modelled with a Normal distribution. The hyperparameters of the
#' distribution are the mean (\code{mu}) and the standard deviation (\code{sd})
#' of the uncertainty distribution. The value of \code{mu} is the expected value
#' of the variable. Inherits from class \code{ModVar}.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
NormModVar <- R6::R6Class(
  classname = "NormModVar",
  lock_class = TRUE,
  inherit = ModVar,
  private = list(
  ),
  public = list(

    #' @description Create a model variable with normal uncertainty.
    #' @param description A character string describing the variable.
    #' @param units Units of the quantity; character string.
    #' @param mu Hyperparameter with mean of the Normal distribution for
    #' the uncertainty of the variable.
    #' @param sigma Hyperparameter equal to the standard deviation of the
    #' normal distribution for the uncertainty of the variable.
    #' @return A \code{NormModVar} object.
    initialize = function(description, units, mu, sigma) {
      # create a normal distribution and check arguments
      D <- NormalDistribution$new(mu, sigma)
      # initialize the base class
      super$initialize(description, units, D = D, k = 1L)
      # return
      return(invisible(self))
    },

    #' @description Tests whether the model variable is probabilistic.
    #' @return \code{TRUE} if probabilistic.
    is_probabilistic = function() {
      return(TRUE)
    }
  )
)
