#' @title A constant model variable
#' @description An R6 class representing a constant in a model.
#' @details A \code{ModVar} with no uncertainty in its value. Its distribution
#' is treated as a Dirac delta function \eqn{\delta(x-c)} where \eqn{c} is the
#' hyperparameter (value of the constant). The benefit over
#' using a regular numeric variable in a model is that it will appear in
#' tabulations of the model variables associated with a model and therefore be
#' explicitly documented as a model input. Inherits from class \code{ModVar}.
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
ConstModVar <- R6::R6Class(
  classname = "ConstModVar",
  lock_class = TRUE,
  inherit = ModVar,
  private = list(
  ),
  public = list(

    #' @description Create a new constant model variable.
    #' @param description A character string description of the variable
    #' and its role in the model. This description will be used in a tabulation
    #' of the variables linked to a model.
    #' @param units A character string description of the units, e.g. "GBP",
    #' "per year".
    #' @param const The constant numerical value of the object.
    #' @return A new \code{ConstModVar} object.
    initialize = function(description, units, const) {
      # initialize the distribution (also checks argument)
      D <- DiracDistribution$new(const)
      # initialize the base class
      super$initialize(description, units, D, k = 1L)
      # return object
      return(invisible(self))
    },

    #' @description Tests whether the model variable is probabilistic.
    #' @details Does the random variable follow a distribution, or is it an
    #' expression involving' random variables, some of which follow
    #' distributions?
    #' @return TRUE if probabilistic
    is_probabilistic = function() {
      return(FALSE)
    }
  )
)
