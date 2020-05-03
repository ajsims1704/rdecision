#' @title
#' ModelVariable
#' 
#' @description
#' An R6 class for a variable in an health economic model
#' 
#' @details 
#' Base class for a variable used in a health economic model. The base 
#' class, which is not intended to be directly instantiated by model
#' applications, wraps a numerical value which is used in calculations.
#' The base class provides a framework for creating classes of model
#' variables whose uncertainties are described by statistical distributions
#' parametrized with hyperparameters.
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims5@@nhs.net}
#' @export 
#' 
ModelVariable <- R6::R6Class(
  classname = "ModelVariable",
  private = list(
    description = 'character',
    units = 'character',
    val = 'numeric'
  ),
  public = list(
    
    #' @description 
    #' Create an object of type `ModelVariable`
    #' @param description A character string description of the variable
    #'        and its role in the
    #'        model. This description will be used in a tabulation of the
    #'        variables linked to a model.
    #' @param units A character string description of the units, e.g. 'GBP',
    #'        'per year'.
    #' @return A new ModelVariable object.
    initialize = function(description, units) {
      private$description <- description
      private$units <- units
      private$val <- 0
    },
    
    #' @description
    #' Set the value of the model variable from its uncertainty distribution.
    #' Nothing is returned; the sampled value is returned at the next
    #' call to `value()`.
    #' @param expected Logical; if TRUE sets the value of the model variable
    #'        returned at subsequent calls to `value()` to be equal to the 
    #'        expectation of the variable. Default is FALSE.
    #' @return Updated ModelVariable object.
    sample = function(expected=F) {
      private$val <- 0
      return(invisible(self))
    },
    
    #' @description
    #' Return the current value of the model variable. This will be the 
    #' expected value if the argument to the most recent call to `sample`
    #' was TRUE or after creation of the object; otherwise it will return
    #' a value sampled from the uncertainty distribution. 
    #' @return Numeric value of the model variable.
    value = function() {
      return(private$val)  
    },
    
    #' @description
    #' Accessor functions for the description.
    #' @return Description of model variable as character string.
    getDescription = function() {
      return(private$description)
    },
    
    #' @description
    #' Accessor function for units.
    #' @return Description of units as character string.
    getUnits = function() {
      return(private$units)
    },
    
    #' @description 
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string.
    getDistribution = function() {
      return(NA)
    },
    
    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    getMean = function() {
      return(NA)
    },
    
    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    getSD = function() {
      return(NA)
    },
    
    #' @description 
    #' Find quantiles of the uncertainty distribution. 
    #' @param probs Numeric vector of probabilities, each in range [0,1].
    #' @return Vector of numeric values of the same length as `probs`.
    getQuantile = function(probs) {
      return(NA)
    }
  )
)
