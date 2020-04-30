#' @title
#' ConstModelVariable
#' 
#' @description 
#' An R6 class for a constant in a model
#' 
#' @details
#' A ModelVariable with no uncertainty in its value. It has no
#' distribution and there are no hyperparameters. Its 
#' benefit over using a regular 'numeric' variable in a model is that
#' it will appear in automatic tabulations of the model variables
#' associated with a model and therefore be explicitly documented
#' as a model input.
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims5@nhs.net}
#' @export
#' 
ConstModelVariable <- R6::R6Class(
  classname = "ConstModelVariable",
  inherit = ModelVariable,
  
  public = list(
    
    #' @description 
    #' Create a new constant model variable
    #' @param description A character string description of the variable
    #'        and its role in the
    #'        model. This description will be used in a tabulation of the
    #'        variables linked to a model.
    #' @param units A character string description of the units, e.g. 'GBP',
    #'        'per year'.
    #' @param const The constant numerical value of the object.
    #' @return A new ModelVariable object.
    initialize = function(description, units, const) {
      super$initialize(description, units)
      private$val <- const
    },
    
    #' @description
    #' Not applicable for a constant variable, no action is taken on sampling.
    #' @param expected Logical; ignored.
    #' @return Updated ModelVariable object.
    sample = function(expected=F) {
      return(invisible(self))
    },

    #' @description 
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string.
    getDistribution = function() {
      return("Constant")
    },
    
    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    getMean = function() {
      return(private$val)
    },
    
    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    getSD = function() {
      return(0)
    },
    
    #' @description 
    #' Quantiles of the uncertainty distribution; for a constant all
    #' quantiles are returned as the value of the constant.
    #' @param probs Numeric vector of probabilities, each in range [0,1].
    #' @return Vector of numeric values of the same length as `probs`.
    getQuantile = function(probs) {
      sapply(probs, FUN=function(x) {
        if (!is.numeric(probs)) {
          stop("ConstModelVariable$getQuantile: argument must be a numeric vector")
        }
      })
      q <- rep(private$val, times=length(probs))
      return(q)
    }

  )
)
