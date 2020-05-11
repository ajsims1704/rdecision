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
    label = 'character',
    description = 'character',
    units = 'character',
    val = 'numeric'
  ),
  public = list(
    
    #' @description 
    #' Create an object of type `ModelVariable`
    #' @param label A character string label for the variable. It is advised
    #' to make this the same as the variable name which helps when tabulating
    #' model variables involving ExpressionModelVariables.
    #' @param description A character string description of the variable
    #'        and its role in the
    #'        model. This description will be used in a tabulation of the
    #'        variables linked to a model.
    #' @param units A character string description of the units, e.g. 'GBP',
    #'        'per year'.
    #' @return A new ModelVariable object.
    initialize = function(label, description, units) {
      private$label <- label
      private$description <- description
      private$units <- units
      private$val <- 0
    },
 
    #' @description 
    #' Is this ModelVariable an expression?
    #' @return TRUE if it inherits from ExpressionModelVariable, FALSE otherwise.
    isExpression = function() {
      return(inherits(self, what='ExpressionModelVariable'))
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
    #' Accessor function for the label.
    #' @return Label of model variable as character string.
    getLabel = function() {
      return(private$label)
    },
    
    #' @description
    #' Accessor function for the description.
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
    #' Return a list of operands given in the expression used to form the
    #' expression. Only relevant for objects of inherited type 
    #' ExpressionModelVariable, but defined for the base class for convenience to
    #' avoid type checking inside iterators.
    #' @return A list of operands that are themselves ModelVariables. An empty list 
    #' for non-expression model variables.
    getOperands = function() {
      return(list())
    },

    #' @description 
    #' Tabulate the model variable and optionally include its operands.
    #' @param include.operands If TRUE include the operands of this model
    #' variable in the table. Otherwise return a table with one row, 
    #' describing this variable.
    #' @return Data frame with one row per model variable, as follows:
    #' \describe{
    #' \item{Label}{The label given to the variable on creation.}
    #' \item{Description}{As given at initiialization.}
    #' \item{Units}{Units of the variable.}
    #' \item{Distribution}{Either the uncertainty distribution, if
    #' it is a regular model variable, or the expression used to create it,
    #' if it is an ExpressionModelVariable.}
    #' \item{Mean}{Expected value.}
    #' \item{SD}{Standard deviation.}
    #' \item{Q2.5}{2.5% quantile.}
    #' \item{Q97.5}{97.5% quantile.}
    #' \item{Qhat}{Asterisk (*) if the quantiles and SD have been estimated
    #' by random sampling.}
    #' }
    tabulate = function(include.operands=FALSE) {
      # create list of model variables
      mvlist <- list(self)
      if (include.operands) {
        mvlist <- c(mvlist, self$getOperands())
      }
      # create a data frame of model variables
      DF <- data.frame(
        Label = sapply(mvlist, FUN=function(x){x$getLabel()}),
        Description = sapply(mvlist, FUN=function(x){x$getDescription()}),
        Units = sapply(mvlist, FUN=function(x){x$getUnits()}),
        Distribution = sapply(mvlist, FUN=function(x){x$getDistribution()}),
        Mean = sapply(mvlist, FUN=function(x){x$getMean()}),
        SD = sapply(mvlist, FUN=function(x){x$getSD()}),
        Q2.5 = sapply(mvlist, FUN=function(x){x$getQuantile(probs=c(0.025))}),
        Q97.5 = sapply(mvlist, FUN=function(x){x$getQuantile(probs=c(0.975))}),
        Qhat = sapply(mvlist, FUN=function(exp){return(ifelse(exp$isExpression(),'*',''))})
      )
      # Return the table
      return(DF)
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
    },
    
    #' @description 
    #' Draw random samples from the model variable. After returning the
    #' sample, the next call to `value()` will return the expected value.
    #' @param n Number of samples to return
    #' @return Numeric vector of samples drawn at random.
    r = function(n) {
      # check the input
      if (!is.numeric(n)) {
        stop('ModelVariable$r: argument n must be numeric')
      }
      if (n < 1) {
        stop('ModelVariable$r: argument n must be at least 1')
      }
      # make random draws
      rv <- vector(mode='numeric', length=n)
      for (i in 1:n) {
        self$sample(expected=F)
        rv[i] <- self$value()
      }
      # reset the variable 
      self$sample(expected=T)
      # return the sample
      return(rv)
    }
  )
)
