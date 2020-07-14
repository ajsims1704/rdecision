#' @title
#' ModVar
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
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export 
#' 
ModVar <- R6::R6Class(
  classname = "ModVar",
  private = list(
    mv.description = NULL,
    mv.units = NULL,
    val = 0
  ),
  public = list(
    
    #' @description 
    #' Create an object of type `ModVar`
    #' @param description A character string description of the variable
    #' and its role in the model. This description will be used in a
    #' tabulation of the variables linked to a model.
    #' @param units A character string description of the units, e.g. 'GBP',
    #' 'per year'.
    #' @return A new ModVar object.
    initialize = function(description, units) {
      # test and set description
      if (!is.character(description)) {
        rlang::abort("Argument 'description' must be a string", 
                     class="description_not_string")
      }
      private$mv.description <- description
      if (!is.character(units)) {
        rlang::abort("Argument 'units' must be a string", 
                     class="units_not_string")
      }
      private$mv.units <- units
    },

    #' @description 
    #' Is this ModVar an expression?
    #' @return TRUE if it inherits from ExprModVar, FALSE otherwise.
    is_expression = function() {
      return(inherits(self, what="ExprModVar"))
    },
    
    #' @description
    #' Set the value of the model variable from its uncertainty distribution.
    #' Nothing is returned; the sampled value is returned at the next
    #' call to `value()`.
    #' @param expected Logical; if TRUE sets the value of the model variable
    #'        returned at subsequent calls to `value()` to be equal to the 
    #'        expectation of the variable. Default is FALSE.
    #' @return Updated ModVar object.
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
    #' Accessor function for the description.
    #' @return Description of model variable as character string.
    description = function() {
      return(private$mv.description)
    },
    
    #' @description
    #' Accessor function for units.
    #' @return Description of units as character string.
    units = function() {
      return(private$mv.units)
    },
    
    #' @description 
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      return(NA)
    },

    #' @description 
    #' Return a list of operands given in the expression used to form the
    #' expression. Only relevant for objects of inherited type 
    #' ExprModVar, but defined for the base class for convenience to
    #' avoid type checking inside iterators.
    #' @return A list of operands that are themselves ModVars. An empty list 
    #' for non-expression model variables.
    operands = function() {
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
    #' \item{Description}{As given at initialization.}
    #' \item{Units}{Units of the variable.}
    #' \item{Distribution}{Either the uncertainty distribution, if
    #' it is a regular model variable, or the expression used to create it,
    #' if it is an ExprModVar.}
    #' \item{Mean}{Expected value.}
    #' \item{SD}{Standard deviation.}
    #' \item{Q2.5}{p=0.025 quantile.}
    #' \item{Q97.5}{p=0.975 quantile.}
    #' \item{Qhat}{Asterisk (*) if the quantiles and SD have been estimated
    #' by random sampling.}
    #' }
#    tabulate = function(include.operands=FALSE) {
#      # create list of model variables
#      mvlist <- list(self)
#      if (include.operands) {
#        mvlist <- c(mvlist, self$getOperands())
#      }
#      # create a data frame of model variables
#      DF <- data.frame(
#        Label = sapply(mvlist, FUN=function(x){x$get_label()}),
#        Description = sapply(mvlist, FUN=function(x){x$getDescription()}),
#        Units = sapply(mvlist, FUN=function(x){x$getUnits()}),
#        Distribution = sapply(mvlist, FUN=function(x){x$getDistribution()}),
#        Mean = sapply(mvlist, FUN=function(x){x$getMean()}),
#        SD = sapply(mvlist, FUN=function(x){x$getSD()}),
#        Q2.5 = sapply(mvlist, FUN=function(x){x$getQuantile(probs=c(0.025))}),
#        Q97.5 = sapply(mvlist, FUN=function(x){x$getQuantile(probs=c(0.975))}),
#        Qhat = sapply(mvlist, FUN=function(exp){return(ifelse(exp$isExpression(),'*',''))})
#      )
#      # Return the table
#      return(DF)
#    },

     #' @description 
     #' Return the point estimate of the variable. 
     #' @return Point estimate as a numeric value.
     point_estimate = function() {
      return(NA)
     },

    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    mean = function() {
      return(NA)
    },
    
    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    SD = function() {
      return(NA)
    },
    
    #' @description 
    #' Find quantiles of the uncertainty distribution. 
    #' @param probs Numeric vector of probabilities, each in range [0,1].
    #' @return Vector of numeric values of the same length as `probs`.
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
      return(NA)
    },
    
    #' @description 
    #' Draw random samples from the model variable. After returning the
    #' sample, the next call to `value()` will return the expected value.
    #' @param n Number of samples to return
    #' @return Numeric vector of samples drawn at random.
    r = function(n) {
      # check the input
      if (is.na(n)) {
        rlang::abort("Argument 'n' must be defined", class="n_not_defined")
      }
      if (!is.numeric(n)) {
        rlang::abort("Argument 'n' must be numeric", class="non-numeric_n")
      }
      if (n < 1) {
        rlang::abort("Argument n must be at least 1", class="n_out_of_range")
      }
      # make random draws
      rv <- vector(mode="numeric", length=n)
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
