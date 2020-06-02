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
    label = as.character(NA),
    description = as.character(NA),
    units = as.character(NA),
    val = 0,
    
    # Function to find self in an environment
    # Uses a looping environment walk function adapted from Hadley Whickham's
    # 'Advanced R'. Starts at leaf and walks towards the ancestor
    # empty environment and updates myenv if it finds a match. Successful
    # matches are overwritten by matches nearer the root of the tree, so
    # that if a reference copy of a variable exists it will cede priority
    # to its origin nearer the root. 
    whereami = function(env = rlang::caller_env()) {
      myenv <- rlang::empty_env()
      while (!identical(env, rlang::empty_env())) {
        # check if I am in this environment
        sapply(rlang::env_names(env), FUN=function(objname) {
          obj <- rlang::env_get(env, objname)
          if (identical(obj, self)) myenv <<- env
        })
        # inspect parent
        env <- rlang::env_parent(env)
      }
      return(myenv)
    }

  ),
  public = list(
    
    #' @description 
    #' Create an object of type `ModVar`
    #' @param description A character string description of the variable
    #'        and its role in the
    #'        model. This description will be used in a tabulation of the
    #'        variables linked to a model.
    #' @param units A character string description of the units, e.g. 'GBP',
    #'        'per year'.
    #' @return A new ModVar object.
    initialize = function(description, units) {
      private$description <- description
      private$units <- units
    },

    #' @description Find the environment nearest the global environment in which
    #' this model variable or a reference to it, lives.
    #' @return An environment. If not found, returns the empty environment.
    # * Don't call from class methods; use private$whereami instead. *
    get_environment = function() {
      myenv <- private$whereami(rlang::caller_env())
      return(myenv)
    },   
    
    #' @description 
    #' Is this ModVar an expression?
    #' @return TRUE if it inherits from ExprModVar, FALSE otherwise.
    isExpression = function() {
      return(inherits(self, what='ExprModVar'))
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
    #' Accessor function for the label. Unless `set_label` has been called, this
    #' function will return the variable's own name. As far as possible, this will 
    #' be the variable name used when the object was first created in the model,
    #' so that it aligns with the variable name used in ExprModVar
    #' and tabulations of variables used in models. But due to the nature of R's
    #' non-standard evaluation, this is not ensured. It will sometimes return
    #' the name of a reference to the original variable, if
    #' the original environment is not an ancestor of the calling environment of
    #' this function. In cases where references to model variables are routinely
    #' created, destroyed and passed around, it is advised to call this method
    #' from the environment of the original object as soon as possible after
    #' it is created. Subsequent calls will return the original name.
    #' @return Label of model variable as character string.
    get_label = function() {
      if (is.na(private$label)) {
        # find the environment, starting with parent
        my.env <- private$whereami(rlang::caller_env()) 
        if (!identical(my.env, rlang::empty_env())) {
          sapply(rlang::env_names(my.env), FUN=function(on){
            v <- eval(rlang::parse_expr(on), envir=my.env)
            if (identical(v,self)) private$label <- on
          })      
        }
      }
      return(private$label)
    },
    
    #' @description 
    #' Function to set the label. Normally this is not required because the label
    #' is set to the name given to the variable by the user. This function allows
    #' the default behaviour to be overridden.
    #' @param label The label to use.
    #' @return Updated ModVar object
    set_label = function(label) {
      if (!is.character(label)) {
        stop("ModVar$set_label: argument 'label' must be a character string",
             call.=FALSE)
      }
      private$label <- label
      return(invisible(self))
    },
    
    #' @description 
    #' Function to unset the label. Causes the label to revert to its default, i.e.
    #' its variable name at the next call to `get_label`.
    #' @return Updated ModVar object
    unset_label = function() {
      private$label <- as.character(NA)
      return(invisible(self))
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
    #' ExprModVar, but defined for the base class for convenience to
    #' avoid type checking inside iterators.
    #' @return A list of operands that are themselves ModVars. An empty list 
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
    tabulate = function(include.operands=FALSE) {
      # create list of model variables
      mvlist <- list(self)
      if (include.operands) {
        mvlist <- c(mvlist, self$getOperands())
      }
      # create a data frame of model variables
      DF <- data.frame(
        Label = sapply(mvlist, FUN=function(x){x$get_label()}),
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
        stop('ModVar$r: argument n must be numeric', call.=FALSE)
      }
      if (n < 1) {
        stop('ModVar$r: argument n must be at least 1', call.=FALSE)
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