#' @title 
#' ExpressionModelVariable
#' 
#' @description 
#' An R6 class for a model variable constructed from an expression
#' involving other model variables.
#' 
#' @details A class to support expressions involving objects
#' of base class ModelVariable, which itself behaves like a 
#' model variable. For example if \code{A} and
#' \code{B} are variables with base class \code{ModelVariable}
#' and \code{c} is a variable of type \code{numeric}, then
#' it is not possible to write, for example, 
#' \code{x <- 42*A/B + c}, because R cannot manipulate class
#' variables using the same operators as regular variables. 
#' But such forms of expression may be desirable in constructing a
#' model and this class provides a mechanism for doing so.
#' 
#' @note Methods `getSD` and `getQuantile` return NA. For many expressions
#' involving model variables there will be no closed form expression
#' for these values, and they would normally be obtained by simulation.
#' Method `getDistribution` returns the string representation of the
#' expression used to create the model variable.
#' 
#' @author Andrew J. Sims \email{andrew.sims5@@nhs.net}
#' @docType class
#' @export
#' 
ExpressionModelVariable <- R6::R6Class(
  classname = "ExpressionModelVariable",
  inherit =  ModelVariable,
  private = list(
    # fields
    expr = 'call',
    expr.value = 'call',
    expr.mean = 'call',
    env = 'environment',

    # function to create an expression involving calling ModelVariable
    # methods. For example if method='value()', each model variable X
    # in the expression will be replaced by X$value().
    # Returns a substituted expression.
    subExpr = function(expr, method) {
      # paired list of variables to substitute
      mv <- list()
      # substitute model variable names with call to value() method
      for (v in all.vars(expr)) {
        # v is a string containing a variable name
        if (inherits(eval(str2lang(v), envir=private$env), what='ModelVariable')) {
          rep <- gsub(v, paste(v, method, sep='$'), v)
        }
        else {
          rep <- v
        }
        mv[[v]] <- str2lang(rep)
      }
      emod <- eval(substitute(substitute(e, env=mv), env=list(e=expr)))
      return(emod)
    }
  ),
  public = list(
    
    #' @description 
    #' Create a Model Variable formed from an expression involving other
    #' model variables.
    #' @param label A character string label for the variable. It is advised
    #' to make this the same as the variable name which helps when tabulating
    #' model variables involving ExpressionModelVariables.
    #' @param description Name for the model variable expresssion. In 
    #' a complex model it may help to tabulate how model variables are
    #' combined into costs, probablities and rates.
    #' @param units Units in which the variable is expressed.
    #' @param expr An R expression involving model variables which would be 
    #' syntactically correct were each model variable to be replaced by
    #' numerical variables.
    #' @param envir The environment in which the model variables live. Normally,
    #' and by default, this is the global environment. But if an object is
    #' created which refers to model variables created in a different 
    #' environment it must be specified. If creating an object from
    #' within a function, for example, set `envir=environment()` in the
    #' parameter list.
    #' @return An object of type ExpressionModelVariable
    initialize = function(label, description, units, expr, envir=globalenv()) {
      super$initialize(label, description, units)
      # check and save arguments
      if (!is.call(expr)) {
        warning("ExpressionModelVariable$new: expr must be of type 'call'")
      }
      private$expr <- expr
      if (!is.environment(envir)) {
        stop("ExpressionModelVariable$new: envir must be of type 'environment'")
      }
      private$env <- envir
      # parse the expression
      private$expr.value <- private$subExpr(expr, 'value()')
      private$expr.mean <- private$subExpr(expr, 'getMean()')
      # set this model variable and its operands to their expected values
      self$sample(T)
    },

    #' @description
    #' Set the value of the model variable from its uncertainty distribution.
    #' Nothing is returned; the sampled value is returned at the next
    #' call to `value()`.
    #' @param expected Logical; if TRUE, sets the value of the model variable
    #'        returned at subsequent calls to `value()` to be equal to the 
    #'        expectation of the variable. Default is FALSE.
    #' @return Updated ExpressionModelVariable object.
    sample = function(expected=F) {
      sapply(self$getOperands(), FUN=function(o) {
        o$sample(expected)        
      })
      return(invisible(self))
    },

    #' @description
    #' Evaluate the expression.
    #' @return Numerical value of the evaluated expression.
    value = function() {
      rv <- eval(private$expr.value, envir=private$env)
      return(rv)  
    },
    
    #' @description 
    #' Accessor function for the name of the expression model variable.
    #' @return Expression as a character string.
    getDistribution = function() {
      ex <- deparse(private$expr)
      return(ex)
    },
    
    #' @description 
    #' Return the expected value of the expression variable. 
    #' @return Expected value as a numeric value.
    getMean = function() {
      rv <- eval(private$expr.mean, envir=private$env)
      return(rv)  
    },
    
    #' @description 
    #' Return a list of operands that are themselves ModelVariables given
    #' in the expression.
    #' @return A list of model variables.
    getOperands = function() {
      # filter the expression variables that are ModelVariables
      mvlist <- list()
      sapply(all.vars(private$expr), FUN=function(v) {
        vv <- eval(str2lang(v), envir=private$env)
        if (inherits(vv, what='ModelVariable')) {
          # add the variable to the list
          mvlist <<- c(mvlist, vv)
          # and add its operands, if any
          sapply(vv$getOperands(), FUN=function(o) {
            mvlist <<- c(mvlist, o)
          })
        }
      })
      return(mvlist)
    }

  )
)
