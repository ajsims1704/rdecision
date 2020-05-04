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
#' Methods `getSD` and `getQuantile` return NA. For many expressions
#' involving model variables there will be no closed form expression
#' for these values, and they would normally be obtained by simulation.
#' Method `getDistribution` returns the string representation of the
#' expression used to create the model variable.
#' 
#' @note
#' TO DO: does not work if expr is not in the global namespace.
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
        if (inherits(eval(str2lang(v)), what='ModelVariable')) {
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
    #' @param description Label for the model variable expresssion. In 
    #' a complex model it may help to tabulate how model variables are
    #' combined into costs, probablities and rates.
    #' @param units Units in which the variable is expressed.
    #' @param expr An R expression involving model variables which would be 
    #' syntactically correct were each model variable to be replaced by
    #' numerical variables.
    #' @return An object of type ExpressionModelVariable
    initialize = function(description, units, expr) {
      super$initialize(description, units)
      # parse the expression
      private$expr <- expr
      private$expr.value <- private$subExpr(expr, 'value()')
      private$expr.mean <- private$subExpr(expr, 'getMean()')
      # set all model variables to their expected values
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
      mvlist <- self$getModelVariables()
      sapply(mvlist, FUN=function(mv) {
        mv$sample(expected)        
      })
      return(invisible(self))
    },

    #' @description
    #' Evaluate the expression.
    #' @return Numerical value of the evaluated expression.
    value = function() {
      rv <- eval(private$expr.value)
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
      rv <- eval(private$expr.mean)
      return(rv)  
    },
    
    #' @description 
    #' Return a list of ModelVariables given in the expression.
    #' @return A named list of model variables. Each member of the list is
    #' named with a character string of the variable name used in constructing
    #' the model. This is not necessary for evaluation, but helps when tabulating
    #' the variables used in a model.
    getModelVariables = function() {
      vars <- all.vars(private$expr)
      names(vars) <- vars
      mv <- lapply(vars, FUN=function(v) {
        return(eval(str2lang(v)))
      })
      lv <- sapply(mv, FUN=function(v) {
        return(inherits(v, what='ModelVariable'))
      })
      return(mv[lv])
    }
    
    # @description
    #' Tabulate all model variables in the expression.
    # @return Data frame with one row per model variable.
    #tabulateModelVariables = function() {
    #  mvlist <- self$getModelVariables()
    #  DF <- data.frame(
    #    Variable = names(mvlist),
    #    Description = sapply(mvlist, FUN=function(x){x$getDescription()}),
    #    Units = sapply(mvlist, FUN=function(x){x$getUnits()}),
    #    Distribution = sapply(mvlist, FUN=function(x){x$getDistribution()}),
    #    Mean = sapply(mvlist, FUN=function(x){x$getMean()}),
    #    SD = sapply(mvlist, FUN=function(x){x$getSD()}),
    #    Q2.5 = sapply(mvlist, FUN=function(x){x$getQuantile(probs=c(0.025))}),
    #    Q97.5 = sapply(mvlist, FUN=function(x){x$getQuantile(probs=c(0.975))})
    #  )
    #  return(DF)
    #}

  )
)
