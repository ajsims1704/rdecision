#' @title 
#' ModelVariableExpression
#' 
#' @description 
#' An R6 class for an expression involving model variables
#' 
#' @details A class to support expressions involving objects
#' of base class ModelVariable. For example if \code{A} and
#' \code{B} are variables with base class \code{ModelVariable}
#' and \code{c} is a variable of type \code{numeric}, then
#' it is not possible to write, for example, 
#' \code{x <- 42*A/B + c}, because R cannot manipulate class
#' variables using the same operators as regular variables. 
#' But such forms of expression may be desirable in constructing a
#' model and this class provides a mechanism for doing so.
#' Instances of the class should be created by providing
#' an expression involving model variables, and evaluated
#' by making a call to \code{eval}. 
#' In its simplest form, if \code{A} is a model variable, then
#' quote(A) is a ModelVariableExpression.
#' 
#' @note
#' TO DO: does not work if expr is not in the global namespace.
#' 
#' @author Andrew J. Sims \email{andrew.sims5@@nhs.net}
#' @docType class
#' @export
#' 
ModelVariableExpression <- R6::R6Class(
  classname = "ModelVariableExpression",
  private = list(
    # fields
    expr = 'call',
    expr.value = 'call',
    description = 'character',
    
    # function to create an expression involving ModelVariables
    parse.expr = function(expr, expected=F) {
      # paired list of variables to substitute
      mv <- list()
      # sampled or expected
      meth <- 'value()'
      # substitute model variable names with call to value() method
      for (v in all.vars(expr)) {
        # v is a string containing a variable name
        if (inherits(eval(str2lang(v)), what='ModelVariable')) {
          rep <- gsub(v, paste(v, meth, sep='$'), v)
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
    #' Create a model variable expression
    #' @param expr An R expression involving model variables which would be 
    #' syntactically correct were each model variable to be replaced by
    #' numerical variables.
    #' @param description Label for the model variable expresssion. In 
    #' a complex model it may help to tabulate how model variables are
    #' combined into costs, probablities and rates.
    #' @return An object of type ModelVariableExpression
    initialize = function(expr, description=NA) {
      private$expr <- expr
      private$expr.value <- private$parse.expr(expr)
      private$description <- description
    },
    
    #' @description
    #' Evaluate the expression.
    #' @return Numerical value of the evaluated expression.
    eval = function() {
      rv <- eval(private$expr.value)
      return(rv)  
    },
    
    #' @description 
    #' Return a list of named ModelVariables associated with the expression.
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
  )
)
