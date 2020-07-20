#' @title 
#' ExprModVar
#' 
#' @description 
#' An R6 class for a model variable constructed from an expression
#' involving other model variables.
#' 
#' @details A class to support expressions involving objects
#' of base class ModVar, which itself behaves like a 
#' model variable. For example if \code{A} and
#' \code{B} are variables with base class \code{ModVar}
#' and \code{c} is a variable of type \code{numeric}, then
#' it is not possible to write, for example, 
#' \code{x <- 42*A/B + c}, because R cannot manipulate class
#' variables using the same operators as regular variables. 
#' But such forms of expression may be desirable in constructing a
#' model and this class provides a mechanism for doing so.
#' 
#' @note For many expressions involving model variables there will 
#' be no closed form expressions for the standard deviation an
#' the quantiles. Therefore they are obtained by simulation.
#' Method `getDistribution` returns the string representation of the
#' expression used to create the model variable.
#' 
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @docType class
#' @export
#' 
ExprModVar <- R6::R6Class(
  classname = "ExprModVar",
  inherit =  ModVar,
  private = list(
    # fields
    expr = NULL,
    expr.value = NULL,
    expr.mean = NULL,
    env = NULL,
    Nsim = 1000,

    # function to create an expression involving calling ModVar
    # methods. For example if method='value()', each model variable X
    # in the expression will be replaced by X$value().
    # Returns a substituted expression.
    subExpr = function(expr, method) {
      # paired list of variables to substitute
      mv <- list()
      # substitute model variable names with call to value() method
      for (v in all.vars(expr)) {
        # v is a string containing a variable name
        if (inherits(eval(str2lang(v), envir=private$env), what='ModVar')) {
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
    #' @param description Name for the model variable expresssion. In 
    #' a complex model it may help to tabulate how model variables are
    #' combined into costs, probablities and rates.
    #' @param units Units in which the variable is expressed.
    #' @param quo A quosure (see package rlang), which contains an expression
    #' and its environment. The usage is `quo(x+y)` or `rlang::quo(x+y)`.
    #' @return An object of type ExprModVar
    initialize = function(description, units, quo) {
      # initialize the base class
      super$initialize(description, units)
      # check and process the quosure
      if (!rlang::is_quosure(quo)) {
        rlang::abort("Argument quo must be a quosure", class="quo_not_quosure")
      }
      private$expr <- rlang::quo_get_expr(quo)
      private$env <- rlang::quo_get_env(quo)
      # parse the expression
      private$expr.value <- private$subExpr(private$expr, 'value()')
      private$expr.mean <- private$subExpr(private$expr, 'getMean()')
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
    #' @return Updated ExprModVar object.
    sample = function(expected=F) {
      sapply(self$operands(), FUN=function(o) {
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
    #' @return Expression as a character string with all control characters
    #' having been removed.
    distribution = function() {
      estr <- rlang::expr_text(private$expr)
      estr <- gsub("[[:cntrl:]]", "", estr)
      return(estr)
    },
    
    #' @description 
    #' Return the expected value of the expression variable. 
    #' @return Expected value as a numeric value.
    mean = function() {
      rv <- eval(private$expr.mean, envir=private$env)
      return(rv)  
    },
    
    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    SD = function() {
      # sample values from the variable
      S <- self$r(private$Nsim)
      return(sd(S))
    },

    #' @description
    #' Return the quantiles by sampling the variable.
    #' @param probs Vector of probabilities, in range [0,1].    
    #' @return Vector of quantiles.
    quantile = function(probs) {
      # check inputs
      sapply(probs, FUN=function(x) {
        if (!is.numeric(probs)) {
          stop("ExprModVar$getQuantile: argument must be a numeric vector")
        }
      })
      # sample the distribution
      S <- self$r(private$Nsim)
      # return the quantiles of the sample
      q <- quantile(S, probs)
      return(q)
    },

    #' @description 
    #' Return a list of operands that are themselves ModVars given
    #' in the expression.
    #' @return A list of model variables.
    operands = function() {
      # filter the expression variables that are ModVars
      mvlist <- list()
      sapply(all.vars(private$expr), FUN=function(v) {
        vv <- eval(str2lang(v), envir=private$env)
        if (inherits(vv, what="ModVar")) {
          # add the variable to the list
          mvlist <<- c(mvlist, vv)
          # and add its operands, if any
          sapply(vv$operands(), FUN=function(o) {
            mvlist <<- c(mvlist, o)
          })
        }
      })
      return(mvlist)
    }

  )
)
