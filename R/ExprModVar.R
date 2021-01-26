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
#' be no closed form expressions for the mean, standard deviation and
#' the quantiles. Therefore they are obtained by simulation. Because
#' a unimodal distribution is not guaranteed, the mode() function
#' returns NA. 
#' 
#' @note Method `distribution` returns the string representation 
#' of the expression used to create the model variable.
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
#    expr.value = NULL,
#    expr.mean = NULL,
    env = NULL,
    nest = NA,
    val = NA

    # # function to create an expression involving calling ModVar
    # # methods. For example if method='value()', each model variable X
    # # in the expression will be replaced by X$value().
    # # Returns a substituted expression.
    # subExpr = function(expr, method) {
    #   # paired list of variables to substitute
    #   mv <- list()
    #   # substitute model variable names with call to value() method
    #   for (v in all.vars(expr)) {
    #     # v is a string containing a variable name
    #     if (inherits(eval(str2lang(v), envir=private$env), what='ModVar')) {
    #       rep <- gsub(v, paste(v, method, sep='$'), v)
    #     }
    #     else {
    #       rep <- v
    #     }
    #     mv[[v]] <- str2lang(rep)
    #   }
    #   emod <- eval(substitute(substitute(e, env=mv), env=list(e=expr)))
    #   return(emod)
    # }
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
    #' @param nest Sample size to be used to estimate the mean, SD and
    #' quantiles of the expression. Values less than 1000 (default) are
    #' unlikely to return meaningful estimates and will be rejected.
    #' @return An object of type ExprModVar
    initialize = function(description, units, quo, nest=1000) {
      # initialize the base class
      super$initialize(description, units)
      # check and process the quosure
      if (!rlang::is_quosure(quo)) {
        rlang::abort("Argument quo must be a quosure", class="quo_not_quosure")
      }
      private$expr <- rlang::quo_get_expr(quo)
      private$env <- rlang::quo_get_env(quo)
      # check and process the sample estimation size
      if (!is.numeric(nest)) {
        rlang::abort("Argument nest must be numeric", class="n_not_numeric")
      }
      if (nest < 1000) {
        rlang::abort("Argument nest must not be less than 1000", class="n_too_small")
      }
      private$nest <- nest
      # return
      return(invisible(self))
    },

    #' @description 
    #' Tests whether the model variable is probabilistic, i.e. a random
    #' variable that follows a distribution, or an expression involving
    #' random variables, at least one of which follows a distribution. 
    #' @return TRUE if probabilistic
    is_probabilistic = function() {
      # test each operand for whether it is probabilistic
      lv <- sapply(all.vars(private$expr), FUN=function(v) {
        rv <- as.logical(NA)
        vv <- eval(str2lang(v), envir=private$env)
        if (inherits(vv, what="ModVar")) {
          rv <- vv$is_probabilistic()
        } else if (inherits(vv, what="numeric")) {
          rv <- FALSE
        }
        return(rv)
      })
      return(any(lv))
    },

    #' #' @description
    #' #' Set the value of the model variable from its uncertainty distribution.
    #' #' Nothing is returned; the sampled value is returned at the next
    #' #' call to `value()`.
    #' #' @param expected Logical; if TRUE, sets the value of the model variable
    #' #'        returned at subsequent calls to `value()` to be equal to the 
    #' #'        expectation of the variable. Default is FALSE.
    #' #' @return Updated ExprModVar object.
    #' sample = function(expected=F) {
    #'   sapply(self$operands(), FUN=function(o) {
    #'     o$sample(expected)        
    #'   })
    #'   return(invisible(self))
    #' },

    #' #' @description
    #' #' Evaluate the expression.
    #' #' @return Numerical value of the evaluated expression.
    #' value = function() {
    #'   rv <- eval(private$expr.value, envir=private$env)
    #'   return(rv)  
    #' },
    
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
    #' Draw a random sample from the model variable. 
    #' @param n Number of samples to draw.
    #' @return A sample drawn at random.
    r = function(n=1) {
      # build expression with each variable replaced with $r(n)
      mv <- list()
      method <- "r(n)"
      # substitute model variable names with call to r(n) method
      for (v in all.vars(private$expr)) {
        # v is a string containing a variable name
        if (inherits(eval(str2lang(v), envir=private$env), what='ModVar')) {
          rep <- gsub(v, paste(v, method, sep='$'), v)
        }
        else {
          rep <- v
        }
        mv[[v]] <- str2lang(rep)
      }
      emod <- eval(substitute(substitute(e, env=mv), env=list(e=private$expr)))
      # evaluate the expression
      assign("n", n, envir=private$env)
      S <- eval(emod, envir=private$env)
      rm("n", envir=private$env)
      # return the sample
      return(S)
    },

    #' @description 
    #' Return the estimated expected value of the expression variable. 
    #' @return Expected value as a numeric value.
    mean = function() {
      # sample values from the variable
      S <- self$r(private$nest)
      return(mean(S))
    },
    
    #' @description 
    #' Return the estimated standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value.
    SD = function() {
      # sample values from the variable
      S <- self$r(private$nest)
      return(sd(S))
    },

    #' @description
    #' Return the estimated quantiles by sampling the variable.
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
      S <- self$r(private$nest)
      # return the quantiles of the sample
      q <- quantile(S, probs)
      return(q)
    },
    
    #' @description
    #' Sets the value of the ModelVariable that will be returned by subsequent
    #' calls to get() until set() is called again. 
    #' @param expected Logical; TRUE to set the value to the mean of the model
    #' variable.
    #' @return Updated ExprModVar.
    set = function(expected=FALSE) {
      # check argument
      if (!is.logical(expected)) {
        rlang::abort("Argument expected must be a logical", 
                     class="expected_not_logical")

      }
      private$val <- ifelse(expected, self$mean(), self$r())
      return(invisible(self))
    },
    
    #' @description
    #' Gets the value of the ExprModVar that was set by the most recent call
    #' to set().
    #' @return Value determined by last set().
    get = function() {
      return(private$val)
    }
    
    #' #' @description 
    #' #' Return a list of operands that are themselves ModVars given
    #' #' in the expression.
    #' #' @return A list of model variables.
    #' operands = function() {
    #'   # filter the expression variables that are ModVars
    #'   mvlist <- list()
    #'   sapply(all.vars(private$expr), FUN=function(v) {
    #'     vv <- eval(str2lang(v), envir=private$env)
    #'     if (inherits(vv, what="ModVar")) {
    #'       # add the variable to the list
    #'       mvlist <<- c(mvlist, vv)
    #'       # and add its operands, if any
    #'       sapply(vv$operands(), FUN=function(o) {
    #'         mvlist <<- c(mvlist, o)
    #'       })
    #'     }
    #'   })
    #'   return(mvlist)
    #' }

  )
)
