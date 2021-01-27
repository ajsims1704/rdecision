#' @title 
#' ExprModVar
#' 
#' @description 
#' An R6 class for a model variable constructed from an expression
#' involving other model variables.
#' 
#' @details A class to support expressions involving objects
#' of base class ModVar, which itself behaves like a 
#' model variable. For example, if \code{A} and
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
#' the quantiles. Therefore they are obtained by simulation, via
#' functions \code{mu_hat}, \code{sigma_hat} and \code{q_hat}.
#' 
#' @note For consistency with \code{ModVar}s which are not expressions, the
#' function \code{mean} returns the value of the expression when all
#' its operands take their mean values. This will, in general, not
#' be the mean of the expression distribution (which can be obtained
#' via \code{mu_hat}), but is the value normally used in the base
#' case of a model as the point estimate. As Briggs \emph{et al} note 
#' (section 4.1.1) "in all but the most nonlinear models, the 
#' difference between the expectation over the output of a 
#' probabilistic model and that model evaluated at the mean values
#' of the input parameters, is likely to be modest."
#' 
#' @note Functions \code{SD}, \code{mode} and \code{quantile} return NA
#' because they do not necessarily have a closed form. The standard
#' deviation can be estimated by calling \code{sigma_hat} and the
#' quantiles by \code{q_hat}. Because a unimodal distribution is not
#' guaranteed, there is no estimator provided for the mode.
#' 
#' @note Method \code{distribution} returns the string representation 
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
    env = NULL

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
    #' Return the value of the expression when its operands take their
    #' mean value (i.e. value returned by call to \code{mean} or their
    #' value, if numeric). See notes on this class for further explanation.
    #' @return Mean value as a numeric value.
    mean = function() {
      # build expression with each variable replaced with $mean()
      mv <- list()
      method <- "mean()"
      # substitute model variable names with call to mean() method
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
      rv <- eval(emod, envir=private$env)
      return(rv)
    },
    
    #' @description 
    #' Return the mode of the variable. By default returns NA, which will be
    #' the case for most ExprModVar variables, because an arbitrary expression
    #' is not guaranteed to be unimodel.
    #' @return Mode as a numeric value.
    mode = function() {
      return(as.numeric(NA))
    },

    #' @description 
    #' Return the standard deviation of the distribution as NA because the
    #' variance is not available as a closed form for all functions of
    #' distributions. 
    #' @return Standard deviation as a numeric value
    SD = function() {
      return(as.numeric(NA))
    },
    
    #' @description 
    #' Find quantiles of the uncertainty distribution.  Not available
    #' as a closed form, and returned as NA.
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
      return(rep(as.numeric(NA),length(probs)))
    },

    #' @description 
    #' Return the estimated expected value of the expression variable. This is
    #' computed by numerical simulation because there is, in general, no closed
    #' form expressions for the mean of a function of distributions.
    #' @param nest Sample size to be used to estimate the mean. Values less than
    #' 1000 (default) are unlikely to return meaningful estimates and will 
    #' be rejected.
    #' @return Expected value as a numeric value.
    mu_hat = function(nest=1000) {
      # check and process the sample estimation size
      if (!is.numeric(nest)) {
        rlang::abort("Argument nest must be numeric", class="nest_not_numeric")
      }
      if (nest < 1000) {
        rlang::abort("Argument nest must not be less than 1000", class="nest_too_small")
      }
      # sample values from the variable
      S <- self$r(nest)
      return(mean(S))
    },
    
    #' @description 
    #' Return the estimated standard deviation of the distribution. This is
    #' computed by numerical simulation because there is, in general, no closed
    #' form expressions for the SD of a function of distributions.
    #' @param nest Sample size to be used to estimate the SD. Values less than
    #' 1000 (default) are unlikely to return meaningful estimates and will 
    #' be rejected.
    #' @return Standard deviation as a numeric value.
    sigma_hat = function(nest=1000) {
      # check and process the sample estimation size
      if (!is.numeric(nest)) {
        rlang::abort("Argument nest must be numeric", class="nest_not_numeric")
      }
      if (nest < 1000) {
        rlang::abort("Argument nest must not be less than 1000", class="nest_too_small")
      }
      # sample values from the variable
      S <- self$r(nest)
      return(sd(S))
    },

    #' @description
    #' Return the estimated quantiles by sampling the variable. This is
    #' computed by numerical simulation because there is, in general, no closed
    #' form expressions for the quantiles of a function of distributions.
    #' @param probs Vector of probabilities, in range [0,1].    
    #' @param nest Sample size to be used to estimate the SD. Values less than
    #' 1000 (default) are unlikely to return meaningful estimates and will 
    #' be rejected.
    #' @return Vector of quantiles.
    q_hat = function(probs, nest=1000) {
      # check probs
      sapply(probs, FUN=function(x) {
        if (!is.numeric(probs)) {
          stop("ExprModVar$getQuantile: argument must be a numeric vector")
        }
      })
      # check and process the sample estimation size
      if (!is.numeric(nest)) {
        rlang::abort("Argument nest must be numeric", class="nest_not_numeric")
      }
      if (nest < 1000) {
        rlang::abort("Argument nest must not be less than 1000", class="nest_too_small")
      }
      # sample the distribution
      S <- self$r(nest)
      # return the quantiles of the sample
      q <- quantile(S, probs)
      return(q)
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
