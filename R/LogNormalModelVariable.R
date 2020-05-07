#' @title 
#' LogNormalModelVariable
#' 
#' @description 
#' An R6 class for a model variable with logNormal uncertainty
#' 
#' @details 
#' A model variable for which the uncertainty in the point estimate can
#' be modelled with a logNormal distribution. 
#' \href{https://sites.google.com/site/probonto/}{ProbOnto}
#' defines seven parametrizations of the log normal distribution. These are linked,
#' allowing the parameters of any one to be derived from any other. All
#' 7 parameterizations require two parameters; their meanings are as follows:
#' \describe{
#' \item{LN1}{\eqn{p_1=\mu}, \eqn{p_2=\sigma}, where \eqn{\mu} and \eqn{\sigma} are the mean
#' and standard deviation, both on the log scale.}
#' \item{LN2}{\eqn{p_1=\mu}, \eqn{p_2=v}, where \eqn{\mu} and \eqn{v} are the mean
#' and variance, both on the log scale.}
#' \item{LN3}{\eqn{p_1=m}, \eqn{p_2=\sigma}, where \eqn{m} is the median on the natural
#' scale and \eqn{\sigma} is the standard deviation on the log scale.}
#' \item{LN4}{\eqn{p_1=m}, \eqn{p_2=c_v}, where \eqn{m} is the median on the natural
#' scale and \eqn{c_v} is the coefficient of variation on the natural scale.}
#' \item{LN5}{\eqn{p_1=\mu}, \eqn{p_2=\tau}, where \eqn{\mu} is the mean on the log
#' scale and \eqn{\tau} is the precision on the log scale.}
#' \item{LN6}{\eqn{p_1=m}, \eqn{p_2=\sigma_g}, where \eqn{m} is the median on
#' the natural scale and \eqn{\sigma_g} is the geometric standard deviation on the
#' natural scale.}
#' \item{LN7}{\eqn{p_1=\mu_N}, \eqn{p1=\sigma_N}, where \eqn{\mu_N} is the mean on
#' the natural scale and \eqn{\sigma_N} is the standard deviation on the
#' natural scale.}
#' }
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims5@@nhs.net}
#' @export
#' 
LogNormalModelVariable <- R6::R6Class(
  classname = "LogNormalModelVariable",
  inherit = ModelVariable,
  private = list(
    meanlog = 'numeric',
    sdlog = 'numeric',
    parametrization = 'character'
  ),
  public = list(
    
    #' @description
    #' Create a model variable with log normal uncertainty. 
    #' @param label A character string label for the variable. It is advised
    #' to make this the same as the variable name which helps when tabulating
    #' model variables involving ExpressionModelVariables.
    #' @param description A character string describing the variable.
    #' @param units Units of the quantity; character string.
    #' @param p1 First hyperparameter, a measure of location. 
    #'        See \link{Details}.
    #' @param p2 Second hyperparameter, a measure of spread.
    #'        See \link{Details}.
    #' @param parametrization A character string taking one of the values
    #'        'LN1' (default) through 'LN7' (see \link{Details}).
    #' @return A LogNormalModelVariable object.
    initialize = function(label, description, units, p1, p2, parametrization='LN1') {
      super$initialize(label, description, units)
      # transform parameters according to parametrization
      private$parametrization <- parametrization
      if (parametrization == 'LN1') {
        private$meanlog <- p1
        private$sdlog <- p2
      }
      else if (parametrization == 'LN2') {
        private$meanlog <- p1
        private$sdlog <- sqrt(p2)
      }
      else if (parametrization == 'LN3') {
        private$meanlog <- log(p1)
        private$sdlog <- p2
      }
      else if (parametrization == 'LN4') {
        private$meanlog <- log(p1)
        private$sdlog <- sqrt(log(p2^2+1))
      }
      else if (parametrization == 'LN5') {
        private$meanlog <- p1
        private$sdlog <- 1/sqrt(p2)
      }
      else if (parametrization == 'LN6') {
        private$meanlog <- log(p1) 
        private$sdlog <- log(p2)
      }
      else if (parametrization == 'LN7') {
        private$meanlog <- log(p1/sqrt(1+(p2^2)/(p1^2))) 
        private$sdlog <- sqrt(log(1+(p2^2)/(p1^2)))
      }
      else {
        stop("LogNormalModelVariable::new: parameter 'parametrize must be one of 'LN1' through 'LN7'.")
      }

      # set the next value returned to be the mean
      private$val <- self$getMean()
    },
    
    #' @description
    #' Set the value of the model variable from its uncertainty distribution.
    #' Nothing is returned; the sampled value is returned at the next
    #' call to `value()`.
    #' @param expected Logical; if TRUE sets the value of the model variable
    #'        returned at subsequent calls to `value()` to be equal to the 
    #'        expectation of the variable. Default is FALSE.
    #' @return Updated NormalModelVariable object.
    sample = function(expected=F) {
      if (expected) {
        private$val <- self$getMean()
      }
      else {
        private$val <- rlnorm(1, mean=private$meanlog, sd=private$sdlog)
      }
      invisible(self)
    },
    
    #' @description 
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string (LN1, LN2 etc).
    getDistribution = function() {
      rv <- paste(private$parametrization, '(', round(private$meanlog,3), ',', 
                  round(private$sdlog,3), ')', sep='')
      return(rv)
    },
    
    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    getMean = function() {
      E <- exp(private$meanlog + 0.5*private$sdlog^2)
      return(E)
    },
    
    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    getSD = function() {
      S <- exp(private$meanlog + 0.5*private$sdlog^2) *
           sqrt(exp(private$sdlog^2) - 1)
      return(S)
    },

    #' @description
    #' Return the quantiles of the logNormal uncertainty distribution.
    #' @param probs Vector of probabilities, in range [0,1].    
    #' @return Vector of quantiles.
    getQuantile = function(probs) {
      sapply(probs, FUN=function(x) {
        if (!is.numeric(probs)) {
          stop("LogNormalModelVariable$getQuantile: argument must be a numeric vector")
        }
      })
      q <- qlnorm(probs, mean=private$meanlog, sd=private$sdlog)
      return(q)
    }

  )
)
