#' @title A model variable whose uncertainty follows a Gamma distribution
#' 
#' @description An R6 class for a model variable with Gamma uncertainty.
#' 
#' @details A model variable for which the uncertainty in the point estimate can
#' be modelled with a Gamma distribution. The hyperparameters of the
#' distribution are the shape (\code{k}) and the scale (\code{theta}). Note
#' that although Briggs \emph{et al} (2006) use the shape, scale formulation,
#' they use \code{alpha}, \code{beta} as parameter names. Inherits from
#' class \code{ModVar}.
#'  
#' @references{
#'   Briggs A, Claxton K, Sculpher M. Decision modelling for health
#'   economic evaluation. Oxford, UK: Oxford University Press; 2006. 
#' }
#' 
#' @note The Gamma model variable class can be used to model the uncertainty of
#' the mean of a count quantity which follows a Poisson distribution. The Gamma
#' distribution is the conjugate prior of a Poisson distribution, and the shape
#' and scale relate directly to the number of intervals from which the mean
#' count has been estimated. Specifically, the shape (\eqn{k}) is equal to the 
#' total count of events in \eqn{1/\theta} intervals, where \eqn{\theta} is the
#' scale. For example, if 200 counts were observed in a sample of 100 intervals, 
#' setting \code{shape=200} and \code{scale=1/100} gives a Gamma distribution 
#' with a mean of 2 and a 95\% confidence interval from 1.73 to 2.29. 
#'
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
GammaModVar <- R6::R6Class(
  classname = "GammaModVar",
  lock_class = TRUE,
  inherit = ModVar,
  private = list(
  ),
  public = list(
    
    #' @description 
    #' Create an object of class \code{GammaModVar}.
    #' @param description A character string describing the variable.
    #' @param units Units of the variable, as character string.
    #' @param shape shape parameter of the Gamma distribution.
    #' @param scale scale parameter of the Gamma distribution.
    #' @return An object of class \code{GammaModVar}. 
    initialize = function(description, units, shape, scale) {
      # create a Gamma distribution (and check arguments)
      D <- GammaDistribution$new(shape, scale)
      # create the base class model variable
      super$initialize(description, units, D=D, k=as.integer(1))
      # return object
      return(invisible(self))
    },

    #' @description 
    #' Tests whether the model variable is probabilistic, i.e. a random
    #' variable that follows a distribution, or an expression involving
    #' random variables, some of which follow distributions. 
    #' @return \code{TRUE} if probabilistic
    is_probabilistic = function() {
      return(TRUE)
    }

  )
)
