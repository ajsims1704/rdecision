#' @title A model variable whose uncertainty follows a log Normal distribution
#' @description An R6 class representing a model variable with log Normal 
#' uncertainty.
#' @details A model variable for which the uncertainty in the point estimate can
#' be modelled with a log Normal distribution. One of seven parametrizations
#' defined by Swat \emph{et al} can be used. Inherits from \code{ModVar}.
#' @references{ 
#'  Briggs A, Claxton K and Sculpher M. Decision Modelling for Health
#'  Economic Evaluation. Oxford 2006, ISBN 978-0-19-852662-9.
#'
#'  Leaper DJ, Edmiston CE and Holy CE. Meta-analysis of the potential
#'  economic impact following introduction of absorbable antimicrobial 
#'  sutures. \emph{British Journal of Surgery} 2017;\bold{104}:e134-e144.
#'
#'  Swat MJ, Grenon P and Wimalaratne S. Ontology and Knowledge Base of
#'  Probability Distributions. \acronym{EMBL-EBI} Technical Report
#'  (ProbOnto 2.5), 13 January 2017, 
#'  \url{https://sites.google.com/site/probonto/download}.
#' }
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
LogNormModVar <- R6::R6Class(
  classname = "LogNormModVar",
  lock_class = TRUE,
  inherit = ModVar,
  private = list(
  ),
  public = list(
    
    #' @description Create a model variable with log normal uncertainty.
    #' @seealso \code{\link{LogNormDistribution}}.
    #' @param description A character string describing the variable.
    #' @param units Units of the quantity; character string.
    #' @param p1 First hyperparameter, a measure of location. 
    #' See \emph{Details}.
    #' @param p2 Second hyperparameter, a measure of spread.
    #' See \emph{Details}.
    #' @param parametrization A character string taking one of the values
    #' \verb{"LN1"} (default) through \verb{"LN7"} (see \emph{Details}).
    #' @return A \code{LogNormModVar} object.
    initialize = function(description, units, p1, p2, parametrization = "LN1") {
      # create a log normal distribution and check parameters
      D <- LogNormDistribution$new(p1=p1, p2=p2, parametrization)
      # initialize the base class
      super$initialize(description, units, D = D, k = 1L)
      # return new object
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
