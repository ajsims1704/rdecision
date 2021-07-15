#' @title \verb{DirichletDistribution} class
#' 
#' @description
#' An R6 class for a multivariate Dirichlet distribition.
#' 
#' @details A multivariate Dirichlet distribution. Modelled as a series of
#' conditional beta distributions. 
#'
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
DirichletDistribition <- R6::R6Class(
  classname = "DirichletDistribution",
  lock_class = TRUE,
  inherit = Distribution,
  private = list(
    alpha = NULL
  ),
  public = list(
    
    #' @description 
    #' Create an object of class \code{DirichletDistribution}.
    #' @param k Order (an integer).
    #' @param alpha Parameters of the distribution; a vector of numeric values
    #' each > 0.
    #' @return An object of class \code{DirichletDistribution}. 
    initialize = function(k, alpha) {
      super$initialize("Dir", k)
      # check alpha parameter
      if (!is.numeric(alpha)) {
        rlang::abort(
          "Argument 'alpha' must be numeric", 
          class="alpha_not_numeric"
        )
      }
      if (alpha <= 0) {
        rlang::abort(
          "Argument 'alpha' must be > 0", 
          class="alpha_not_supported"
        )
      }
      private$alpha <- alpha
      # check beta parameter
      if (!is.numeric(beta)) {
        rlang::abort(
          "Argument 'beta must be numeric", 
          class="beta_not_numeric"
        )
      }
      if (beta <= 0) {
        rlang::abort(
          "Argument 'beta' must be > 0", 
          class="beta_not_supported"
        )
      }
      private$beta <- beta
      # ensure first call to get() is valid
      self$set("expected")
      # return BetaModVar
      return(invisible(self))
    }
  )
)
    