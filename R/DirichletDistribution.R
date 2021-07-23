#' @title \verb{DirichletDistribution} class
#' 
#' @description An R6 class for a multivariate Dirichlet distribution.
#' 
#' @details A multivariate Dirichlet distribution. Modelled as a series of
#' conditional beta distributions. 
#'
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
DirichletDistribution <- R6::R6Class(
  classname = "DirichletDistribution",
  lock_class = TRUE,
  inherit = Distribution,
  private = list(
    alpha = NULL
  ),
  public = list(
    
    #' @description 
    #' Create an object of class \code{DirichletDistribution}.
    #' @param K Order (integer \eqn{\ge 2}).
    #' @param alpha Parameters of the distribution; a vector of \code{K} numeric
    #' values each > 0.
    #' @return An object of class \code{DirichletDistribution}. 
    initialize = function(K, alpha) {
      # create subclass object and check parameter
      super$initialize("Dir", K)
      if (K < 2) {
        rlang::abort("'K' must be greater than 1", class = "K_unsupported")
      }
      # check alpha parameter
      sapply(alpha, FUN=function(x) {
        if (is.na(x)) {
          rlang::abort("All elements of 'alpha' must be defined",
                       class="alpha_not_defined")
        }
        if (!is.numeric(x)) {
          rlang::abort("Argument 'alpha' must be a numeric vector",
                       class="alpha_not_numeric")
        }
        if (x<=0) {
          rlang::abort("Elements of 'alpha' must be > 0",
                       class="alpha_unsupported")
        }
      })
      private$alpha <- alpha
      # check that the order and number of parameters are equal
      if (K != length(alpha)) {
        rlang::abort(
          "'alpha' must have K elements", 
          class="inconsistent_K_alpha"
        )
      }
      # return Dirichlet distribution object
      return(invisible(self))
    },
    
    #' @description Accessor function for the name of the distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- paste('Dir(', paste(private$alpha, collapse=','), ')', sep='')
      return(rv)
    }
    
  )
)
    