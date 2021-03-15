#' @title \verb{DigraphLayout} class
#' 
#' @description
#' An R6 class to represent a layout of a digraph.
#' 
#' @details 
#' Provides methods to create a layout of a directed graph. Uses the \code{dot}
#' algorithm of Gansner (1993).
#'
#' @references{ 
#'   Gansner ER, Koutsofios E, North SC, Vo K-P. A technique for drawing 
#'   directed graphs. \emph{IEEE Transactions on Software Engineering}
#'   1993;\bold{19}:214–30, \doi{10.1109/32.221135}.
#' }
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
DigraphLayout <- R6::R6Class(
  classname = "DigraphLayout",
  lock_class = TRUE,
  inherit = Graph,
  private = list(
    # class private variables
    G = NULL,
    
    # network simplex algorithm
    rank = function() {
      
    }
    
  ),
  
  public = list(
    #' @description Creates a new layout object from a \code{Digraph}.
    #' @param G an object of class \code{Digraph}.
    #' @return A \code{DigraphLayout} object.
    initialize = function(G) {
      # check argument
      if (!inherits(G, what="Digraph")) {
        rlang::abort(
          "Argument is not of class 'Digraph'",
          class = "invalid_G"
        )
      }
      # save reference to G
      private$G <- G
      # return layout object
      return(invisible(self))
    }
    
  )

)
    