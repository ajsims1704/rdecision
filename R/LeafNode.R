#' @title
#' LeafNode
#' 
#' @description 
#' An R6 class for a leaf node in a decision tree representing a clinical state.
#'
#' @details It represents a state of being, and is associated with an annual cost,
#' an incremental utility and a benefit. It inherits from class Node so that it 
#' can be part of a decision tree as a leaf node with no children. 
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' 
#' @export
#' 
LeafNode <- R6::R6Class(
  classname = "LeafNode",
  inherit = Node,
  private = list(
#    node.cost = 0,
    node.utility = 1,
#    node.benefit = 0,
    node.interval = as.difftime(tim=365.25, units="days")
  ),
  public = list(
    
    #' @description
    #' Create a new \code{LeafNode} object; synonymous with a clinical outcome.
    #' @param name Character string; a label for the state.
    #' @param utility The incremental utility that a user associates with
    #' being in the health state (range -Inf to 1) for the interval. Intended
    #' for use with cost benefit analysis.
    #' @param interval The time interval, over which the \code{utility} parameters
    #' apply, expressed as an R \code{difftime} object; default 1 year.
    #' @return A new \code{LeafNode} object
    initialize = function(name, utility=1,
                          interval=as.difftime(365.25, units="days")) {
      super$initialize(name)
      # check and set utility
      if (!is.numeric(utility)) {
        rlang::abort("Argument 'utility' must be a numeric value.")
      }
      if (utility > 1) {
        rlang.abort("Argument 'utility' must be in the range [-Inf,1].")
      }
      private$node.utility <- utility
      # check and set the interval
      if (class(interval) != 'difftime') {
        rlang::abort("Argument 'interval' must be of class 'difftime'.")
      }
      private$node.interval <- interval
    },
    
    #' @description 
    #' Return the incremental utility associated with being in the state for
    #' the interval.
    #' @param expected Parameter passed to the \code{value} method of the model
    #' variable used to define utility; ignored otherwise.
    #' @return Incremental utility (numeric value).
    utility = function(expected) {
      rv <- private$node.utility
      if (inherits(rv, what="ModVar")) {
        rv <- rv$value(expected)
      } 
      return(rv)
    }
    
  )
 )
