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
    node.cost = 0,
    node.utility = 1,
    node.benefit = 0,
    interval = as.difftime(tim=365.25, units="days")
  ),
  public = list(
    
    #' @description
    #' Create a new \code{LeafNode} object; synonymous with a clinical outcome.
    #' @param name Character string; a label for the state.
    #' @param cost The cost of being in the state over the interval.
    #' @param utility The incremental utility that a user associates with
    #' being in the health state (range -Inf to 1) for the interval. Intended
    #' for use with cost benefit analysis.
    #' @param benefit The financial benefit accruing from reaching the node.
    #' Intended for use with cost-benefit analysis. Normally only one of
    #' utility or benefit are set.
    #' @param interval The time interval, over which the \code{cost} and
    #' \code{utility} parameters apply, expressed as an R \code{difftime} object;
    #' default 1 year.
    #' @return A new \code{LeafNode} object
    initialize = function(name, cost=0, utility=1, benefit=0,
                          interval=as.difftime(365.25, units="days")) {
      super$initialize(name)
      # check and set cost
      if (!is.numeric(cost) && !inherits(cost, what="ModVar")) {
        rlang::abort("Argument 'cost' must be of type 'numeric' or 'ModVar'")
      }
      private$cost <- cost    
      # check and set utility
      if (!is.numeric(utility)) {
        rlang::abort("Argument 'utility' must be a numeric value.")
      }
      if (utility > 1) {
        rlang.abort("Argument 'utility' must be in the range [-Inf,1].")
      }
      private$utility <- utility
      # check and set benefit
      if (!is.numeric(benefit) && !inherits(benefit, what="ModVar")) {
        rlang::abort("Argument 'benefit' must be of type 'numeric' or 'ModVar'")
      }
      private$benefit <- benefit    
      # check and set the interval
      if (class(interval) != 'difftime') {
        rlang::abort("Argument 'interval' must be of class 'difftime'.")
      }
      private$interval <- interval
    },
    
    #' #' @description 
    #' #' Return the label of the state; the name of the clinical outcome.
    #' #' @return Name of the clinical outcome or state; character string.
    #' get_name = function() {
    #'   return(private$name)
    #' },
    
    #' @description 
    #' Return the cost incurred by being in the state for the interval.
    #' @return Cost, as a numeric value.
    cost = function() {
      rv <- private$node.cost
      if (inherits(rv, what="ModVar")) {
        rv <- rv$value()
      } 
      return(rv)
    },
    
    #' @description 
    #' Return the incremental utility associated with being in the state for
    #' the interval.
    #' @return Incremental utility (numeric value).
    utility = function() {
      return(private$node.utility)
    },
    
    #' @description 
    #' Return the benefit (payoff) associated with being in the state for
    #' the interval.
    #' @return Benefit (numeric value).
    benefit = function() {
      return(private$node.benefit)
    }
  )
 )
