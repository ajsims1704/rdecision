#' @title
#' LeafNode
#' 
#' @description 
#' An R6 class for a leaf node in a decision tree
#'
#' @details A LeafNode is synomymous with a clinical outcome, or end
#' point of a decision tree. Paths through the tree must end on leaf
#' nodes.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims5@@newcastle.ac.uk}
#' 
#' @export
#' 
LeafNode <- R6::R6Class(
  classname = "LeafNode",
  inherit = Node,
  private = list(
    name = "character",
    utility = "numeric"
  ),
  public = list(
    
    #' @description
    #' Create a new `LeafNode` object; synonymous with a clinical outcome.
    #' @param name Character string; a label for the leaf node which is
    #'        synonymous with the name of the root-to-leaf pathway
    #' @param utility the preference or value that a user associates with
    #'        a given health state (range 0 to 1).
    #' @return A new `LeafNode` object
    initialize = function(name, utility=1) {
      super$initialize()
      private$name <- name
      if (!is.numeric(utility)) {
        stop("LeafNode$new: utility must be a numeric value")
      }
      if (utility > 1) {
        stop("LeafNode$new: utility must be in the range [-Inf,1]")
      }
      private$utility <- utility
    },
    
    #' @description 
    #' Return the label of the leaf node; the name of the clinical outcome.
    #' @return Name of the clinical outcome; character string.
    getName = function() {
      return(private$name)
    },
    
    #' @description 
    #' Return the utility associated with the clinical outcome.
    #' @return Utility (numeric value)
    getUtility = function() {
      return(private$utility)
    }
  )
 )
