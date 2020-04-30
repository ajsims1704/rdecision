#' @title
#' LeafNode
#' 
#' @description 
#' An R6 class for a leaf node in a decision tree
#'
#' @details A LefNode is synomymous with a clinical outcome, or end
#' point of a decision tree. Paths through the tree must end on leaf
#' nodes.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims5@nhs.net}
#' 
#' @export
#' 
LeafNode <- R6::R6Class(
  classname = "LeafNode",
  inherit = Node,
  private = list(
    pathway = "character",
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
      private$pathway <- name
      if (!is.numeric(utility)) {
        stop("LeafNode$new: utility must be a numeric value")
      }
      if ((utility < 0) | (utility < 1)) {
        stop("LeafNode$new: utility must be in the range [0,1]")
      }
      private$utility <- utility
    },
    
    #' @description 
    #' Return the label of the leaf node; the name of the clinical outcome.
    #' @return Name of the clinical outcome; character string.
    getPathway = function() {
      return(private$pathway)
    },
    
    #' @description 
    #' Return the utility associated with the clinical outcome.
    #' @return Utility (numeric value)
    getUtility = function() {
      return(private$utility)
    }
  )
 )
