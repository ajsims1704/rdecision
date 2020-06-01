#' @title 
#' Edge
#' 
#' @description
#' An R6 class to represent an edge in a decision tree
#' 
#' @details Edges are the formal term for paths linking nodes in a
#' hierarchical tree. It is not intended that package users creating models
#' should instantiate the `Edge` class. Instead, it is included in the
#' package as a convenience class used in the construction and traversal
#' of decision trees by the package methods themselves, and for examining
#' and printing model structure.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
Edge <- R6::R6Class(
  classname = "Edge",
  private = list(
    fromNode = NULL,
    toNode = NULL,
    label = ""
  ),
  public = list(
    
    #' @description
    #' Create an object of type `Edge`.
    #' @param fromNode Node nearest the root to which the edge connects.
    #' @param toNode Node nearest the leaf to which the edge connects.
    #' @param label Character string containing the edge label.
    #' @return A new `Edge` object.
    initialize = function(fromNode, toNode, label) {
      
      # check and set fromNode
      if (!inherits(fromNode, what="Node")) {
        stop("Edge$new: `fromNode` must inherit from type `Node`")
      }
      else {
        private$fromNode <- fromNode
      }
      
      # check and set toNode
      if (!inherits(toNode, what="Node")) {
        stop(paste("Edge$new: `toNode` must inherit from type `Node` for ", label))
      }
      else {
        private$toNode <- toNode
      }
      
      # check and set label
      if (!is.character(label)) {
        stop("Edge$new: `label` must be of type `character` not ", class(label))
      }
      else {
        private$label <- label
      }
    },
    
    #' @description
    #' Access toNode.
    #' @return `Node` to which the edge leads.
    getToNode = function() {
      return(private$toNode)
    },
    
    #' @description
    #' Access label.
    #' @return Label of the edge; character string.
    getLabel = function() {
      return(private$label)
    }

  )
)
