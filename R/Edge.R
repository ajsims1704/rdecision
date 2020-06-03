#' @title 
#' Edge
#' 
#' @description
#' An R6 class to represent an directed edge in a digraph
#' 
#' @details Edges are the formal term for links between pairs of nodes in a
#' graph. It is not intended that package users creating models should 
#' instantiate the `Edge` class. Instead, it is included in the
#' package as a convenience class used in the construction and traversal
#' of models by the package methods themselves, and for examining
#' and printing model structure.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
Edge <- R6::R6Class(
  classname = "Edge",
  private = list(
    source = NULL,
    target = NULL,
    label = ""
  ),
  public = list(
    
    #' @description
    #' Create an object of type 'Edge'.
    #' @param source Node from which the edge comes.
    #' @param target second Node to which the edge connects.
    #' @param label Character string containing the edge label.
    #' @return A new `Edge` object.
    initialize = function(source, target, label="") {
      # check and set source
      if (!inherits(source, what="Node")) {
        rlang::abort("Argument 'source' must inherit from type 'Node'",
                     class="non-Node_source")
      }
      else {
        private$source <- source
      }
      # check and set target
      if (!inherits(target, what="Node")) {
        rlang::abort("Argument 'target' must inherit from type 'Node'",
                      class="non-Node_target")
      }
      else {
        private$target <- target
      }
      # check and set label
      if (!is.character(label)) {
        rlang::abort("Argument 'label' must be of type 'character'", 
                     class="non-string_label")
      }
      else {
        private$label <- label
      }
    },

    #' @description
    #' Access source node.
    #' @return `Node` from which the edge leads.
    get_source = function() {
      return(private$source)
    },
    
    #' @description
    #' Access target node.
    #' @return `Node` to which the edge leads.
    get_target = function() {
      return(private$target)
    },
    
    #' @description
    #' Access label.
    #' @return Label of the edge; character string.
    get_label = function() {
      return(private$label)
    }

  )
)
