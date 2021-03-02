#' @title \verb{Edge} class
#' 
#' @description
#' An R6 class to represent an edge in a graph.
#' 
#' @details Edges are the formal term for links between pairs of nodes in a
#' graph. 
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'  
Edge <- R6::R6Class(
  classname = "Edge",
  private = list(
    v1 = NULL,
    v2 = NULL,
    edgelabel = ""
  ),
  public = list(
    
    #' @description
    #' Create an object of type \verb{Edge}.
    #' @param v1 Node at one endpoint of the edge.
    #' @param v2 Node at the other endpoint of the edge.
    #' @param label Character string containing the edge label.
    #' @return A new \verb{Edge} object.
    initialize = function(v1, v2, label="") {
      # check and set v1
      if (!inherits(v1, what="Node")) {
        rlang::abort("'v1' must be a 'Node'", class="non-Node_endpoint")
      }
      else {
        private$v1 <- v1
      }
      # check and set v2
      if (!inherits(v2, what="Node")) {
        rlang::abort("'v2' must be a 'Node'", class="non-Node_endpoint")
      }
      else {
        private$v2 <- v2
      }
      # check and set label
      if (!is.character(label)) {
        rlang::abort("'label' must be a string", class="non-string_label")
      }
      else {
        private$edgelabel <- label
      }
    },
    
    #' @description
    #' Is this edge the same as the argument? (DOM-style)
    #' @param e edge to compare with this one
    #' @return TRUE if `e` is also this one.
    is_same_edge = function(e) {
      return(identical(self,e))
    },
    
    #' @description
    #' Retrieve the endpoints of the edge.
    #' @return List of two nodes to which the edge is connected.
    endpoints = function() {
      return(c(private$v1, private$v2))
    },
    
    #' @description
    #' Access label.
    #' @return Label of the edge; character string.
    label = function() {
      return(private$edgelabel)
    }
  )
)
