#' @title An edge in a graph
#' @description An R6 class representing an edge in a graph.
#' @details Edges are the formal term for links between pairs of nodes in a
#' graph. A base class.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
Edge <- R6::R6Class(
  classname = "Edge",
  lock_class = TRUE,
  private = list(
    v1 = NULL,
    v2 = NULL,
    edgelabel = NULL
  ),
  public = list(
    
    #' @description Create an object of type \code{Edge}.
    #' @param v1 Node at one endpoint of the edge.
    #' @param v2 Node at the other endpoint of the edge.
    #' @param label Character string containing the edge label.
    #' @return A new \code{Edge} object.
    initialize = function(v1, v2, label = "") {
      # check and set v1
      abortifnot(inherits(v1, what = "Node"),
        message = "'v1' must be a 'Node'", 
        class = "non-Node_endpoint"
      )
      private$v1 <- v1
      # check and set v2
      abortifnot(inherits(v2, what = "Node"),
        message = "'v2' must be a 'Node'", 
        class = "non-Node_endpoint"
      )
      private$v2 <- v2
      # check and set label
      abortifnot(is.character(label),
        message = "'label' must be a string", 
        class = "non-string_label"
      )
      private$edgelabel <- label
      # return Edge object
      return(invisible(self))
    },
    
    #' @description Is this edge the same as the argument? 
    #' @param e edge to compare with this one
    #' @return \code{TRUE} if \code{e} is also this one.
    is_same_edge = function(e) {
      return(identical(self, e))
    },
    
    #' @description Retrieve the endpoints of the edge.
    #' @return List of two nodes to which the edge is connected.
    endpoints = function() {
      return(c(private$v1, private$v2))
    },
    
    #' @description Access label.
    #' @return Label of the edge; character string.
    label = function() {
      return(private$edgelabel)
    }
  )
)
