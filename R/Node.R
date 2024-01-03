#' @title A node in a graph
#' @description An R6 class representing a node in a graph.
#' @details A base class to represent a single node in a graph.
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
Node <- R6::R6Class(
  classname = "Node",
  lock_class = TRUE,
  private = list(
    .label = NULL
  ),
  public = list(

    #' @description Create new \code{Node} object.
    #' @param label An optional label for the node.
    #' @return A new \code{Node} object.
    initialize = function(label = "") {
      abortifnot(is.character(label),
        message = "Argument label is not a string",
        class = "non-string_label"
      )
      private$.label <- label
      return(invisible(self))
    },

    #' @description Return the label of the node.
    #' @return Label as a character string.
    label = function() {
      return(private$.label)
    },

    #' @description node type
    #' @return \code{Node} class, as character string.
    type = function() {
      c <- class(self)[[1L]]
      return(c)
    }
  )
)
