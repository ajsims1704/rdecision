#' @title 
#' Node
#' 
#' @description
#' An R6 class to represent a node in a decision tree
#' 
#' @details 
#' Base class to represent a single node in a decision tree. Objects of base
#' class Node are not expected to be created as model objects. 
#'
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Node <- R6::R6Class(
  classname = "Node",
  private = list(
    .label = ""
  ),
  
  public = list(
    
    #' @description
    #' Create new Node object.
    #' @param label An optional label for the node.
    #' @return A new Node object.
    initialize = function(label="") {
      if (!is.character(label)) {
        rlang::abort("Argument label is not a string", class="non-string_label")
      }
      private$.label <- label
      return(invisible(self))
    },
    
    #' @description 
    #' Return the label of the node.
    #' @return Label as a character string.
    label = function() {
      return(private$.label)
    },

    #' @description
    #' node type
    #' @return 
    #' Node class, as character string
    type = function() {
      c <- class(self)[1]
      return(c)
    }

  )
)
