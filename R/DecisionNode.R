#' @title 
#' DecisionNode
#' 
#' @description 
#' An R6 class for a decision node in a decision tree
#' 
#' @details 
#' A class to represent a decision node in a decision tree. The node
#' is associated with one or more branches to child nodes.  
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
DecisionNode <- R6::R6Class(
  classname = "DecisionNode",
  inherit = Node,
  private = list(
  ),
  public = list(

    #' @description 
    #' Create a new decision node.
    #' @param children A list of Nodes which are the children of this
    #' decision node.
    #' @param choices A list of character stings containing the labels
    #' of each choice associated with the decision.
    #' @return A new DecisionNode object
    initialize = function(children, choices) {
      # ensure base class fields are initialized
      super$initialize()
      # check child nodes
      if (length(children) == 0) {
        stop("`children` must contain at least one object of class `Node`.")
      }
      sapply(children, function(x) {
        if (inherits(x, what="Node")==FALSE){
          stop("Each element in `children` must be of class `Node`")
        }
      })
      # check choices
      if (length(choices) != length(children)) {
        stop("`choices` must contain the same number of objects as `children`.")
      }
      sapply(choices, function(x) {
        if (!is.character(x)){
          stop("Each element in 'choices' must be of class `character`.")
        }
      })
      # add edges to this node
      for (i in 1:length(children)) {
        edge <- Arrow$new(self, children[[i]], choices[[i]])
        private$addArrow(edge)
      }
      return(invisible(self))
    },
    
    #' @description 
    #' Return names of the choices associated with this node.
    #' @return A vector of character strings.
    get_choices = function() {
      choices <- sapply(private$edges, FUN=function(e){e$get_label()})
      return(choices)
    }
    
  )
)
