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
#' of decision trees by the package methods themselves. In this case
#' objects of type `Edge` are used to collect together information relating to 
#' the same edge, i.e. a label, a cost and a conditional probability.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims5@nhs.net}
#' @export
#'  
Edge <- R6::R6Class(
  classname = "Edge",
  private = list(
    fromNode = 'Node',
    toNode = 'Node',
    label = 'character',
    cost = 'numeric', 
    p = 'numeric' 
  ),
  public = list(
    
    #' @description
    #' Create an object of type `Edge`.
    #' @param fromNode Node nearest the root to which the edge connects.
    #' @param toNode Node nearest the leaf to which the edge connects.
    #' @param label Character string containing the edge label.
    #' @param cost Cost associated with traversing the edge; ModelVariable.
    #' @param p Probability of traversing the edge conditional on
    #'          having reached `fromNode`; ModelVariable.
    #' @return A new `Edge` object.
    initialize = function(fromNode, toNode, label, cost=0, p=1) {
      
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

      # check and set cost
      self$setCost(cost)

      # check and set conditional probability
      self$setP(p)
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
    },
    
    #' @description
    #' Access cost.
    #' @return Cost associated with traversing the edge; numeric.
    getCost = function() {
      return(private$cost)
    },

    #' @description
    #' Set the cost associated with traversing the edge.
    #' @param c Cost as a numeric value.
    #' @return Updated object.
    setCost = function(c) {
      if (is.numeric(c)) {
        private$cost <- c
      }
      else {
        stop("Edge$setCost: argument c must be of type `numeric`")
      }
      return(invisible(self))
    },

    #' @description
    #' Get the conditional probability of traversing the edge.
    #' @return Conditional probability; numeric.
    getP = function() {
      p <- private$p        
      return(p)
    },
    
    #' @description
    #' Set conditional probability of traversing the edge
    #' @param p Conditional probability value; a numeric in the range [0,1].
    #' @return Updated `edge` object.
    setP = function(p) {
      if (is.numeric(p)) {
        if (p>=0 & p<=1) {
          private$p <- p
        }
        else {
          if (p < 0) {
            private$p <- 0
          }
          else {
            if (p > 1) {
              private$p <- 1
            }
          }
          warning("Edge$setP: Conditional probability argument to Edge adjusted to range [0,1]")
        }
      }
      else {
        stop("Edge$setP: Conditional probability argument to Edge must be numeric")
      }
      return(invisible(self))
    }
  )
)
