#' @title 
#' Path
#' 
#' @description 
#' An R6 class for pathway through a decision tree.
#' 
#' @details 
#' A class to represent a traversal through a decision tree. Essentially
#' a wrapper for an ordered list of Nodes.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims5@@nhs.net}
#' @export
#' 
Path <- R6::R6Class(
  classname = "Path",
  
  private = list(
    nodes = 'list'
  ),
  
  public = list(
    
    #' @description 
    #' Create a path from a list of nodes. Must end on a leaf node.
    #' @param A list of Nodes.
    initialize = function(nodes) {
      # check the supplied node list
      if (length(nodes) < 1) {
        stop("Path$new: argument nodes must have at least one element")
      }
      sapply(nodes, FUN = function(n) {
        if (!inherits(n, what='Node')) {
          stop("Path$new: All elements of nodes must be of type Node")
        }
      })
      last <- nodes[[length(nodes)]]
      if (last$nodeType() != 'LeafNode') {
        stop("Path$new: the supplied path in nodes must end on a LeafNode")
      }
      private$nodes <- nodes
    },

    #' @description Returns the name of the choice arising from the first decision
    #' node of a list of nodes. In many decision trees, where the decision 
    #' node is at the left, many leaf nodes
    #' (pathway names) will be associated with the same choice.
    #' @note Uses the first decision node in the list.
    #' @return label of the choice; specifically the \code{edgelabel}
    #'         field of the first decision node in \code{nodes}. 
    #'
    getChoice = function() {
      # return value
      rc <- NA
      # find first decision node
      for (i in 1:(length(private$nodes)-1)) {
        thisNode <- private$nodes[[i]]
        nextNode <- private$nodes[[i+1]]
        if (thisNode$nodeType()=='DecisionNode') {
          rc <- thisNode$getLabel(nextNode)
          break
        }
      }
      # return the choice
      return(rc)
    },
    
    #' @description Returns the name of the pathway from the (final) leaf
    #' node of a list of nodes. 
    #' @note Assumes the leaf node is the last node in the list.
    #' @return name of pathway; specifically the @code{pathway}
    #'         field of the final leaf node in @code{nodes}. 
    getName = function() {
      # return value
      rc <- NA
      # assume leaf node is the last node in the pathway
      node <- private$nodes[[length(private$nodes)]]
      # if leaf node, return its pathway
      if (node$nodeType()=='LeafNode') {
        rc <- node$getName()
      }
      return(rc)
    },
    
    #' @description Calculates the product of the conditional P values in
    #' the tree traversal.
    #' @note Assumes the nodes are supplied in order of traversal and that
    #' the final node is a leaf node.
    #' @return Product of conditional probabilities.
    getProbability = function() {
      # p value to return
      p <- 1
      # step through each node in the path except the last
      for (i in 1:(length(private$nodes)-1)){
        thisNode <- private$nodes[[i]]
        # if chance node, get p for the path to the next node
        if (thisNode$nodeType()=='ChanceNode') {
          nextNode <- private$nodes[[i+1]]
          nodeP <- thisNode$getP(nextNode)
          p <- p * nodeP
        }
      }
      return(p)
    },
    
    #' @description Calculates the sum of the pathway costs in
    #' the tree traversal.
    #' @note Assumes that the final node is a leaf node.
    #' @return Sum of the pathway costs.
    getCost = function() {
      # cost value to return
      cost <- 0
      # step through each node in the path
      for (i in 1:(length(private$nodes)-1)){
        thisNode <- private$nodes[[i]]
        # if chance node, get cost for the path to the next node
        if (thisNode$nodeType() %in% c('ChanceNode', 'DecisionNode')) {
          nextNode <- private$nodes[[i+1]]
          nodeCost <- thisNode$getCost(nextNode)
          cost <- cost + nodeCost
        }
      }
      return(cost)
    },
    
    #' @description Calculates the sum of the pathway utilities in
    #' the tree traversal.
    #' @note Assumes that only the final leaf node has a utility value.
    #' @return Sum of pathway utilities
    getUtility = function() {
      # cost value to return
      utility <- NA
      # step through each node in the path
      for (i in 1:length(private$nodes)){
        n <- private$nodes[[i]]
        # if leaf node, get utility
        if (inherits(n, what='LeafNode')) {
          utility <- n$getUtility()
          break
        }
      }
      return(utility)
    },
    
    #' @description 
    #' Evaluate and tabulate the pathway.
    #' @return Data frame with one row, as follows:
    #' \describe{
    #' \item{Choice}{The choice from which the pathway leads.}
    #' \item{Pathway}{The end-point or outcome; i.e. label ofleaf node.}
    #' \item{Probability}{Product of probabilities.}
    #' \item{Cost}{Sum of cost.}
    #' \item{Utility}{Sum of utility.}
    #' }
    tabulate = function() {
      # create a data frame of model variables
      DF <- data.frame(
        'Choice' = self$getChoice(),
        'Pathway' = self$getName(),
        'Probability' = self$getProbability(),
        'Cost' = self$getCost(),
        'Utility' = self$getUtility()
      )
      # return it
      return(DF)
    }
  )
)
