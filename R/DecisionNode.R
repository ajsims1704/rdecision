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
#' @author Andrew J. Sims \email{andrew.sims5@@nhs.net}
#' @export
#' 
DecisionNode <- R6::R6Class(
  classname = "DecisionNode",
  inherit = Node,
  private = list(
    
    # fields
    costs = 'list',
    
    # description Set numerical cost values in edges.
    # return An updated object.
    setEdgeCosts = function() {
      for (i in 1:length(private$edges)) {
        edge <- private$edges[[i]]
        cost <- private$costs[[i]]
        if (inherits(cost, what='ModelVariable')) {
          c <- cost$value()
          edge$setCost(c)
        }
        else if (is.numeric(cost)) {
          edge$setCost(cost)
        }
        else {
          stop("Edge$setEdgeCosts: cost must be of type `ModelVariable` or `numeric`")
        }
      }  
      return(invisible(self))
    }

  ),
  public = list(

    #' @description 
    #' Create a new decision node.
    #' @param children A list of Nodes which are the children of this
    #'        decision node.
    #' @param edgelabels A vector of character stings containing the labels
    #'        of each choice associated with the decision.
    #' @param costs A list of values containing the costs associated
    #'        with each choice. Each value can be a numeric variable,
    #'        or a ModelVariable.
    #' @return A new DecisionNode object
    initialize = function(children, edgelabels, costs) {

      # ensure base class fields are initialized
      super$initialize()

      # check child nodes
      if (length(children) == 0) {
        stop("`children` must contain at least one object of class `Node`.")
      }
      sapply(children, function(x) {
        if (inherits(x, what="Node")==F){
          stop("Each element in `children` must be of class `Node`")
        }
      })

      # check edge labels
      if (length(edgelabels) != length(children)) {
        stop("`edgelabels` must contain the same number of objects as `children`.")
      }
      sapply(edgelabels, function(x) {
        if (!is.character(x)){
          stop("Each element in `edgelabels` must be of class `character`.")
        }
      })

      # add edges to this node with zero costs, to ensure initialization
      for (i in 1:length(children)) {
        edge <- Edge$new(self, children[[i]], edgelabels[[i]], cost=0)
        private$addEdge(edge)
      }

      # check and store costs
      if (length(costs) != length(children)) {
        stop("`costs` must contain the same number of objects as `children`.")
      }
      sapply(costs, function(x) {
        if (is.numeric(x)) {
        }
        else if (inherits(x, what='ModelVariable')==T) {
        }
        else {
          stop("Each element in `costs` must be of class `numeric` or `ModelVariable`")
        }
      })
      private$costs <- costs
      
      # set the costs for each edge
      private$setEdgeCosts()
    },

    #' @description 
    #' Function to return a list of model variables associated with the node.
    #' @return List of model variables associated with the node.
    getModelVariables = function() {
      # make a list of all private objects that may be associated with model variables
      objects <- c(private$costs)
      # iterate objects and create list of model variables
      mvlist <- list()
      lapply(objects, FUN=function(o) {
        if (inherits(o, what='ModelVariable')) {
          mvlist <<- c(mvlist, o)
        }
      })
      # return list of model variables
      return(mvlist)
    },
    
    #' @description 
    #' Sample all model variables in this node and update edges.
    #' @param expected If TRUE cause each model variable to return its expected
    #'        value at the next call to `value()`. If FALSE each model variable
    #'        will return the sampled value. Default is FALSE.
    #' @return An updated DecisionNode object.
    sample = function(expected=F) {
      # get the model variables associated with this node
      mvlist <- self$getModelVariables()
      # sample them
      sapply(mvlist, FUN=function(mv) {
        mv$sample(expected)
      })
      # update edges
      private$setEdgeCosts()
      # return reference to updated node
      return(invisible(self))  
    },
    
    #' @description 
    #' Evaluate a decision. Starting with this decision node, the function
    #' works though all possible paths and computes the probability,
    #' cost and utility of each. 
    #' @param expected If TRUE, evaluate each model variable as its mean value,
    #'        otherwise sample each one from their uncertainty distrbution.
    #' @return A data frame with one row per path and columns organized as
    #' follows:
    #' \describe{
    #' \item{Choice}{The choice with which the path is associated.}
    #' \item{Pathway}{The leaf node on which the pathway ends; normally the 
    #' clinical outcome.}
    #' \item{Probability}{The probability of traversing the pathway. The total
    #' probability of each choice should sum to unity; i.e. the sum of the 
    #' Probability column should equal the number of branches leaving the 
    #' decision node.}
    #' \item{Cost}{The cost of traversing the pathway.}
    #' \item{ExpectedCost}{Cost \eqn{*} probability of traversing the pathway.}
    #' \item{Utility}{The utility associated with the outcome.}
    #' \item{ExpectedUtility}{Utility \eqn{*} probability of traversing the pathway.}
    #' }
    evaluate = function(expected=T) {
      # sample this node and all descendants
      descendants <- self$descendantNodes()
      lapply(descendants, FUN=function(n) {
        n$sample(expected)
      })
      # evaluate cost and utility of each path
      RES <- data.frame(
        'Choice' = unlist(path.apply(self, FUN=pathway.choice)),
        'Pathway' = unlist(path.apply(self, FUN=pathway.name)),
        'Probability' = unlist(path.apply(self, FUN=pathway.probability)),
        'Cost' = unlist(path.apply(self, FUN=pathway.cost)),
        'ExpectedCost' = NA,
        'Utility' = unlist(path.apply(self, FUN=pathway.utility)),
        'ExpectedUtility' = NA
      )
      RES$ExpectedCost <- round(RES$Probability*RES$Cost,2)
      RES$ExpectedUtility <- round(RES$Probability*RES$Utility,4)
      return(RES)
    }
  )
)
