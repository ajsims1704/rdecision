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
    #'        decision node.
    #' @param edgelabels A vector of character stings containing the labels
    #'        of each choice associated with the decision.
    #' @return A new DecisionNode object
    initialize = function(children, edgelabels) {

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

      # check edge labels
      if (length(edgelabels) != length(children)) {
        stop("`edgelabels` must contain the same number of objects as `children`.")
      }
      sapply(edgelabels, function(x) {
        if (!is.character(x)){
          stop("Each element in `edgelabels` must be of class `character`.")
        }
      })

      # add edges to this node
      for (i in 1:length(children)) {
        edge <- Edge$new(self, children[[i]], edgelabels[[i]])
        private$addEdge(edge)
      }
      
      return(invisible(self))
    },
    
    #' @description 
    #' Update the tree by sampling model variables and then updating numerical
    #' edge values.
    #' @param expected if TRUE set model variables to their expectation; 
    #' otherwise sample from their uncertainty distributions.
    #' @returns Updated DecisionNode object.
    updateTree = function(expected=TRUE) {
      # sample model variables of this node and all descendants
      descendants <- self$descendantNodes()
      lapply(descendants, FUN=function(n) {
        n$sample_modvars(expected)
      })
      # return updated DecisionNode
      return(invisible(self))
    }, 
    
    #' @description 
    #' Evaluate a decision. Starting with this decision node, the function
    #' works though all possible paths and computes the probability,
    #' cost and utility of each. 
    #' @param expected If TRUE, evaluate each model variable as its mean value,
    #'        otherwise sample each one from their uncertainty distrbution.
    #' @param uncorrelate If TRUE, resample and update the tree between
    #' the evaluation of each choice. This causes any model variables that
    #' are common to more than one choice to be resampled between choices,
    #' and removes correlation due to shared model variables. Other forms
    #' of correlation may not be removed.
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
    evaluatePathways = function(expected=TRUE, uncorrelate=FALSE) {
      # if no requireement to uncorrelate, resample the tree once
      if (!uncorrelate) {
        self$updateTree(expected)
      }
      # choice by choice
      choices <- sapply(private$edges, FUN=function(e){e$getLabel()})
      choicerows <- lapply(choices, FUN=function(choice) {
        if (uncorrelate) {
          self$updateTree(expected)
        }
        paths <- self$getPathways(choice)
        RCH <- do.call('rbind', lapply(paths, FUN=function(x){x$tabulate()}))
        return(RCH)
      })
      RES <- do.call('rbind', choicerows)
      # add expected cost and utility     
      RES$ExpectedCost <- RES$Probability*RES$Cost
      RES$ExpectedUtility <- RES$Probability*RES$Utility
      return(RES)
    },
    
    #' @description 
    #' Evaluate each choice. Starting with this decision node, the function
    #' works though all possible paths and computes the probability,
    #' cost and utility of each, then aggregates by choice. 
    #' @param expected If TRUE, evaluate each model variable as its mean value,
    #'        otherwise sample each one from their uncertainty distrbution.
    #' @param uncorrelate If TRUE, resample and update the tree between
    #' the evaluation of each choice. This causes any model variables that
    #' are common to more than one choice to be resampled between choices,
    #' and removes correlation due to shared model variables.
    #' @param N Number of replicates. Intended for use with PSA (expected=F);
    #' use with expected=T will be repetitive and uninformative. 
    #' @return A data frame with one row per choice per run and columns
    #' organized as follows:
    #' \describe{
    #' \item{Run}{The run number}
    #' \item{Choice}{The choice.}
    #' \item{Cost}{Aggregate cost of the choice.}
    #' \item{Utility}{Aggregate utility of the choice.}
    #' }
    evaluateChoices = function(expected=TRUE, uncorrelate=FALSE, N=1) {
      DF <- do.call('rbind', lapply(1:N, FUN=function(n){
        # evaluate pathways
        RES <- self$evaluatePathways(expected, uncorrelate)
        RES$Choice <- as.character(RES$Choice)
        # aggregate them by choice
        SUM <- aggregate(
          RES[,c('ExpectedCost', 'ExpectedUtility')],
          by = list(RES$Choice),
          FUN = sum
        )
        names(SUM) <- c('Choice', 'Cost', 'Utility')
        SUM <- cbind(Run=rep(n, times=nrow(SUM)), SUM)
        # return the aggregates
        return(SUM)
      }))
      return(DF)
    }
    
  )
)
