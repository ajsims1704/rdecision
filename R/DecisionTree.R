#' @title 
#' DecisionTree
#' 
#' @description 
#' An R6 class to represent a decision tree
#' 
#' @details 
#' A class to represent a decision tree. An object contains a tree of
#' decision nodes, chance nodes and states, connected by edges, and
#' meeting the criteria for a decision tree.
#' 
#' @note
#' In graph theory a decision tree is a k-ary directed rooted tree, or 
#' 'arborescence'. This is considered a form of directed graph (digraph) by
#' several authors. In `rdecision` the root is a DecisionNode and in decision 
#' trees used in health
#' economics there is an implied directionality, from root to leaf nodes.}
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
DecisionTree <- R6::R6Class(
  classname = "DecisionTree",
  private = list(
    dn = NULL
  ),
  public = list(
    
    #' @description 
    #' Create a new decision tree. The tree must start with a decision node
    #' which is the root of a tree of chance nodes and state nodes
    #' connected by edges.
    #' @param dn An object inheriting from class DecisionNode.
    #' @return A DecisionTree object
    initialize = function(dn) {
      # check and set dn
      if (!inherits(dn, what="DecisionNode")) {
        rlang::abort("Argument 'dn' must of class 'DecisionNode'",
                     class="non-DecisionNode_root")
      }
      private$dn <- dn
      return(invisible(self))
    },
    
    #' @description 
    #' Update the tree by sampling model variables.
    #' @param expected if TRUE set model variables to their expectation; 
    #' otherwise sample from their uncertainty distributions.
    #' @returns Updated DecisionNode object.
    update = function(expected=TRUE) {
      # sample model variables of the root node and all its descendants
      descendants <- private$dn$descendantNodes()
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
      # if no requirement to uncorrelate, resample the tree once
      if (!uncorrelate) {
        self$update(expected)
      }
      # choice by choice
      choices <- private$dn$get_choices()
      choicerows <- lapply(choices, FUN=function(choice) {
        if (uncorrelate) {
          self$update(expected)
        }
        paths <- private$dn$getPathways(choice)
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


   