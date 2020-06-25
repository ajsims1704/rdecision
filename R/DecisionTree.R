#' @title 
#' DecisionTree
#' 
#' @description 
#' An R6 class to represent a decision tree
#' 
#' @details 
#' A class to represent a decision tree. An object contains a tree of
#' decision nodes, chance nodes and leaf nodes, connected by edges
#' (either actions or reactions) and which satisfies the following
#' conditions:
#' \enumerate{
#' \item{Nodes and edges must form a tree with a single root and
#' there must be a unique path from the root to each node.
#' In graph theory terminology, the directed graph formed by the nodes
#' and edges must be an \dfn{arborescence}.}
#' \item{Each node must inherit from one of \code{DecisionNode},
#' \code{ChanceNode} or \code{LeafNode}. Formally the set of vertices
#' must be a disjoint union of sets of decision nodes, chance nodes
#' and leaf nodes.}
#' \item{All and only leaf nodes must have no children.}
#' \item{Each edge must inherit from either \code{Action} or
#' \code{Reaction}.}
#' \item{All and only edges that have source endpoints joined to 
#' decision nodes must inherit from \code{Action}.}
#' \item{All and only edges that have source endpoints joined to 
#' chance nodes must inherit from \code{Reaction}.}
#' \item{The sum of probabilities of each set of reaction edges 
#' with a common source endpoint must be 1.}
#' \item{Each \code{DecisionNode} must have a label, and the labels of all
#' \code{DecisionNodes} must be unique within the model.}
#' \item{Each \code{Action} must have a label, and the labels of  
#' \code{Action}s that share a common source endpoint must be unique.}
#' }
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
DecisionTree <- R6::R6Class(
  classname = "DecisionTree",
  inherit = Arborescence,
  private = list(
  ),
  public = list(
    
    #' @description 
    #' Create a new decision tree. The tree must consist of a set of
    #' nodes and a set of edges which satisfy the conditions given
    #' in the details section of this class.
    #' @param V A list of nodes.
    #' @param E A list of edges.
    #' @return A DecisionTree object
    initialize = function(V,E) {
      # initialize the base class(es); checks that {V,E} form an arboresecence
      super$initialize(V,E)
      # check the V is a disjoint union {D,C,L} nodes
      D <- which(sapply(V, function(v){inherits(v,what="DecisionNode")}),arr.ind=TRUE)
      C <- which(sapply(V, function(v){inherits(v,what="ChanceNode")}),arr.ind=TRUE)
      L <- which(sapply(V, function(v){inherits(v,what="LeafNode")}),arr.ind=TRUE)
      W <- union(D,union(C,L))
      if (!setequal(seq_along(V),W)) {
        rlang::abort(
          "Each node must be a 'DecisionNode', 'ChanceNode' or 'LeafNode'.",
          class="incorrect_node_type")
      }
      # all and only leaf nodes must have no children
      P <- which(sapply(V,function(v) {self$is_parent(v)}),arr.ind=TRUE)
      if (!setequal(P,union(D,C))) {
        rlang::abort("All and only leaf nodes must have no children",
                     class = "leaf_non-child_sets_unequal")
      }  
      # each edge must inherit from action or reaction
      if (!all(sapply(E,function(e){inherits(e, what=c("Action","Reaction"))}))) {
        rlang::abort("Each edge must inherit from Action or Reaction", 
                     class="incorrect_edge_type")
      }
      # Action (reaction) edges must emerge from DecisionNodes (ChanceNodes)
      eok <- sapply(E,function(e){
        rc <- TRUE
        if (inherits(e,what="Action")) {
          if (!inherits(e$source(), what="DecisionNode")) {
            rc <- FALSE
          }
        } else {
          if (!inherits(e$source(), what="ChanceNode")) {
            rc <- FALSE
          }
        }
        return(rc)
      })
      if (!all(eok)) {
        rlang::abort("Actions must start at DecisionNodes; Reactions must start at ChanceNodes",
                     class = "incorrect_edge_type ")
      }
      # DecisionNode labels must be unique and all their Action labels must be unique
      D.lab <- sapply(D,function(d){
        v <- private$V[[d]]
        K <- self$direct_successors(v)
        choices <- sapply(K, function(k) {
          w <- self$walk(list(v,k))
          e <- w[[1]]
          return(e$label())
        })
        if (length(choices) != length(unique(choices))) {
          rlang::abort("Labels of actions with a common source node must be unique",
                       class="non_unique_labels")
        }
        return(v$label())
      })
      if (length(D.lab) != length(unique(D.lab))) {
        rlang::abort("Labels of DecisionNodes must be unique", class="non_unique_labels")
      }
      # return a new DecisionTree object
      return(invisible(self))
    },
    
    #' @description 
    #' Compute the components of payoff associated with the paths in the
    #' decision tree. For each path, the strategy, probability, cost,
    #' benefit and utility are calculated.
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
    #' \item{Strategy}{The strategy used to traverse the path; i.e. a list of 
    #' decision nodes and the decision made at each.}
    #' \item{Leaf}{The leaf node on which the pathway ends; normally the 
    #' clinical outcome.}
    #' \item{Probability}{The probability of traversing the pathway. The total
    #' probability of each strategy should sum to unity.}
    #' \item{Cost}{The cost of traversing the pathway.}
    #' \item{ExpectedCost}{Cost \eqn{*} probability of traversing the pathway.}
    #' \item{Utility}{The utility associated with the outcome.}
    #' \item{ExpectedUtility}{Utility \eqn{*} probability of traversing the pathway.}
    #' }
    payoffs = function(expected=TRUE, uncorrelate=FALSE) {
      
      # get all pathways through the tree
      r <- self$root()
      P <- list()
      sapply(private$V, function(v){
        if (self$is_leaf(v)) {
          pp <- self$paths(r,v)
          P <- list(P, pp[[1]])    
        }
      })
      
      
      
      # # if no requirement to uncorrelate, resample the tree once
      # if (!uncorrelate) {
      #   self$update(expected)
      # }
      # # choice by choice
      # choices <- private$dn$get_choices()
      # choicerows <- lapply(choices, FUN=function(choice) {
      #   if (uncorrelate) {
      #     self$update(expected)
      #   }
      #   paths <- private$dn$getPathways(choice)
      #   RCH <- do.call('rbind', lapply(paths, FUN=function(x){x$tabulate()}))
      #   return(RCH)
      # })
      # RES <- do.call('rbind', choicerows)
      # # add expected cost and utility     
      # RES$ExpectedCost <- RES$Probability*RES$Cost
      # RES$ExpectedUtility <- RES$Probability*RES$Utility
      
      return(RES)
    }
    
  )
)


