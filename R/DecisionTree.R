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
    #' Evaluate the components of payoff associated with the paths in the
    #' decision tree. For each path, the strategy, probability, cost,
    #' benefit and utility are calculated.
    #' @param expected If TRUE, evaluate each model variable as its mean value,
    #'        otherwise sample each one from their uncertainty distribution.
    #' @return A data frame (payoff table) with one row per path and columns
    #' organized as follows:
    #' \describe{
    #' \item{<label of decision node>}{One column for each decision node
    #' in the mode. Each column is named with the label of the node. For each
    #' row (path) the value is the label of the Action edge taken from the
    #' decision node.}
    #' \item{Strategy}{A character string unique to the sequence of decisions
    #' used to traverse path. The form of the string is constructed from node
    #' and edge indexes, but is subject to change in future versions, and
    #' guaranteed only to be unique.}
    #' \item{Leaf}{The label of the leaf node on which the pathway ends; 
    #' normally the clinical outcome.}
    #' \item{Probability}{The probability of traversing the pathway. The total
    #' probability of each strategy should sum to unity.}
    #' \item{Cost}{The cost of traversing the pathway.}
    #' \item{Benefit}{The benefit derived from traversing the pathway.}
    #' \item{Utility}{The utility associated with the outcome (leaf node).}
    #' \item{ECost}{Cost \eqn{*} probability of traversing the pathway.}
    #' \item{EBenefit}{Benefit \eqn{*} probability of traversing the pathway.}
    #' \item{EUtility}{Utility \eqn{*} probability of traversing the pathway.}
    #' }
    evaluate_paths = function(expected=TRUE) {
      # find all root to leaf paths
      P <- self$root_to_leaf_paths()
      # get the names of all decision nodes and create a data frame
      dn <- list()
      sapply(private$V, function(v) {
        if (inherits(v, what="DecisionNode")) {
          dn <<- c(dn, v$label())
        }
      })
      PAYOFF <- data.frame(PID=seq_along(P), stringsAsFactors=FALSE)
      DM <- matrix(data=as.character(NA), nrow=length(P), ncol=length(dn),
                   dimnames=list(list(),dn))
      PAYOFF <- cbind(PAYOFF, DM, deparse.level=1, stringsAsFactors=FALSE)
      PAYOFF$Strategy <- rep(as.character(NA), length(P))
      PAYOFF$Probability <- rep(as.numeric(NA), length(P))
      PAYOFF$Cost <- rep(as.numeric(NA), length(P))
      PAYOFF$Benefit <- rep(as.numeric(NA), length(P))
      # evaluate each path
      for (i in seq_along(P)) {
        # get path
        path <- P[[i]]
        # decisions
        strategy <- ""
        sapply(self$walk(path), function(e) {
          v <- e$source()
          if (inherits(v, what="DecisionNode")) {
            PAYOFF[PAYOFF$PID==i, v$label()] <<- e$label()
            strategy <<- paste(
              strategy, 
              paste(self$element_index(v),self$element_index(e), sep=":"), 
              sep="|")
          }
        })
        PAYOFF[PAYOFF$PID==i, "Strategy"] <- strategy
        # label of the leaf node at end of the path
        PAYOFF[PAYOFF$PID==i, "Leaf"] <- path[[length(path)]]$label()
        # probability
        pr <- 1
        sapply(self$walk(path), function(e) {
          if (inherits(e, what="Reaction")) {
            pr <<- pr * e$p(expected)
          }
        })
        PAYOFF[PAYOFF$PID==i,"Probability"] <- pr
        # cost
        cost <- 0
        sapply(self$walk(path), function(e) {
          cost <<- cost + e$cost(expected)
        })
        PAYOFF[PAYOFF$PID==i,"Cost"] <- cost
        # benefit
        benefit <- 0
        sapply(self$walk(path), function(e) {
          benefit <<- benefit + e$benefit(expected)
        })
        PAYOFF[PAYOFF$PID==i,"Benefit"] <- benefit
        # utility of the leaf node at end of the path
        PAYOFF[PAYOFF$PID==i, "Utility"] <- path[[length(path)]]$utility(expected)
      }
      # add expected cost and utility     
      PAYOFF$ECost <- PAYOFF$Probability*PAYOFF$Cost
      PAYOFF$EBenefit <- PAYOFF$Probability*PAYOFF$Benefit
      PAYOFF$EUtility <- PAYOFF$Probability*PAYOFF$Utility
      # return the payoff table      
      return(PAYOFF)
    },
    
    #' @description 
    #' Evaluate each strategy. Starting with the root, the function
    #' works though all possible paths to leaf nodes and computes the probability,
    #' cost, benefit and utility of each, then aggregates by strategy.   
    #' @param expected If TRUE, evaluate each model variable as its mean value,
    #' otherwise sample each one from their uncertainty distribution.
    #' @param N Number of replicates. Intended for use with PSA (expected=F);
    #' use with expected=T will be repetitive and uninformative. 
    #' @return A data frame with one row per strategy per run and columns
    #' organized as follows:
    #' \describe{
    #' \item{Run}{The run number}
    #' \item{Strategy}{The strategy.}
    #' \item{Cost}{Aggregate cost of the choice.}
    #' \item{Utility}{Aggregate utility of the choice.}
    #' }
    evaluate = function(expected=TRUE, N=1) {
      # names of columns to aggregate
      keep <- c("ECost", "EBenefit", "EUtility")
      # names of columns to copy to identify each strategy
      dn <- c()
      sapply(private$V, function(v) {
        if (inherits(v, what="DecisionNode")) {
          dn <<- c(dn, v$label())
        }
      })
      # make repeated calls 
      DF <- do.call('rbind', lapply(1:N, FUN=function(n){
        # evaluate pathways
        RES <- self$evaluate_paths(expected)
        RES$Strategy <- as.character(RES$Strategy)
        # aggregate them by strategy
        SUM <- aggregate(
          RES[,keep],
          by = list(RES$Strategy),
          FUN = sum
        )
        names(SUM) <- c("Strategy", "Cost", "Benefit", "Utility")
        SUM <- cbind(Run=rep(n, times=nrow(SUM)), SUM)
        # aggregate the Reaction paths by strategy
        DEC <- aggregate(
          RES[,dn],
          by = list(RES$Strategy),
          FUN = function(x){x[1]}
        )
        names(DEC) <- c("Strategy", dn)
        SUM <- merge(SUM, DEC, by="Strategy")
        # return the aggregates
        return(SUM)
      }))
      DF$Strategy <- NULL
      return(DF)
    }
    
    
  )
)


