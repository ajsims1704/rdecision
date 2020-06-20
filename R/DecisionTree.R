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
#' \item{Each node must inherit from one of \code{DecisionNode},
#' \code{ChanceNode} or \code{LeafNode}. Formally the set of vertices
#' must be a disjoint union of sets of decision nodes, chance nodes
#' and leaf nodes.}
#' \item{Each edge must inherit from either \code{Action} or
#' \code{Reaction}.}
#' \item{Nodes and edges must form a tree with a single root and
#' there must be a unique path from the root to each node.
#' In graph theory, the directed graph formed by the nodes and edges must
#' be an \dfn{arborescence}.}
#' \item{All and only leaf nodes must have no children.}
#' \item{All and only edges that have source endpoints joined to 
#' decision nodes must inherit from \code{Action}.}
#' \item{All and only edges that have source endpoints joined to 
#' chance nodes must inherit from \code{Reaction}.}
#' \item{The sum of probabilities of each set of reaction edges 
#' with a common source endpoint must be 1.}
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
      # check the types of each node
      sapply(V, function(v) {
        if (inherits(v,what="DecisionNode")) {
          
        } else if (inherits(v, what="ChanceNode")) {
          
        } else if (inherits(v, what="LeafNode")) {
          
        } else {
          rlang::abort(
            "Each node must be a 'DecisionNode', 'ChanceNode' or 'LeafNode'.",
            class="incorrect_node_type")
        }
      })
      # initialize the base class(es)
      super$initialize(V,E)
      # return a new DecisionTree object
      return(invisible(self))
    }
    
  )
)


