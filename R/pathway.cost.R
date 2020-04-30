#' @name pathway.cost
#' 
#' @title Calculate pathway cost from decision and chance nodes.
#' 
#' @description Calculates the sum of the pathways costs in
#' the tree traversal.
#' 
#' @note Assumes that the final node is a leaf node.
#' 
#' @param nodes List of nodes in the node-to-leaf traversal path.
#' 
#' @export
pathway.cost <- function(nodes) {
  # cost value to return
  cost <- 0
  # step through each node in the path
  if (length(nodes) > 1) {
    for (i in 1:(length(nodes)-1)){
      thisNode <- nodes[[i]]
      # if chance node, get cost for the path to the next node
      if (thisNode$nodeType() %in% c('ChanceNode', 'DecisionNode')) {
        nextNode <- nodes[[i+1]]
        nodeCost <- thisNode$getCost(nextNode)
        cost <- cost + nodeCost
      }
    }
  }
  return(cost)
}
