#' @name pathway.probability
#' 
#' @title Calculate pathway probability from conditional probabilities.
#' 
#' @description Calculates the product of the conditional P values in
#' the tree traversal.
#' 
#' @note Assumes the nodes are supplied in order of traversal and that
#' the final node is a leaf node.
#' 
#' @param nodes List of nodes in the node-to-leaf traversal path.
#' 
#' @export
pathway.probability <- function(nodes) {
  # p value to return
  p <- 1
  # step through each node in the path except the last
  if (length(nodes) > 1) {
    for (i in 1:(length(nodes)-1)){
      thisNode <- nodes[[i]]
      # if chance node, get p for the path to the next node
      if (thisNode$nodeType()=='ChanceNode') {
        nextNode <- nodes[[i+1]]
        nodeP <- thisNode$getP(nextNode)
        p <- p * nodeP
      }
    }
  }
  return(p)
}
