#' @name pathway.utility
#' 
#' @title Calculate pathway utility.
#' 
#' @description Calculates the sum of the pathway utilities in
#' the tree traversal.
#' 
#' @note Assumes that only the final leaf node has a utility value.
#' 
#' @param nodes List of nodes in the node-to-leaf traversal path.
#' 
#' @export
pathway.utility <- function(nodes) {
  # cost value to return
  utility <- NA
  # step through each node in the path
  if (length(nodes) > 0) {
    for (i in 1:length(nodes)){
      n <- nodes[[i]]
      # if leaf node, get utility
      if (inherits(n, what='LeafNode')) {
        utility <- n$getUtility()
        break
      }
    }
  }
  return(utility)
}
