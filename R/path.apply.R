#' @name path.apply
#' @title An apply function for node-to-leaf paths in a decision tree.
#' 
#' @description \code{path.apply} traverses each node-to-leaf path
#'              in the tree, starting with @code{node}, and applies
#'              function FUN to each list of nodes in each node-to-leaf
#'              path. 
#'              
#' @param node first node in the tree to which FUN should be applied.
#' @param FUN  function to apply to each list of nodes starting with
#'             \code{node} and ending with one of the leaf nodes. The
#'             first agument to FUN must be a list of Nodes.
#' @param ... optional arguments to FUN.
#' @return A list of length equal to the number of node-to-leaf paths
#'         in the tree with each member of the list equal to the result
#'         of applying FUN to that list of node-to-leaf nodes. For example,
#'         if FUN=length, the return value will be a list of the node
#'         count of each node-to-leaf traversal of the tree.
#'
#' @export
path.apply <- function(node, FUN, ...) {
  path <- list()
  rc <- list()
  toLeaf <- function(node) {
    # push current node to path
    path[[length(path)+1]] <<- node
    # leaf reached; apply FUN to current path
    if (node$hasChildNodes()) {
      # process child nodes
      for (child in node$childNodes()) {
        toLeaf(child)
      }
    }
    else {
      rv <- do.call(FUN, args=list(path, ...))
      rc[[length(rc)+1]] <<- rv
    }
    # pop current node from path
    path <<- path[1:(length(path)-1)]
  }
  toLeaf(node)
  return(rc)
}
