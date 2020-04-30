#' @name node.apply
#' @title An apply function for all nodes in a decision tree
#' @author Andrew J. Sims \email{andrew.sims5@nhs.net}
#' @description \code{node.apply} traverses each node path
#'              in the tree, starting with \code{node}, and applies
#'              function FUN to each node in the (sub-)tree.
#'              
#' @param node first node in the tree to which FUN should be applied.
#' @param FUN  function to apply to each node in the subtree.
#' @param ... optional arguments to FUN.
#' @return A list of length equal to the number of nodes in the 
#'         tree with each member of the list equal to the result
#'         of applying FUN to that node. For example,
#'         if FUN=length, the return value will be the number of
#'         nodes in the tree.
#'
#' @export
node.apply <- function(node, FUN, ...) {
  nodes <- list()
  toLeaf <- function(node) {
    # push current node to path
    nodes[[length(nodes)+1]] <<- node
    # process child nodes if not leaf
    if (node$hasChildNodes()) {
      for (child in node$childNodes()) {
        toLeaf(child)
      }
    }
  }
  toLeaf(node)
  rc <- do.call(FUN, args=list(nodes, ...))
  return(rc)
}
