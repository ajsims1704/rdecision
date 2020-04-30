#' @name pathway.name
#' 
#' @title Return leaf name from a leaf node in a  list of nodes.
#' 
#' @description Returns the name of the pathway from the (final) leaf
#' node of a list of nodes. Intended to be used with @code{path.apply}
#' to return a list of leaf node names for each node-to-leaf traversal.
#' 
#' @note Assumes the leaf node is the last node in the list.
#' 
#' @param nodes list of objects of type 'Node'.
#' @return name of pathway; specifically the @code{pathway}
#'         field of the final leaf node in @code{nodes}. 
#'
#' @export
pathway.name <- function(nodes) {
  # return value
  rc <- NA
  # assume leaf node is the last node in the pathway
  if (length(nodes) > 0) {
    node <- nodes[[length(nodes)]]
    # if leaf node, return its pathway
    if (node$nodeType()=='LeafNode') {
      rc <- node$getPathway()
    }
  }
  return(rc)
}
