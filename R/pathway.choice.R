#' @name pathway.choice
#' 
#' @title Returns the choice label associated with the first decision node
#' in a list of nodes.
#' 
#' @description Returns the name of the choice arising from the first decision
#' node of a list of nodes. Intended to be used with \code{path.apply}
#' to return a list of choices for each node-to-leaf traversal. In many
#' decision trees, where the decision node is at the left, many leaf nodes
#' (pathway names) will be associated with the same choice.
#' 
#' @note Uses the first decision node in the list.
#' 
#' @param nodes list of objects of type 'Node'.
#' @return label of the choice; specifically the \code{edgelabel}
#'         field of the first decision node in \code{nodes}. 
#'
#' @export
pathway.choice <- function(nodes) {
  # return value
  rc <- NA
  # find first decision node
  if (length(nodes) > 1) {
    for (i in 1:(length(nodes)-1)) {
      thisNode <- nodes[[i]]
      nextNode <- nodes[[i+1]]
      if (thisNode$nodeType()=='DecisionNode') {
        rc <- thisNode$getLabel(nextNode)
        break
      }
    }
  }
  # return the choice
  return(rc)
}
