#' @name node.mvlist
#' 
#' @title Returns a list of model variables associated with a node
#' @author Andrew J. Sims \email{andrew.sims5@nhs.net}
#' 
#' @description Each node may have several ModelVariables associated with
#' it, directly as costs, utilities etc, or indirectly via 
#' ModelVariableExpressions that have been supplied as costs, probabilities, 
#' utilities etc. This function is intended for use with \code{node.apply}
#' which traverses each node in a (sub-)tree. 
#' 
#' @param nodes List of objects of type 'Node'.
#' @return Unique list of model variables associated with any part of the subtree.
#'
#' @export
#' 
node.mvlist <- function(nodes) {
  # return value
  mvlist <- list()
  # iterate the nodes
  lapply(nodes, FUN=function(n) {
    mv <- n$getModelVariables()
    if (length(mv)>0) {
      mvlist <<- c(mvlist, unlist(mv))
    }
  })
  # unique variables (use duplicated to avoid removing names)
  umv <- mvlist[!duplicated(mvlist)]
  # return the choice
  return(umv)
}
