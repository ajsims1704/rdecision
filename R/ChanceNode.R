#' @title
#' ChanceNode
#'
#' @description 
#' An R6 class to represent a chance node in a decision tree.
#' 
#' @details
#' An R6 class to represent a chance node in a decision tree. 
#' The node is associated with at least two branches to other nodes, each 
#' of which has a conditional probability (the probability of following
#' that branch given that the node has been reached) and a cost. 
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
ChanceNode <- R6::R6Class(
  classname = "ChanceNode",
  inherit = Node,
  private = list(
    # private fields
    # p = NULL,
    # ptype = "numeric"
  ),
  public = list(
    
    #' @description
    #' Create a new ChanceNode object
    #' @param children a list of \eqn{k \ge 2} Nodes which will be the children
    #'        of this node.
    #' @param edgelabels a list of \eqn{k} character strings to label each edge 
    #'        (branch) leaving the `ChanceNode`, given in the same order as
    #'        `children`.
    #' @param ptype a character string taking one of four possible
    #'        values to define how the \code{p} argument is defined:
    #'        'numeric', 'MV', 'Beta' or 'Dirichlet'. 
    #' @param p a list of probabilities associated with \eqn{k} branches.
    #'        There are four possible configurations of the list 
    #'        based on the \code{ptype} argument.
    #' \describe{
    #' \item{'numeric'}{\eqn{k} numeric values; the simplest case in which the   
    #' probabilities are certain. Supplied values should add
    #' to unity and be given in the same order as `children`.}
    #' \item{'MV'}{At least one `ModVar` and exactly one
    #' `NA``, and the remainder either `numeric` or `ModVar`, given in the
    #' same order as `children`. The single NA will be replaced on evaluation of
    #' the model variables by a value to ensure the sum of probabilities
    #' is unity. This option is not recommended, because there is
    #' a chance that during sampling, individual branch probabilities
    #' may be less than zero, or that the sum of the sampled model variable
    #' expressions may exceed 1.}
    #' \item{'Beta'}{One `BetaModVar` and one NA. Used for \eqn{k=2}. The 
    #' element defined as NA will be replaced by one minus the sampled 
    #' value of the supplied beta distribution.}
    #' \item{'Dirichlet'}{One DirichletModVar, with \eqn{k} parameters,
    #' given in the same order as `children`.}
    #' \item{'auto'}{Infer which of the previous options applies based on
    #' the types of elements of p}
    #' }
    #' @return A new `ChanceNode` object
    initialize = function(label="") {
      ## ensure base class fields are initialized
      super$initialize(label)
      # ## check child nodes
      # if (length(children) < 2) {
      #   rlang::abort("Argument 'children' must contain at least two objects of class 'Node'.",
      #                class="incorrect_child_count")
      # }
      # sapply(children, function(x) {
      #   if (!inherits(x, what="Node")){
      #     rlang::abort("Each element in 'children' must be of class 'Node'.",
      #                  class="non-Node_child")
      #   }
      # })
      # 
      # ## check edge labels
      # if (length(edgelabels) != length(children)) {
      #   rlang::abort("Argument 'edgelabels' must contain the same number of objects as 'children'.",
      #                class="incorrect_edge_label_count")
      # }
      # sapply(edgelabels, function(x) {
      #   if (!is.character(x)){
      #     rlang::abort("Each element in 'edgelabels' must be of class 'character'.",
      #                  class="non-string_edge_label")
      #   }
      # })
      # 
      # ## add edges to this node
      # for (i in 1:length(children)) {
      #   edge <- Arrow$new(self, children[[i]], edgelabels[[i]])
      #   private$addArrow(edge)
      # }
      # 
      # ## set ptype
      # if (!is.character(ptype)) {
      #   rlang::abort("Argument 'ptype' must be of class 'character'",
      #                class="non-string_ptype")
      # }
      # else {
      #   private$ptype <- ptype  
      # }
      # 
      # ## check and store p values
      # 
      # # count each type of member of p
      # k <- length(p)
      # nna <- sum(is.na(p))
      # nnu <- sum(sapply(p, is.numeric))
      # nmv <- sum(sapply(p, FUN=function(e){return(inherits(e, what='ModVar'))}))
      # 
      # # switch on ptype 
      # if (ptype == 'numeric') {
      #   if (k != length(children)) {
      #     stop('ChanceNode$new: `p` must contain the same number of elements as children')
      #   }
      #   if (nnu != k) {
      #     stop('ChanceNode$new: all elements of `p` must be of type `numeric` for ptype=numeric')
      #   }
      #   private$p <- p
      # }
      # else if (ptype == 'MV') {
      #   if (k != length(children)) {
      #     stop('ChanceNode$new: `p` must contain the same number of elements as children')
      #   }
      #   if (nna != 1) {
      #     stop("ChanceNode$new: one element of p must be NA for `ptype='MVE`.")
      #   }
      #   if (nmv < 1) {
      #     stop("ChanceNode$new: at least one element of p must be a ModVar for `ptype=MVE`.")
      #   }
      #   if ((nna+nmv+nnu) != k) {
      #     stop("ChanceNode$new: all elements of `p` must be of type `numeric`, `ModVar` or `NA` for `ptype=MVE`")
      #   }
      #   warning("ChanceNode$new: `ptype='MV'` may lead to p values out of range [0,1].")
      #   private$p <- p
      # }
      # else if (ptype == 'Beta') {
      #   stop("ChanceNode$new: `ptype=Beta` not yet implemented")
      # }
      # else if (ptype == 'Dirichlet') {
      #   stop("ChanceNode$new: `ptype=Dirichlet` not yet implemented")
      # }
      # else if (ptype == "auto") {
      #   if (nnu == k) {
      #     private$ptype <- 'numeric'
      #     private$p <- p
      #   }
      #   else if ( (nna==1) && (nmv>=1) & ((nna+nmv+nnu)==k) ) {
      #     private$ptype <- 'MV'
      #     private$p <- p
      #     warning("ChanceNode$new: 'ptype=\"MV\"' may cause p values outside [0,1].",
      #             call.=FALSE)
      #   }
      #   else {
      #     stop("ChanceNode$new: cannot guess `ptype` from supplied p for `ptype=auto`.")
      #   }
      # }
      # # anything else is illegal
      # else {
      #   stop("ChanceNode$new: `ptype` must be one of 'numeric', 'MV', 'Beta' or 'Dirichlet'")
      # }
      # return a ChanceNode object
      return(invisible(self))
    }

    #' #' @description
    #' #' Function to return the conditional probability of the edge which links to
    #' #' the specified child node.
    #' #' @param childNode child node to which to find probability of linking edge 
    #' #' @return Numerical value of probability.
    #' get_p = function(childNode) {
    #'   # create vector of p values, with one element per edge
    #'   pedge <- vector('numeric', length=length(private$edges))
    #'   # compute the probability associated with each edge
    #'   if (private$ptype == 'numeric') {
    #'     for (i in 1:length(private$edges)) {
    #'       pedge[i] <- private$p[[i]]
    #'     }
    #'   }
    #'   else if (private$ptype == 'MV') {
    #'     for (i in 1:length(private$edges)) {
    #'       p <- private$p[[i]]
    #'       if (inherits(p, what='ModVar')) {
    #'         v <- p$value()
    #'         pedge[i] <- v
    #'       }
    #'       else {
    #'         pedge[i] <- p
    #'       }
    #'     }
    #'     pedge[is.na(pedge)] <- 1 - sum(pedge, na.rm=T)
    #'   }
    #'   else {
    #'     rlang::abort("Only ptype='numeric' and ptype='MV' are currently supported",
    #'                  class="unsupported_ptype")
    #'   }
    #'   # return the single p value associated with the edge to the given child node
    #'   rv <- 0
    #'   ie <- private$whichArrow(childNode)
    #'   if (!is.na(ie)){
    #'     rv <- pedge[ie]
    #'   }
    #'   return(rv)
    #' },

    #' #' @description
    #' #' Return the list of model variables associated with the node. The
    #' #' model variables may be associated with costs or probabilities.
    #' #' @return List of model variables. 
    #' get_modvars = function() {
    #'   # make a list of all private objects that may be associated with model variables
    #'   mv <- c(private$p)
    #'   # iterate objects and create list of model variables
    #'   mvlist <- list()
    #'   lapply(mv, FUN=function(v) {
    #'     if (inherits(v, what='ModVar')) {
    #'       mvlist <<- c(mvlist, v)
    #'     }
    #'   })
    #'   # return list of model variables
    #'   return(mvlist)
    #' }
    
  )
)
