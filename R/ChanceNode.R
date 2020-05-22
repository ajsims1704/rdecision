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
#' @author Andrew Sims \email{andrew.sims5@@nhs.net}
#' @export
#' 
ChanceNode <- R6::R6Class(
  classname = "ChanceNode",
  inherit = Node,
  private = list(

    # private fields
    costs = 'list',
    p = "list",
    ptype = 'character'
    
  ),
  public = list(
    
    #' @description
    #' Create a new ChanceNode object
    #' @param children a list of \eqn{k \ge 2} Nodes which will be the children
    #'        of this node.
    #' @param edgelabels a list of \eqn{k} character strings to label each edge 
    #'        (branch) leaving the `ChanceNode`, given in the same order as
    #'        `children`.
    #' @param costs A list of \eqn{k} costs associated with each edge (branch) 
    #'        leaving the `ChanceNode`. Each element may be of type `numeric` or
    #'        `ModVar`; given in the same order as `children`.
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
    initialize = function(children, edgelabels, costs, p, ptype='auto') {

      ## ensure base class fields are initialized
      super$initialize()
      
      ## check child nodes
      if (length(children) < 2) {
        stop("ChanceNode$new: `children` must contain at least two objects of class `Node`.")
      }
      sapply(children, function(x) {
        if (!inherits(x, what="Node")){
          stop("ChanceNode$new: Each element in `children` must be of class `Node`")
        }
      })

      ## check edge labels
      if (length(edgelabels) != length(children)) {
        stop("ChanceNode$new: `edgelabels` must contain the same number of objects as `children`.")
      }
      sapply(edgelabels, function(x) {
        if (!is.character(x)){
          stop("ChanceNode$new: Each element in `edgelabels` must be of class `character`.")
        }
      })

      ## add edges to this node, using equal probability and zero cost per
      ## branch to ensure initialization of edge
      for (i in 1:length(children)) {
        pFlat <- 1/length(children)
        edge <- Edge$new(self, children[[i]], edgelabels[[i]], cost=0, p=pFlat)
        private$addEdge(edge)
      }

      ## check and store costs
      if (length(costs) != length(children)) {
        stop("ChanceNode$new: `costs` must contain the same number of objects as `children`.")
      }
      sapply(costs, function(x) {
        if (is.numeric(x)) {
        }
        else if (inherits(x, what='ModVar')) {
        }
        else {
          stop("ChanceNode$new: Each element in `costs` must be of class `numeric` or 'ModVar`")
        }
      })
      private$costs <- costs

      ## set ptype
      if (!is.character(ptype)) {
        stop("ChanceNode$new: `ptype`` must be of class `character`")
      }
      else {
        private$ptype <- ptype  
      }

      ## check and store p values
      
      # count each type of member of p
      k <- length(p)
      nna <- sum(is.na(p))
      nnu <- sum(sapply(p, is.numeric))
      nmv <- sum(sapply(p, FUN=function(e){return(inherits(e, what='ModVar'))}))
      
      # switch on ptype 
      if (ptype == 'numeric') {
        if (k != length(children)) {
          stop('ChanceNode$new: `p` must contain the same number of elements as children')
        }
        if (nnu != k) {
          stop('ChanceNode$new: all elements of `p` must be of type `numeric` for ptype=numeric')
        }
        private$p <- p
      }
      else if (ptype == 'MV') {
        if (k != length(children)) {
          stop('ChanceNode$new: `p` must contain the same number of elements as children')
        }
        if (nna != 1) {
          stop("ChanceNode$new: one element of p must be NA for `ptype='MVE`.")
        }
        if (nmv < 1) {
          stop("ChanceNode$new: at least one element of p must be a ModVar for `ptype=MVE`.")
        }
        if ((nna+nmv+nnu) != k) {
          stop("ChanceNode$new: all elements of `p` must be of type `numeric`, `ModVar` or `NA` for `ptype=MVE`")
        }
        warning("ChanceNode$new: `ptype='MV'` may lead to p values out of range [0,1].")
        private$p <- p
      }
      else if (ptype == 'Beta') {
        stop("ChanceNode$new: `ptype=Beta` not yet implemented")
      }
      else if (ptype == 'Dirichlet') {
        stop("ChanceNode$new: `ptype=Dirichlet` not yet implemented")
      }
      else if (ptype == "auto") {
        if (nnu == k) {
          private$ptype <- 'numeric'
          private$p <- p
        }
        else if ( (nna==1) && (nmv>=1) & ((nna+nmv+nnu)==k) ) {
          private$ptype <- 'MV'
          private$p <- p
          warning("ChanceNode$new: 'ptype=\"MV\"' may cause p values outside [0,1].",
                  call.=FALSE)
        }
        else {
          stop("ChanceNode$new: cannot guess `ptype` from supplied p for `ptype=auto`.")
        }
      }
      # anything else is illegal
      else {
        stop("ChanceNode$new: `ptype` must be one of 'numeric', 'MV', 'Beta' or 'Dirichlet'")
      }

      # update numeric edge values
      self$updateEdges()
    },

    #' @description
    #' Return the list of model variables associated with the node. The
    #' model variables may be associated with costs or probabilities.
    #' @return List of model variables. 
    get_modvars = function() {
      # make a list of all private objects that may be associated with model variables
      mv <- c(private$p, private$costs)
      # iterate objects and create list of model variables
      mvlist <- list()
      lapply(mv, FUN=function(v) {
        if (inherits(v, what='ModVar')) {
          mvlist <<- c(mvlist, v)
        }
      })
      # return list of model variables
      return(mvlist)
    },
    
    #' @description 
    #' Sample all model variables in this node and update edges.
    #' @param expected If TRUE cause each model variable to return its expected
    #'        value at the next call to `value()`. If FALSE each model variable
    #'        will return the sampled value. Default is FALSE.
    #' @return An updated ChanceNode object.
    sample_modvars = function(expected=F) {
      # get the model variables associated with this node
      mvlist <- self$get_modvars()
      # sample them
      sapply(mvlist, FUN=function(mv) {
        mv$sample(expected)
      })
      # return reference to updated node
      return(invisible(self))  
    },
    
    #' @description Update numerical values in edges.
    #' @return An updated object.
    updateEdges = function() {
      # update costs
      for (i in 1:length(private$edges)) {
        edge <- private$edges[[i]]
        cost <- private$costs[[i]]
        if (inherits(cost, what='ModVar')) {
          c <- cost$value()
          edge$setCost(c)
        }
        else if (is.numeric(cost)) {
          edge$setCost(cost)
        }
        else {
          stop("Edge$setEdgeCosts: cost must be of type `ModVar` or `numeric`")
        }
      }  
      # update probabilities
      if (private$ptype == 'numeric') {
        for (i in 1:length(private$edges)) {
          edge <- private$edges[[i]]
          edge$setP(private$p[[i]])
        }
      }
      else if (private$ptype == 'MV') {
        pedge <- vector('numeric', length=length(private$edges))
        for (i in 1:length(private$edges)) {
          edge <- private$edges[[i]]
          p <- private$p[[i]]
          if (inherits(p, what='ModVar')) {
             v <- p$value()
             pedge[i] <- v
          }
          else {
            pedge[i] <- p
          }
        }
        pedge[is.na(pedge)] <- 1 - sum(pedge, na.rm=T)
        for (i in 1:length(private$edges)) {
          edge <- private$edges[[i]]
          edge$setP(pedge[i])
        }
      }
      # return updated Node object
      return(invisible(self))
    }

  )
)
