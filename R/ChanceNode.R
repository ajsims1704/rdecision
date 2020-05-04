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
    ptype = 'character',
    
    # description Set numerical cost values in edges.
    # return An updated object.
    setEdgeCosts = function() {
      for (i in 1:length(private$edges)) {
        edge <- private$edges[[i]]
        cost <- private$costs[[i]]
        if (inherits(cost, what='ModelVariable')) {
          c <- cost$value()
          edge$setCost(c)
        }
        else if (is.numeric(cost)) {
          edge$setCost(cost)
        }
        else {
          stop("Edge$setEdgeCosts: cost must be of type `ModelVariable` or `numeric`")
        }
      }  
      return(invisible(self))
    },
    
    # description Set numerical p values in edges
    # return An updated ChanceNode object
    setEdgeP = function() {
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
          if (inherits(p, what='ModelVariable')) {
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
      return(invisible(self))
    }
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
    #'        `ModelVariable`; given in the same order as `children`.
    #' @param ptype a character string taking one of four possible
    #'        values to define how the \code{p} argument is defined:
    #'        'numeric', 'MVE', 'Beta' or 'Dirichlet'. 
    #' @param p a list of probabilities associated with \eqn{k} branches.
    #'        There are four possible configurations of the list 
    #'        based on the \code{ptype} argument.
    #' \describe{
    #' \item{'numeric'}{\eqn{k} numeric values; the simplest case in which the   
    #'           probabilities are certain. Supplied values should add
    #'           to unity and be given in the same order as `children`.}
    #' \item{'MV'}{\eqn{k-1} `ModelVariable`s or `numeric` elements and
    #'       a single `numeric(NA)`, give in the same order as `children`. The 
    #'       single NA will be replaced on evaluation of the model variable
    #'       expressions by a value to ensure the sum of probabilities
    #'       is unity. This option is not recommended, because there is
    #'       a chance that during sampling, individual branch probabilities
    #'       may be less than zero, or that the sum of the sampled model variable
    #'       expressions may exceed 1.}
    #' \item{'Beta'}{One `BetaModelVariable` and one NA. Used for \eqn{k=2}. The 
    #'       element defined as NA will be replaced by one minus the sampled 
    #'       value of the supplied beta distribution.}
    #' \item{'Dirichlet'}{One DirichletModelVariable, with \eqn{k} parameters,
    #'       given in the same order as `children`.}
    #' }
    #' @return A new `ChanceNode` object
    initialize = function(children, edgelabels, costs, p, ptype='numeric') {

      # ensure base class fields are initialized
      super$initialize()
      
      # check child nodes
      if (length(children) < 2) {
        stop("ChanceNode$new: `children` must contain at least two objects of class `Node`.")
      }
      sapply(children, function(x) {
        if (!inherits(x, what="Node")){
          stop("ChanceNode$new: Each element in `children` must be of class `Node`")
        }
      })

      # check edge labels
      if (length(edgelabels) != length(children)) {
        stop("ChanceNode$new: `edgelabels` must contain the same number of objects as `children`.")
      }
      sapply(edgelabels, function(x) {
        if (!is.character(x)){
          stop("ChanceNode$new: Each element in `edgelabels` must be of class `character`.")
        }
      })

      # add edges to this node, using equal probability and zero cost per
      # branch to ensure initialization of edge
      for (i in 1:length(children)) {
        pFlat <- 1/length(children)
        edge <- Edge$new(self, children[[i]], edgelabels[[i]], cost=0, p=pFlat)
        private$addEdge(edge)
      }

      # check and store costs
      if (length(costs) != length(children)) {
        stop("ChanceNode$new: `costs` must contain the same number of objects as `children`.")
      }
      sapply(costs, function(x) {
        if (is.numeric(x)) {
        }
        else if (inherits(x, what='ModelVariable')) {
        }
        else {
          stop("ChanceNode$new: Each element in `costs` must be of class `numeric` or 'ModelVariable`")
        }
      })
      private$costs <- costs

      # set costs for each edge
      private$setEdgeCosts()
      
      # set ptype
      if (!is.character(ptype)) {
        stop("ChanceNode$new: `ptype`` must be of class `character`")
      }
      else {
        private$ptype <- ptype  
      }

      # check and store p
      if (ptype == 'numeric') {
        if (length(p) != length(children)) {
          stop('ChanceNode$new: `p`` must contain the same number of elements as children')
        }
        sapply(p, FUN=function(x){
          if (!is.numeric(x)) {
            stop('ChanceNode$new: all elements of `p` must be of type `numeric`')
          }
        })
        private$p <- p
      }
      else if (ptype == 'MV') {
        if (length(p) != length(children)) {
          stop('ChanceNode$new: `p`` must contain the same number of elements as children')
        }
        nna <- 0
        sapply(p, FUN=function(x){
          if (is.numeric(x)) {
            if (is.na(x)) {
              nna <<- nna + 1
            }
          }
          else if (inherits(x, what='ModelVariable')) {
          }
          else {
            stop("ChanceNode$new: all elements of `p` must be of type `numeric`, `ModelVariable` or `NA`")
          }
        })
        if (nna != 1) {
          stop("ChanceNode$new: one element of p must be numeric(NA) for `ptype='MVE`")
        }
        private$p <- p
      }
      else if (ptype == 'Beta') {
        stop("ChanceNode$new: `ptype=Beta` not yet implemented")
      }
      else if (ptype == 'Dirichlet') {
        stop("ChanceNode$new: `ptype=Dirichlet` not yet implemented")
      }
      # anything else is illegal
      else {
        stop("ChanceNode$new: `ptype` must be one of 'numeric', 'MV', 'Beta' or 'Dirichlet'")
      }

      # set p for each edge
      private$setEdgeP()
    },

    #' @description
    #' Return the list of model variables associated with the node. The
    #' model variables may be associated with costs or probabilities.
    #' @return List of model variables. 
    getModelVariables = function() {
      # make a list of all private objects that may be associated with model variables
      objects <- c(private$p, private$costs)
      # iterate objects and create list of model variables
      mvlist <- list()
      lapply(objects, FUN=function(o) {
        if (inherits(o, what='ModelVariable')==T) {
          mvlist <<- c(mvlist, o)
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
    sample = function(expected=F) {
      # get the model variables associated with this node
      mvlist <- self$getModelVariables()
      # sample them
      sapply(mvlist, FUN=function(mv) {
        mv$sample(expected)
      })
      # update edges
      private$setEdgeCosts()
      private$setEdgeP()
      # return reference to updated node
      return(invisible(self))  
    }

  )
)
