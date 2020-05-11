#' @title 
#' Node
#' 
#' @description
#' An R6 class to represent a node in a decision tree
#' 
#' @details 
#' Base class to represent a single node in a decision tree. Non subclassed
#' nodes are not expected to be created as model objects. Document Object
#' Model (DOM) names are used for node methods as far as possibe.

#' @docType class
#' @author Andrew Sims \email{andrew.sims5@@nhs.net}
#' @export
#' 
Node <- R6::R6Class(
  classname = "Node",
  private = list(
    
    # field list of Edge objects linking to child objects
    edges = 'list',
    
    # description
    # Add an edge linking to a child node
    # return 
    # An updated Node object
    addEdge = function(edge) {
      if (inherits(x=edge, what='Edge')) {
        private$edges <- c(private$edges, edge)
      }
      else {
        stop('Argument to Node::addEdge must inherit from Edge')
      }
      return(invisible(self))
    },
    
    # description
    # find index of edge connected to a given child node
    # param childNode child node to which to find edge
    # return index of edge, or NA if no link to that child
    whichEdge = function(childNode) {
      re <- NA
      for (i in 1:length(private$edges)) {
        e <- private$edges[[i]]
        toNode <- e$getToNode()
        if (toNode$isSameNode(childNode)) {
          re <- i
          break
        }
      }
      return(re)
    }
  ),
  
  public = list(
    
    #' @description
    #' Create new Node object.
    #' @return A new Node object.
    initialize = function() {
      private$edges <- list()
      return(invisible(self))
    },
    
    #' @description
    #' Does the node have any child nodes? (DOM-style)
    #' @return 
    #' TRUE if node has children, FALSE if not
    hasChildNodes = function() {
      return(length(private$edges)>0)
    },
    
    #' @description
    #' Return list of child nodes (DOM-style)
    #' @return 
    #' list of child Nodes
    childNodes = function() {
      children = list()
      for (e in private$edges) {
        children <- c(children, e$getToNode())
      }
      return(children)
    },

    #' @description 
    #' Return list of descendent nodes.
    #' @return List of descendent nodes, including self.
    descendantNodes = function() {
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
      toLeaf(self)
      return(nodes)
    },

    #' @description
    #' Is this node the same as the argument? (DOM-style)
    #' @param otherNode node to compare with this one
    #' @return TRUE if `otherNode` is also this one
    isSameNode = function(otherNode) {
      return(identical(self, otherNode)) 
    },
    
    #' @description
    #' node type (DOM-style)
    #' @return 
    #' Node class, as character string
    nodeType = function() {
      c <- class(self)[1]
      return(c)
    },
    
    #' @description
    #' Trace and list all pathways ending on leaf nodes which start
    #' with this node.
    #' @param choice Name of choice. All pathways are returned if NA.
    #' @return A list of Path objects. Each member of the list is a
    #' path from this node to a leaf, limited to those associated
    #' with choice, if defined.
    getPathways = function(choice=NA) {
      path <- list()
      rc <- list()
      toLeaf <- function(node) {
        # push current node to path
        path[[length(path)+1]] <<- node
        # leaf reached; store the path
        if (node$hasChildNodes()) {
          # process child nodes
          for (child in node$childNodes()) {
            toLeaf(child)
          }
        }
        else {
          p <- Path$new(path)
          if (!is.na(choice)) {
            if (p$getChoice()==choice) {
              rc[[length(rc)+1]] <<- p
            }
          }
          else {
            rc[[length(rc)+1]] <<- p
          }
        }
        # pop current node from path
        path <<- path[1:(length(path)-1)]
      }
      toLeaf(self)
      return(rc)
    },
    
    #' @description 
    #' Return label of edge which links to specified child node
    #' @param childNode child node to which find label of linking edge
    #' @return label as character string
    getLabel = function(childNode) {
      rv <- NA
      ie <- private$whichEdge(childNode)
      if (!is.na(ie)){
        edge <- private$edges[[ie]]
        rv <- edge$getLabel()
      }
      return(rv)
    },
    
    #' @description
    #' Function to return the conditional probability of the edge which links to
    #' the specified child node
    #' @param childNode child node to which to find probability of linking edge 
    #' @return numerical value of probability
    getP = function(childNode) {
      rv <- 0
      ie <- private$whichEdge(childNode)
      if (!is.na(ie)){
        edge <- private$edges[[ie]]
        rv <- edge$getP()
      }
      return(rv)
    },
    
    #' @description
    #' function to return the utility associated with the node
    #' @return 
    #' Utility, numeric
    getUtility = function() {
      return(NA)
    },
    
    #' @description
    #' Function to return the cost of the edge which links to the specified child node
    #' @param childNode child node to identify edge with associated cost of traversal
    #' @return Cost, numerical value
    getCost = function(childNode) {
      rv <- 0
      ie <- private$whichEdge(childNode)
      if (!is.na(ie)){
        edge <- private$edges[[ie]]
        rv <- edge$getCost()
      }
      return(rv)
    },
    
    #' @description 
    #' Function to return a list of model variables associated with this node.
    #' @return 
    #' List of model variables associated with this node.
    getModelVariables = function() {
      return(list())
    },
    
    #' @description
    #' Tabulate all model variables associated with this node.
    #' @param include.descendants If TRUE, model variables associated
    #' with this node and its descendants are tabulated; otherwise only
    #' the ones that are associated with this node.
    #' @param include.operands If TRUE, recursively add model variables which are
    #' included in expressions in ExpressionModelVariables. Default is
    #' FALSE.
    #' @return Data frame with one row per model variable, as follows:
    #' \describe{
    #' \item{Label}{The label given to the variable on creation.}
    #' \item{Description}{As given at initiialization.}
    #' \item{Units}{Units of the variable.}
    #' \item{Distribution}{Either the uncertainty distribution, if
    #' it is a regular model variable, or the expression used to create it,
    #' if it is an ExpressionModelVariable.}
    #' \item{Mean}{Expected value.}
    #' \item{SD}{Standard deviation.}
    #' \item{Q2.5}{2.5% quantile.}
    #' \item{Q97.5}{97.5% quantile.}
    #' \item{Qhat}{Asterisk (*) if the quantiles and SD have been estimated
    #' by random sampling.}
    #' }
    tabulateModelVariables = function(include.descendants=FALSE, 
                                      include.operands=FALSE) {
      # create list of nodes
      if (include.descendants) {
        nodes <- self$descendantNodes()
      } 
      else {
        nodes <- list(self)
      }
      # list model variables associated with these nodes
      mvlist <- list()
      sapply(nodes, FUN=function(n) {
        mv <- n$getModelVariables()
        if (length(mv) > 0) {
          mvlist <<- c(mvlist, unlist(mv))
        }
      })
      # tabulate the model variables
      DF <- do.call(
        'rbind', 
        lapply(mvlist, FUN=function(x){x$tabulate(include.operands)})
      )
      DF <- DF[!duplicated(DF),]
      # order the table
      if (nrow(DF) > 0) {
        DF <- DF[order(DF$Label),]
      }
      # return the tabulated variables
      return(DF)
    },
    
    #' @description 
    #' Sample the model variables associated with the node.
    #' @param expected if TRUE, use the expected value of the model variables in
    #'        the node; otherwise sample from their uncertainty distributions.
    #' @return Updated Node object
    sampleModelVariables = function(expected=FALSE) {
      return(invisible(self))
    },
    
    #' @description 
    #' Update the values on the edges associated with the node.
    #' @return Updated Node object
    updateEdges = function() {
      return(invisible(self))
    }
  )
)
