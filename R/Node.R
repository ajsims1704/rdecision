#' @title 
#' Node
#' 
#' @description
#' An R6 class to represent a node in a decision tree
#' 
#' @details 
#' Base class to represent a single node in a decision tree. Objects of base
#' class Node are not expected to be created as model objects. Document Object
#' Model (DOM) names are used for node methods as far as possibe.
#'
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Node <- R6::R6Class(
  classname = "Node",
  private = list(
    
    # field list of Edge objects linking to child objects
    edges = NULL,
    
    # description
    # Add an edge linking to a child node
    # return 
    # An updated Node object
    addEdge = function(edge) {
      if (inherits(x=edge, what='Edge')) {
        private$edges <- c(private$edges, edge)
      }
      else {
        rlang::abort("Argument edge must inherit from Edge",
                     class="non-Edge_edge")
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
        toNode <- e$get_target()
        if (toNode$is_same_node(childNode)) {
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
    has_child_nodes = function() {
      return(length(private$edges)>0)
    },
    
    #' @description
    #' Return list of child nodes (DOM-style)
    #' @return 
    #' list of child Nodes
    child_nodes = function() {
      children = list()
      for (e in private$edges) {
        children <- c(children, e$get_target())
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
        if (node$has_child_nodes()) {
          for (child in node$child_nodes()) {
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
    is_same_node = function(otherNode) {
      return(identical(self, otherNode)) 
    },
    
    #' @description
    #' node type (DOM-style)
    #' @return 
    #' Node class, as character string
    node_type = function() {
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
        if (node$has_child_nodes()) {
          # process child nodes
          for (child in node$child_nodes()) {
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
    get_label = function(childNode) {
      rv <- NA
      ie <- private$whichEdge(childNode)
      if (!is.na(ie)){
        edge <- private$edges[[ie]]
        rv <- edge$get_label()
      }
      return(rv)
    },
    
    #' @description 
    #' Function to return a list of model variables associated with this node.
    #' @return 
    #' List of model variables associated with this node.
    get_modvars = function() {
      return(list())
    },
    
    #' @description
    #' Tabulate all model variables associated with this node.
    #' @param include.descendants If TRUE, model variables associated
    #' with this node and its descendants are tabulated; otherwise only
    #' the ones that are associated with this node.
    #' @param include.operands If TRUE, recursively add model variables which are
    #' included in expressions in ExprModVars. Default is
    #' FALSE.
    #' @return Data frame with one row per model variable, as follows:
    #' \describe{
    #' \item{Label}{The label given to the variable on creation.}
    #' \item{Description}{As given at initialization.}
    #' \item{Units}{Units of the variable.}
    #' \item{Distribution}{The uncertainty distribution or an expression.} 
    #' \item{Mean}{Expected value.}
    #' \item{SD}{Standard deviation.}
    #' \item{Q2.5}{p=0.025 quantile.}
    #' \item{Q97.5}{p=0.975 quantile.}
    #' \item{Qhat}{Asterisk if the quantiles and SD were estimated by random sampling.}
    #' }
    tabulate_modvars = function(include.descendants=FALSE, 
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
        mv <- n$get_modvars()
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
    sample_modvars = function(expected=FALSE) {
      # get the model variables associated with this node
      mvlist <- self$get_modvars()
      # sample them
      sapply(mvlist, FUN=function(mv) {
        mv$sample(expected)
      })
      return(invisible(self))
    }
    
  )
)
