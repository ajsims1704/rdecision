#' @title 
#' Arborescence
#' 
#' @description
#' An R6 class to represent an arborescence (rooted directed tree).
#' 
#' @details 
#' Class to encapsulate a directed rooted tree specialization of a digraph.
#' An arboresecence must be a directed tree with exactly one root and the
#' directed paths from the root must be unique.
#' 
#' @references 
#' \itemize{
#'   \item Walker, John Q II. A A node-positioning algorithm for general trees.
#'   University of North Carolina Technical Report TR 89-034, 1989.
#'   \item Tilford, J and Reingold, E. Tidier Drawings of Trees. IEEE
#'    Transactions on Software Engineering. 1981;7: 223–228.
#'     doi:10.1109/TSE.1981.234519
#' }
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Arborescence <- R6::R6Class(
  classname = "Arborescence",
  inherit = Digraph,
  private = list(
  ),
  public = list(
    
    #' @description 
    #' Create a new Arborescence object from sets of nodes and edges. 
    #' @param V A list of Nodes.
    #' @param A A list of Arrows.
    #' @return An Arborescence object.
    initialize = function(V,A) {
      # initialize the base Digraph class (checks V, A)
      super$initialize(V,A)
      # check that the graph is an arborescence
      if (!self$is_arborescence()) {
        rlang::abort("The graph must be an arborescence", class="not_arborescence")
      }
      # return new Arborescence object
      return(invisible(self))
    },
    
    #' @description 
    #' Test whether the given node is a parent (has child nodes).
    #' @param v Node to test
    #' @return TRUE if v has one or more child nodes, FALSE otherwise.
    is_parent = function(v) {
      # check if this vertex has direct successors (also checks v)
      C <- self$direct_successors(v)
      return(length(C)>0)
    },
    
    #' @description 
    #' Test whether the given node is a leaf. In an arborescence,
    #' \code{is_parent()} and \code{is_leaf()} will be mutually exclusive.
    #' @param v Vertex to test.
    #' @return TRUE if v has no child nodes, FALSE otherwise. 
    is_leaf = function(v) {
      # check if this vertex has direct successors (also checks v)
      C <- self$direct_successors(v)
      return(length(C)==0)
    },
  
    #' @description 
    #' Find the root vertex of the arborescence.
    #' @return The root vertex.
    root = function() {
      # vertex with no incoming edges (only one, checked in initialize)
      u <- which(apply(private$B, MARGIN=1, function(r){!any(r>0)}),arr.ind=TRUE)
      return(private$V[[u]])
    },
    
    #' @description 
    #' Find all directed paths from the root of the tree to the leaves.
    #' @return A list of ordered node lists. 
    root_to_leaf_paths = function() {
      # Find the root
      r <- self$root()
      P <- list()
      sapply(private$V, function(v){
        if (self$is_leaf(v)) {
          pp <- self$paths(r,v)
          P[[length(P)+1]] <<- pp[[1]]    
        }
      })
      # return the paths
      return(P)
    },
    
    #' @description
    #' This function implements function POSITIONTREE (Walker, 1989) to
    #' determine the coordinates for each node in an arborescence.
    #' @return A numeric matrix with one row per node and two columns (x and y).
    #' The row number of each node in the matrix is the value given by
    #' the Graph::element_index() function.
    position_tree = function() {
      # create the coordinate matrix
      XY <- matrix(data=NA, nrow=self$order(), ncol=2, 
                   dimnames=list(NULL,c("x","y")))
      PREVNODE <- vector(mode="integer", length=self$order())
      # initialize list of previous nodes at each level
      INITPREVNODELIST <- function() {
        for (i in 1:length(PREVNODE)) {
          PREVNODE[i] <<- 0
        }  
      }
      # function for first postorder walk
      FIRSTWALK <- function(inode, level) {
        
      }
      # main function
      INITPREVNODELIST()
      FIRSTWALK(self$element_index(self$root()),0)
      
      # return the coordinate matrix
      return(XY)
    }
    
  )
  
)
