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
    #' Find the siblings of a vertex in the arborescence.
    #' @param v Vertex to test.
    #' @return A (possibly empty) list of siblings. 
    siblings = function(v) {
      # list of siblings
      S <- list()
      # find parent (also checks if v is in the arborescence)
      P <- self$direct_predecessors(v)
      # if v is not the root, it and its siblings are the direct successors of p
      if (length(P)==1) {
        # get all children of the parent
        C <- self$direct_successors(P[[1]])
        # exclude self
        for (c in C) {
          if (!identical(c,v)) {
            S <- c(S,c)
          }
        }
      }
      # return the list of siblings
      return(S)
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
    #' Implements function POSITIONTREE (Walker, 1989) to
    #' determine the coordinates for each node in an arborescence.
    #' @return A numeric matrix with one row per node and two columns (x and y).
    #' The row number of each node in the matrix is the value given by
    #' the Graph::element_index() function.
    position_tree = function() {
      # globals for the algorithm
      LevelZeroPtr <- 0
      MaxDepth <- Inf
      SiblingSeparation <- 4
      SubtreeSeparation <- 4
      # create the coordinate matrix
      XY <- matrix(data=NA, nrow=self$order(), ncol=2, 
                   dimnames=list(NULL,c("x","y")))
      # prevnode list (max 'height' is order of graph)
      PREVNODE <- vector(mode="integer", length=self$order())
      # per-node arrays
      LEFTNEIGHBOR <- vector(mode="integer", length=self$order())
      MODIFIER <- vector(mode="numeric", length=self$order())
      PRELIM <- vector(mode="numeric", length=self$order())
      # initialize list of previous nodes at each level
      INITPREVNODELIST <- function() {
      }
      # get previous node at this level
      GETPREVNODEATLEVEL <- function(Level) {
        # Level is zero-based
        return(PREVNODE[Level+1])
      }
      # set an element in the list
      SETPREVNODEATLEVEL <- function(Level, iNode) {
        # Level is zero-based
        PREVNODE[Level+1] <<- iNode
      }
      # test if node is a leaf
      ISLEAF <- function(iNode) {
        v <- private$V[[iNode]]
        return(self$is_leaf(v))
      }
      # left size of each node
      LEFTSIZE <- function(iNode) {
        return(1)
      }
      # right size of each node
      RIGHTSIZE <- function(iNode) {
        return(1)
      }
      # mean node size
      MEANNODESIZE <- function(LeftNode, RightNode) {
        NodeSize <- 0
        NodeSize <- NodeSize + RIGHTSIZE(LeftNode)
        NodeSize <- NodeSize + LEFTSIZE(RightNode)
        return(NodeSize)
      }
      # test if a node has a left sibling
      HASLEFTSIBLING <- function(iNode) {
        v <- private$V[[iNode]]
        S <- self$siblings(v)
        iS <- sapply(S, FUN=function(s){return(self$element_index(s))})
        rb <- any(iS<iNode)
        return(rb)
      }
      # find the node's closest sibling on the left
      LEFTSIBLING <- function(iNode) {
        rn <- NA
        v <- private$V[[iNode]]
        S <- self$siblings(v)
        iS <- sapply(S, FUN=function(s){return(self$element_index(s))})
        lS <- iS[which(iS<iNode)]
        if (any(iS<iNode)) {
          rn <- max(lS)
        }
        return(rn)
      }
      # test if a node has a right sibling
      HASRIGHTSIBLING <- function(iNode) {
        v <- private$V[[iNode]]
        S <- self$siblings(v)
        iS <- sapply(S, FUN=function(s){return(self$element_index(s))})
        rb <- any(iS>iNode)
        return(rb)
      }
      # find the node's closest sibling on the right
      RIGHTSIBLING <- function(iNode) {
        rn <- NA
        v <- private$V[[iNode]]
        S <- self$siblings(v)
        iS <- sapply(S, FUN=function(s){return(self$element_index(s))})
        rS <- iS[which(iS>iNode)]
        if (any(iS>iNode)) {
          rn <- min(rS)
        }
        return(rn)
      }
      # parent of the node
      PARENT <- function(iNode) {
        rn <- NA
        v <- private$V[[iNode]]
        P <- self$direct_predecessors(v)
        if (length(P)==1) {
          rn <- self$element_index(P[[1]])
        }
        return(rn)
      }
      # find the first child of iNode
      FIRSTCHILD <- function(iNode) {
        rn <- NA
        v <- private$V[[iNode]]
        C <- self$direct_successors(v)
        iC <- sapply(C, FUN=function(c){return(self$element_index(c))})
        if (length(iC) > 0) {
          rn <- min(iC)
        }
        return(rn)
      }
      # clean up small sibling subtrees      
      APPORTION <- function(iNode, Level) {
        Leftmost <- FIRSTCHILD(iNode)
        Neighbor <- LEFTNEIGHBOR[Leftmost]
        CompareDepth <- 1
        DepthToStop <- MaxDepth - Level
        while (Leftmost != 0 && Neighbor != 0 && CompareDepth <= DepthToStop) {
          # Compute the location of Leftmost and where it should 
          # be with respect to Neighbor.
          LeftModsum <- 0
          RightModsum <- 0
          AncestorLeftmost <- Leftmost
          AncestorNeighbor <- Neighbor
          for (i in seq(0,CompareDepth)) {
            AncestorLeftmost <- PARENT(AncestorLeftmost)
            AncestorNeighbor <- PARENT(AncestorNeighbor)
            RightModsum <- RightModsum + MODIFIER[AncestorLeftmost]
            LeftModsum <- LeftModsum + MODIFIER[AncestorNeighbor]
          }
          # Find the MoveDistance, and apply it to Node's subtree.
          # Add appropriate portions to smaller interior subtrees.
          MoveDistance <- (PRELIM[Neighbor] +
                           LeftModsum +
                           SubtreeSeparation + 
                           MEANNODESIZE(Leftmost, Neighbor)) -
                          (PRELIM[Leftmost] + RightModsum)          
        }
      }
      # function for first postorder walk
      FIRSTWALK <- function(iNode, Level) {
        # Set the pointer to the previous node at this level.
        LEFTNEIGHBOR[iNode] <<- GETPREVNODEATLEVEL(Level) 
        SETPREVNODEATLEVEL(Level, iNode) # This is now the previous.
        MODIFIER[iNode] <<- 0   # Set the default modifier value. 
        if (ISLEAF(iNode) | Level==MaxDepth) {
          if (HASLEFTSIBLING(iNode)) {
            # Determine the preliminary x-coordinate based on: 
            #   the preliminary x-coordinate of the left sibling, 
            #   the separation between sibling nodes, and 
            #   the mean size of left sibling and current node.
            PRELIM[iNode] <<-  PRELIM[LEFTSIBLING(iNode)] + 
                               SiblingSeparation +
                               MEANNODESIZE(LEFTSIBLING(iNode), iNode)
          } else {
           # No sibling on the left to worry about.
           PRELIM[iNode] <<- 0
          }
        } else {
          # This Node is not a leaf, so call this procedure 
          # recursively for each of its offspring.
          Leftmost <- FIRSTCHILD(iNode)
          Rightmost <- FIRSTCHILD(iNode)
          FIRSTWALK(Leftmost, Level + 1)
          while (HASRIGHTSIBLING(Rightmost)) {
            Rightmost <- RIGHTSIBLING(Rightmost)
            FIRSTWALK(Rightmost, Level + 1 ) 
          }
          Midpoint <- (PRELIM[Leftmost] + PRELIM[Rightmost]) / 2
          if (HASLEFTSIBLING(iNode)) {
            PRELIM[iNode] <<- PRELIM[LEFTSIBLING(iNode)] + 
                              SiblingSeparation + 
                              MEANNODESIZE(LEFTSIBLING(iNode), iNode)
            MODIFIER[iNode] <<- PRELIM[iNode] - Midpoint
            APPORTION(iNode, Level) 
          } else {
            PRELIM[iNode] <<- Midpoint          
          } 
        }
        return
      }
      # main function
      INITPREVNODELIST()
      iRoot <- self$element_index(self$root())
      FIRSTWALK(iRoot,0)
      
      # return the coordinate matrix
      return(XY)
    }
    
  )
  
)
