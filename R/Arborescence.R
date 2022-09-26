#' @title A rooted directed tree
#' @description An R6 class representing an \dfn{arborescence} (a rooted 
#' directed tree).
#' @details Class to encapsulate a directed rooted tree specialization of a 
#' digraph. An arborescence is a directed tree with exactly one root and 
#' unique directed paths from the root. Inherits from class \code{Digraph}.
#' @references{
#'   Walker, John Q II. A A node-positioning algorithm for general trees.
#'   University of North Carolina Technical Report \acronym{TR} 89-034, 1989.
#' }
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
Arborescence <- R6::R6Class(
  classname = "Arborescence",
  inherit = Digraph,
  private = list(
    # arrays needed by postree
    PREVNODE = NULL,
    PRELIM = NULL,
    MODIFIER = NULL,
    LEFTNEIGHBOR = NULL,
    XCOORD = NULL,
    YCOORD = NULL
  ),
  public = list(
    
    #' @description Create a new \code{Arborescence} object from sets of nodes
    #' and edges. 
    #' @param V A list of Nodes.
    #' @param A A list of Arrows.
    #' @return An \code{Arborescence} object.
    initialize = function(V,A) {
      # initialize the base Digraph class (checks V, A)
      super$initialize(V,A)
      # check that the graph is an arborescence
      abortifnot(self$is_arborescence(),
        message = "The graph must be an arborescence", 
        class = "not_arborescence"
      )
      # return new Arborescence object
      return(invisible(self))
    },
    
    #' @description Test whether the given node is a parent (has child nodes).
    #' @param v Node to test
    #' @return TRUE if v has one or more child nodes, FALSE otherwise.
    is_parent = function(v) {
      # check if this vertex has direct successors (also checks v)
      C <- self$direct_successors(v)
      return(length(C) > 0L)
    },
    
    #' @description Test whether the given node is a leaf. In an arborescence,
    #' \code{is_parent()} and \code{is_leaf()} are mutually exclusive.
    #' @param v Vertex to test.
    #' @return TRUE if v has no child nodes, FALSE otherwise. 
    is_leaf = function(v) {
      # check if this vertex has direct successors (also checks v)
      C <- self$direct_successors(v)
      return(length(C) == 0L)
    },
  
    #' @description Find the root vertex of the arborescence.
    #' @return The root vertex.
    root = function() {
      # vertex with no incoming edges (only one, checked in initialize)
      B <- self$digraph_incidence_matrix()
      u <- which(
        apply(B, MARGIN = 1L, function(r) !any(r > 0L)), 
        arr.ind = TRUE
      )
      return(private$V[[u]])
    },

    #' @description Find the siblings of a vertex in the arborescence.
    #' @param v Vertex to test.
    #' @return A (possibly empty) list of siblings. 
    siblings = function(v) {
      # list of siblings
      S <- list()
      # find parent (also checks if v is in the arborescence)
      P <- self$direct_predecessors(v)
      # if v is not the root, it and its siblings are the direct successors of p
      if (length(P) == 1L) {
        # get all children of the parent
        C <- self$direct_successors(P[[1L]])
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
    
    #' @description Find all directed paths from the root of the tree to the
    #' leaves.
    #' @return A list of ordered node lists. 
    root_to_leaf_paths = function() {
      # Find the root
      r <- self$root()
      P <- list()
      for (v in private$V) {
        if (self$is_leaf(v)) {
          pp <- self$paths(r,v)
          P[[length(P) + 1L]] <- pp[[1L]]    
        }
      }
      # return the paths
      return(P)
    },
    
    #' @description Implements function \verb{POSITIONTREE} (Walker, 1989) to
    #' determine the coordinates for each node in an arborescence.
    #' @param SiblingSeparation Distance in arbitrary units for the
    #' distance between siblings.
    #' @param SubtreeSeparation Distance in arbitrary units for the
    #' distance between neighbouring subtrees.
    #' @param LevelSeparation Distance in arbitrary units for the 
    #' separation between adjacent levels.
    #' @param RootOrientation Must be one of "NORTH", "SOUTH", "EAST", "WEST".
    #' Defined as per Walker (1989), but noting that Walker assumed that
    #' y increased down the page. Thus the meaning of NORTH and SOUTH are
    #' opposite to his, with the default (SOUTH) having the child nodes at
    #' positive y value and root at zero, as per his example (figure 12).
    #' @param MaxDepth The maximum depth (number of levels) to be drawn; if
    #' the tree exceeds this, an error will be raised.
    #' @return A data frame with one row per node and three columns (n, x
    #' and y) where \code{n} gives the node index given by the 
    #' \code{Graph::vertex_index()} function.
    # There were 3 bugs in the pseudo-code in the report, possibly corrected
    # in the later paper, indicated by ##DEBUG## in the code below. 
    postree = function(SiblingSeparation = 4.0, SubtreeSeparation = 4.0, 
                       LevelSeparation = 1.0, RootOrientation = "SOUTH",
                       MaxDepth = Inf) {
      # check input parameters
      abortifnot(is.numeric(SiblingSeparation),
        message = "'SiblingSeparation' must be numeric",
        class = "non-numeric_SiblingSeparation"
      )
      abortifnot(is.numeric(SubtreeSeparation),
        message = "'SubstreeSeparation' must be numeric",
        class = "non-numeric_SubtreeSeparation"
      )
      abortifnot(is.numeric(LevelSeparation),
        message = "'LevelSeparation' must be numeric",
        class = "non-numeric_LevelSeparation"
      )
      abortifnot(is.character(RootOrientation),
        message = "'RootOrientation' must be character",
        class = "non-character_RootOrientation"
      )
      abortifnot((RootOrientation %in% c("NORTH", "SOUTH", "EAST", "WEST")),
        message = "'RootOrientation' must be one of NORTH, SOUTH, EAST, WEST",
        class = "invalid_RootOrientation"
      )
      abortifnot(is.numeric(MaxDepth),
        message = "'MaxDepth' must be numeric",
        class = "invalid_MaxDepth"
      )
      # globals for the algorithm
      LevelZeroPtr <- 0L
      xTopAdjustment <- 0.0
      yTopAdjustment <- 0.0
      # prevnode list (max 'height' is order of graph)
      private$PREVNODE <- vector(mode="integer", length=self$order())
      # per-node arrays
      private$LEFTNEIGHBOR <- vector(mode="integer", length=self$order())
      private$MODIFIER <- vector(mode="numeric", length=self$order())
      private$PRELIM <- vector(mode="numeric", length=self$order())
      private$XCOORD <- vector(mode="numeric", length=self$order())
      private$YCOORD <- vector(mode="numeric", length=self$order())
      # initialize list of previous nodes at each level
      INITPREVNODELIST <- function() {
      }
      # get previous node at this level
      GETPREVNODEATLEVEL <- function(Level) {
        # Level is zero-based
        return(private$PREVNODE[Level + 1L])
      }
      # set an element in the list
      SETPREVNODEATLEVEL <- function(Level, iNode) {
        # Level is zero-based
        private$PREVNODE[Level + 1L] <- iNode
      }
      # test if node is a leaf
      ISLEAF <- function(iNode) {
        v <- private$V[[iNode]]
        return(self$is_leaf(v))
      }
      # left size of each node
      LEFTSIZE <- function(iNode) {
        return(1.0)
      }
      # right size of each node
      RIGHTSIZE <- function(iNode) {
        return(1.0)
      }
      # top size of each node (EAST/WEST trees)
      TOPSIZE <- function(iNode) {
        return(1.0)
      }
      # bottom size of each node (EAST/WEST trees)
      BOTTOMSIZE <- function(iNode) {
        return(1.0)
      }
      # mean node size
      MEANNODESIZE <- function(LeftNode, RightNode) {
        NodeSize <- 0.0
        if (RootOrientation %in% c("NORTH","SOUTH")) {
          NodeSize <- NodeSize + RIGHTSIZE(LeftNode)
          NodeSize <- NodeSize + LEFTSIZE(RightNode)
        } else {
          NodeSize <- NodeSize + TOPSIZE(LeftNode)
          NodeSize <- NodeSize + BOTTOMSIZE(RightNode)
        }
        return(NodeSize)
      }
      # test if a node has a left sibling
      HASLEFTSIBLING <- function(iNode) {
        v <- private$V[[iNode]]
        S <- self$siblings(v)
        iS <- vapply(S, FUN.VALUE = 1L, FUN=function(s) self$vertex_index(s))
        rb <- any(iS < iNode)
        return(rb)
      }
      # find the node's closest sibling on the left (0 if none)
      LEFTSIBLING <- function(iNode) {
        rn <- 0L
        v <- private$V[[iNode]]
        S <- self$siblings(v)
        iS <- vapply(S, FUN.VALUE = 1L, FUN=function(s) self$vertex_index(s))
        lS <- iS[which(iS < iNode)]
        if (any(iS < iNode)) {
          rn <- max(lS)
        }
        return(rn)
      }
      # test if a node has a right sibling 
      HASRIGHTSIBLING <- function(iNode) {
        v <- private$V[[iNode]]
        S <- self$siblings(v)
        iS <- vapply(S, FUN.VALUE = 1L, FUN=function(s) self$vertex_index(s))
        rb <- any(iS > iNode)
        return(rb)
      }
      # find the node's closest sibling on the right (0 if none)
      RIGHTSIBLING <- function(iNode) {
        rn <- 0L
        v <- private$V[[iNode]]
        S <- self$siblings(v)
        iS <- vapply(S, FUN.VALUE = 1L, FUN = function(s) self$vertex_index(s))
        rS <- iS[which(iS>iNode)]
        if (any(iS>iNode)) {
          rn <- min(rS)
        }
        return(rn)
      }
      # parent of the node (0 if none)
      PARENT <- function(iNode) {
        rn <- 0L
        v <- private$V[[iNode]]
        P <- self$direct_predecessors(v)
        if (length(P) == 1L) {
          rn <- self$vertex_index(P[[1L]])
        }
        return(rn)
      }
      # does the specified node have a child?
      HASCHILD <- function(iNode) {
        v <- private$V[[iNode]]
        C <- self$direct_successors(v)
        return(length(C) > 0L)
      }
      # find the first child of iNode (0 if none)
      FIRSTCHILD <- function(iNode) {
        rn <- 0L
        v <- private$V[[iNode]]
        C <- self$direct_successors(v)
        iC <- vapply(C, FUN.VALUE = 1L, FUN = function(c) self$vertex_index(c))
        if (length(iC) > 0L) {
          rn <- min(iC)
        }
        return(rn)
      }
      # Leftmost descendant of a node at a given depth
      GETLEFTMOST <- function(iNode, Level, Depth) {
        if (Level >= Depth) {
          return(iNode)
        } else if (ISLEAF(iNode)) {
          return(0L)
        } else {
          Rightmost <- FIRSTCHILD(iNode)
          Leftmost <- GETLEFTMOST(Rightmost, Level + 1L, Depth)
          # Do a postorder walk of the subtree below Node.
          while (Leftmost == 0L && HASRIGHTSIBLING(Rightmost)) {
            Rightmost <- RIGHTSIBLING(Rightmost)
            Leftmost <- GETLEFTMOST(Rightmost, Level + 1L, Depth)
          }
          return(Leftmost)
        }
      }
      # clean up small sibling subtrees      
      APPORTION <- function(iNode, Level) {
        Leftmost <- FIRSTCHILD(iNode)
        Neighbor <- private$LEFTNEIGHBOR[Leftmost]
        CompareDepth <- 1L
        DepthToStop <- MaxDepth - Level
        #
        while (Leftmost != 0L && Neighbor != 0L && 
               CompareDepth <= DepthToStop) {
          # Compute the location of Leftmost and where it should 
          # be with respect to Neighbor.
          LeftModsum <- 0.0
          RightModsum <- 0.0
          AncestorLeftmost <- Leftmost
          AncestorNeighbor <- Neighbor
          for (i in seq(0L, CompareDepth - 1L)) {
            AncestorLeftmost <- PARENT(AncestorLeftmost)
            AncestorNeighbor <- PARENT(AncestorNeighbor)
            RightModsum <- RightModsum + private$MODIFIER[AncestorLeftmost]
            LeftModsum <- LeftModsum + private$MODIFIER[AncestorNeighbor]
          }
          # Find the MoveDistance, and apply it to Node's subtree.
          # Add appropriate portions to smaller interior subtrees.
          MoveDistance <- (private$PRELIM[Neighbor] +
                           LeftModsum +
                           SubtreeSeparation + 
                           MEANNODESIZE(Leftmost, Neighbor)) -
                           (private$PRELIM[Leftmost] + RightModsum)  
          if (MoveDistance > 0.0) {
            # Count interior sibling subtrees in LeftSiblings
            TempPtr <- iNode
            LeftSiblings <- 0L
            while (TempPtr != 0L && TempPtr != AncestorNeighbor) {
              LeftSiblings <- LeftSiblings + 1L
              TempPtr <- LEFTSIBLING(TempPtr)
            } 
            if (TempPtr != 0L) {
              # Apply portions to appropriate left sibling subtrees.
              Portion <- MoveDistance / LeftSiblings
              TempPtr <- iNode
              while (TempPtr != AncestorNeighbor) { ##DEBUG - != not =##
                private$PRELIM[TempPtr] <- private$PRELIM[TempPtr] + 
                                           MoveDistance
                private$MODIFIER[TempPtr] <- private$MODIFIER[TempPtr] + 
                                             MoveDistance
                MoveDistance <- MoveDistance - Portion
                TempPtr <- LEFTSIBLING(TempPtr)
              }
            }
          } else {
            # Don't need to move anything--it needs to 
            # be done by an ancestor because 
            # AncestorNeighbor and AncestorLeftmost are 
            # not siblings of each other.
            return
          }
          # Determine the leftmost descendant of Node at the next 
          # lower level to compare its positioning against that of
          # its Neighbor
          CompareDepth <- CompareDepth + 1L
          if (ISLEAF(Leftmost)) {
            Leftmost <- GETLEFTMOST(iNode, 0L, CompareDepth)
          } else {
# nocov start
# in an arborescence the leftmost node in a subtree is always a leaf
            Leftmost <- FIRSTCHILD(Leftmost)
# nocov end
          }
          Neighbor <- private$LEFTNEIGHBOR[Leftmost] ##DEBUG - line missing##
        }
        return
      }
      # function for first postorder walk
      FIRSTWALK <- function(iNode, Level) {
        # Set the pointer to the previous node at this level.
        private$LEFTNEIGHBOR[iNode] <- GETPREVNODEATLEVEL(Level) 
        SETPREVNODEATLEVEL(Level, iNode) # This is now the previous.
        private$MODIFIER[iNode] <- 0.0   # Set the default modifier value. 
        if (ISLEAF(iNode) || Level==MaxDepth) {
          if (HASLEFTSIBLING(iNode)) {
            # Determine the preliminary x-coordinate based on: 
            #   the preliminary x-coordinate of the left sibling, 
            #   the separation between sibling nodes, and 
            #   the mean size of left sibling and current node.
            private$PRELIM[iNode] <- private$PRELIM[LEFTSIBLING(iNode)] + 
                                     SiblingSeparation +
                                     MEANNODESIZE(LEFTSIBLING(iNode), iNode)
          } else {
           # No sibling on the left to worry about.
           private$PRELIM[iNode] <- 0.0
          }
        } else {
          # This Node is not a leaf, so call this procedure 
          # recursively for each of its offspring.
          Leftmost <- FIRSTCHILD(iNode)
          Rightmost <- FIRSTCHILD(iNode)
          FIRSTWALK(Leftmost, Level + 1L)
          while (HASRIGHTSIBLING(Rightmost)) {
            Rightmost <- RIGHTSIBLING(Rightmost)
            FIRSTWALK(Rightmost, Level + 1L) 
          }
          Midpoint <- 
            (private$PRELIM[Leftmost] + private$PRELIM[Rightmost]) / 2.0
          if (HASLEFTSIBLING(iNode)) {
            private$PRELIM[iNode] <- private$PRELIM[LEFTSIBLING(iNode)] + 
                                     SiblingSeparation + 
                                     MEANNODESIZE(LEFTSIBLING(iNode), iNode)
            private$MODIFIER[iNode] <- private$PRELIM[iNode] - Midpoint
            APPORTION(iNode, Level) 
          } else {
            private$PRELIM[iNode] <- Midpoint          
          } 
        }
        return
      }
      # check that x and y are within extent of display device. It is
      # assumed that the tree will be scaled to the window, and this
      # implementation always returns TRUE.
      CHECKEXTENTSRANGE <- function(xValue, yValue) {
        return(TRUE)
      }
      # second walk
      SECONDWALK <- function(iNode, Level, Modsum) {
        if (Level <= MaxDepth) {
          if (RootOrientation == "NORTH") {
            xTemp <- xTopAdjustment + (private$PRELIM[iNode] + Modsum)
            yTemp <- yTopAdjustment - (Level * LevelSeparation)
          } else if (RootOrientation == "SOUTH") {
            xTemp <- xTopAdjustment + (private$PRELIM[iNode] + Modsum)
            yTemp <- yTopAdjustment + (Level * LevelSeparation)
          } else if (RootOrientation == "EAST") {
            xTemp <- xTopAdjustment + (Level * LevelSeparation)
            yTemp <- yTopAdjustment - (private$PRELIM[iNode] + Modsum)
          } else if (RootOrientation == "WEST") {
            xTemp <- xTopAdjustment - (Level * LevelSeparation)
            yTemp <- yTopAdjustment - (private$PRELIM[iNode] + Modsum)
          } 
          # Check to see that xTemp and yTemp are of the proper 
          # size for your application. 
          if (CHECKEXTENTSRANGE(xTemp, yTemp)) {
            private$XCOORD[iNode] <- xTemp
            private$YCOORD[iNode] <- yTemp
            Result <- TRUE
            if (HASCHILD(iNode)) {
              # Apply the Modifier value for this node to 
              # all its offspring. 
              Result <- SECONDWALK(FIRSTCHILD(iNode),
                                   Level + 1L,
                                   Modsum + private$MODIFIER[iNode])
            }
            if (Result==TRUE && HASRIGHTSIBLING(iNode)) {
                Result <- SECONDWALK(RIGHTSIBLING(iNode),
                                     Level,   ##DEBUG - not Level+1##
                                     Modsum)
            }
          } else {
            # Continuing would put the tree outside of the 
            # drawable extent's range.
# nocov start
# there are no limits on the drawable extent, so will never reach here
            Result <- FALSE
# nocov end
          }
        } else {
          # We are at a level deeper than that we want to draw. 
          Result <- FALSE
        }
        return(Result)
      }      
      # determine coordinates for each node in a tree
      POSITIONTREE <- function(iNode) {
        if (iNode != 0L) {
          # Initialize the list of previous nodes at each level.  
          INITPREVNODELIST()
          # Do the preliminary positioning with a postorder walk.
          FIRSTWALK(iNode, 0L)
          # Determine how to adjust all the nodes with respect to 
          # the location of the root. 
          if (RootOrientation %in% c("NORTH", "SOUTH")) {
            xTopAdjustment <- private$XCOORD[iNode] - private$PRELIM[iNode]
            yTopAdjustment <- private$YCOORD[iNode]
          } else {
            xTopAdjustment <- private$XCOORD[iNode] 
            yTopAdjustment <- private$YCOORD[iNode] + private$PRELIM[iNode]
          }
          # Do the final positioning with a preorder walk
          return(SECONDWALK(iNode, 0L, 0L))      
        } else {
          # Trivial: return TRUE if a null pointer was passed.
# nocov start
# an arborescence must have a root node, so never reach here
          return(TRUE)         
# nocov end
        }
      }
      # call Walker's main function
      iRoot <- self$vertex_index(self$root())
      rc <- POSITIONTREE(iRoot)
      if (!rc) {
        rlang::abort(
          "Error in POSITIONTREE",
          class = "POSITIONTREE_error"
        )
      }
      # create and populate the coordinate data frame
      XY <- data.frame(
        n = seq_len(self$order()),
        x = private$XCOORD,
        y = private$YCOORD
      )
      # return the coordinate data frame
      return(XY)
    }
  )
)
