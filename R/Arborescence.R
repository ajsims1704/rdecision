#' @title 
#' Arborescence
#' 
#' @description
#' An R6 class to represent an arborescence (rooted directed tree).
#' 
#' @details 
#' Class to encapsulate a directed rooted tree specialization of a digraph.
#' An arborescence must be a directed tree with exactly one root and the
#' directed paths from the root must be unique.
#' 
#' @references 
#'   \cite{Walker, John Q II. A A node-positioning algorithm for general trees.
#'   University of North Carolina Technical Report TR 89-034, 1989.
#'  }
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
        rlang::abort(
          "The graph must be an arborescence", 
          class="not_arborescence"
        )
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
      B <- self$digraph_incidence_matrix()
      u <- which(apply(B, MARGIN=1, function(r){!any(r>0)}),arr.ind=TRUE)
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
      vapply(X=private$V, FUN.VALUE=TRUE, FUN=function(v){
        if (self$is_leaf(v)) {
          pp <- self$paths(r,v)
          P[[length(P)+1]] <<- pp[[1]]    
        }
        return(TRUE)
      })
      # return the paths
      return(P)
    },
    
    #' @description
    #' Implements function POSITIONTREE (Walker, 1989) to
    #' determine the coordinates for each node in an arborescence.
    #' @param SiblingSeparation Distance in arbitrary units for the
    #' distance between siblings.
    #' @param SubtreeSeparation Distance in arbitrary units for the
    #' distance between neigbouring subtrees.
    #' @param LevelSeparation Distance in arbitrary units for the 
    #' separation between adjacent levels.
    #' @param RootOrientation Must be one of "NORTH", "SOUTH", "EAST", "WEST".
    #' Defined as per Walker (1989), but noting that Walker assumed that
    #' y increased down the page. Thus the meaning of NORTH and SOUTH are
    #' opposite to his, with the default (SOUTH) having the child nodes at
    #' positive y value and root at zero, as per his example (figure 12).
    #' @return A data frame with one row per node and three columns (n, x
    #' and y) where \code{n} gives the node index given by the 
    #' Graph::vertex_index() function.
    # There were 3 bugs in the pseudo-code in the report, possibly corrected
    # in the later paper, indicated by ##DEBUG## in the code below. 
    postree = function(SiblingSeparation=4, SubtreeSeparation=4, 
                       LevelSeparation=1, RootOrientation="SOUTH") {
      # check input parameters
      if (!is.numeric(SiblingSeparation)) {
        rlang::abort(
          message = "'SiblingSeparation' must be numeric",
          class = "non-numeric_SiblingSeparation"
        )
      }
      if (!is.numeric(SubtreeSeparation)) {
        rlang::abort(
          message = "'SubstreeSeparation' must be numeric",
          class = "non-numeric_SubtreeSeparation"
        )
      }
      if (!is.numeric(LevelSeparation)) {
        rlang::abort(
          message = "'LevelSeparation' must be numeric",
          class = "non-numeric_LevelSeparation"
        )
      }
      if (!is.character(RootOrientation)) {
        rlang::abort(
          message = "'RootOrientation' must be numeric",
          class = "non-character_RootOrientation"
        )
      }
      if (!(RootOrientation %in% c("NORTH", "SOUTH", "EAST", "WEST"))) {
        rlang::abort(
          message = "'RootOrientation' must be one of NORTH, SOUTH, EAST, WEST",
          class = "illegal_RootOrientation"
        )
      }
      # globals for the algorithm
      LevelZeroPtr <- 0
      MaxDepth <- Inf
      xTopAdjustment <- 0
      yTopAdjustment <- 0
      # prevnode list (max 'height' is order of graph)
      PREVNODE <- vector(mode="integer", length=self$order())
      # per-node arrays
      LEFTNEIGHBOR <- vector(mode="integer", length=self$order())
      MODIFIER <- vector(mode="numeric", length=self$order())
      PRELIM <- vector(mode="numeric", length=self$order())
      XCOORD <- vector(mode="numeric", length=self$order())
      YCOORD <- vector(mode="numeric", length=self$order())
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
      # top size of each node (EAST/WEST trees)
      TOPSIZE <- function(iNode) {
        return(1)
      }
      # bottom size of each node (EAST/WEST trees)
      BOTTOMSIZE <- function(iNode) {
        return(1)
      }
      # mean node size
      MEANNODESIZE <- function(LeftNode, RightNode) {
        NodeSize <- 0
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
        iS <- sapply(S, FUN=function(s){return(self$vertex_index(s))})
        rb <- any(iS<iNode)
        return(rb)
      }
      # find the node's closest sibling on the left (0 if none)
      LEFTSIBLING <- function(iNode) {
        rn <- 0
        v <- private$V[[iNode]]
        S <- self$siblings(v)
        iS <- sapply(S, FUN=function(s){return(self$vertex_index(s))})
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
        iS <- sapply(S, FUN=function(s){return(self$vertex_index(s))})
        rb <- any(iS>iNode)
        return(rb)
      }
      # find the node's closest sibling on the right (0 if none)
      RIGHTSIBLING <- function(iNode) {
        rn <- 0
        v <- private$V[[iNode]]
        S <- self$siblings(v)
        iS <- sapply(S, FUN=function(s){return(self$vertex_index(s))})
        rS <- iS[which(iS>iNode)]
        if (any(iS>iNode)) {
          rn <- min(rS)
        }
        return(rn)
      }
      # parent of the node (0 if none)
      PARENT <- function(iNode) {
        rn <- 0
        v <- private$V[[iNode]]
        P <- self$direct_predecessors(v)
        if (length(P)==1) {
          rn <- self$vertex_index(P[[1]])
        }
        return(rn)
      }
      # does the specified node have a child?
      HASCHILD <- function(iNode) {
        v <- private$V[[iNode]]
        C <- self$direct_successors(v)
        return(length(C)>0)
      }
      # find the first child of iNode (0 if none)
      FIRSTCHILD <- function(iNode) {
        rn <- 0
        v <- private$V[[iNode]]
        C <- self$direct_successors(v)
        iC <- sapply(C, FUN=function(c){return(self$vertex_index(c))})
        if (length(iC) > 0) {
          rn <- min(iC)
        }
        return(rn)
      }
      # Leftmost descendant of a node at a given depth
      GETLEFTMOST <- function(iNode, Level, Depth) {
        if (Level >= Depth) {
          return(iNode)
        } else if (ISLEAF(iNode)) {
          return(0)
        } else {
          Rightmost <- FIRSTCHILD(iNode)
          Leftmost <- GETLEFTMOST(Rightmost,Level+1,Depth)
          # Do a postorder walk of the subtree below Node.
          while (Leftmost == 0 && HASRIGHTSIBLING(Rightmost)) {
            Rightmost <- RIGHTSIBLING(Rightmost)
            Leftmost <- GETLEFTMOST(Rightmost, Level + 1, Depth)
          }
          return(Leftmost)
        }
      }
      # clean up small sibling subtrees      
      APPORTION <- function(iNode, Level) {
        Leftmost <- FIRSTCHILD(iNode)
        Neighbor <- LEFTNEIGHBOR[Leftmost]
        CompareDepth <- 1
        DepthToStop <- MaxDepth - Level
        #
        while (Leftmost != 0 && Neighbor != 0 && CompareDepth <= DepthToStop) {
          # Compute the location of Leftmost and where it should 
          # be with respect to Neighbor.
          LeftModsum <- 0
          RightModsum <- 0
          AncestorLeftmost <- Leftmost
          AncestorNeighbor <- Neighbor
          for (i in seq(0,CompareDepth-1)) {
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
          if (MoveDistance > 0) {
            # Count interior sibling subtrees in LeftSiblings
            TempPtr <- iNode
            LeftSiblings <- 0
            while (TempPtr != 0 && TempPtr != AncestorNeighbor) {
              LeftSiblings <- LeftSiblings + 1
              TempPtr <- LEFTSIBLING(TempPtr)
            } 
            if (TempPtr != 0) {
              # Apply portions to appropriate left sibling subtrees.
              Portion <- MoveDistance / LeftSiblings
              TempPtr <- iNode
              while (TempPtr != AncestorNeighbor) { ##DEBUG - != not =##
                PRELIM[TempPtr] <<- PRELIM[TempPtr] + MoveDistance
                MODIFIER[TempPtr] <<- MODIFIER[TempPtr] + MoveDistance
                MoveDistance <- MoveDistance - Portion
                TempPtr <- LEFTSIBLING(TempPtr)
              }
            }
          }
          else {
            # Don't need to move anything--it needs to 
            # be done by an ancestor because 
            # AncestorNeighbor and AncestorLeftmost are 
            # not siblings of each other.
            return
          }
          # Determine the leftmost descendant of Node at the next 
          # lower level to compare its positioning against that of
          # its Neighbor
          CompareDepth <- CompareDepth + 1
          if (ISLEAF(Leftmost)) {
            Leftmost <- GETLEFTMOST(iNode, 0, CompareDepth)
          } else {
            Leftmost <- FIRSTCHILD(Leftmost)        
          }
          Neighbor <- LEFTNEIGHBOR[Leftmost] ##DEBUG - line missing##

        }
        return
      }
      # function for first postorder walk
      FIRSTWALK <- function(iNode, Level) {
        # Set the pointer to the previous node at this level.
        LEFTNEIGHBOR[iNode] <<- GETPREVNODEATLEVEL(Level) 
        SETPREVNODEATLEVEL(Level, iNode) # This is now the previous.
        MODIFIER[iNode] <<- 0   # Set the default modifier value. 
        if (ISLEAF(iNode) || Level==MaxDepth) {
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
            FIRSTWALK(Rightmost, Level + 1) 
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
            xTemp <- xTopAdjustment + (PRELIM[iNode] + Modsum)
            yTemp <- yTopAdjustment - (Level * LevelSeparation)
          } else if (RootOrientation == "SOUTH") {
            xTemp <- xTopAdjustment + (PRELIM[iNode] + Modsum)
            yTemp <- yTopAdjustment + (Level * LevelSeparation)
          } else if (RootOrientation == "EAST") {
            xTemp <- xTopAdjustment + (Level * LevelSeparation)
            yTemp <- yTopAdjustment - (PRELIM[iNode] + Modsum)
          } else if (RootOrientation == "WEST") {
            xTemp <- xTopAdjustment - (Level * LevelSeparation)
            yTemp <- yTopAdjustment - (PRELIM[iNode] + Modsum)
          } 
          # Check to see that xTemp and yTemp are of the proper 
          # size for your application. 
          if (CHECKEXTENTSRANGE(xTemp, yTemp)) {
            XCOORD[iNode] <<- xTemp
            YCOORD[iNode] <<- yTemp
            Result <- TRUE
            if (HASCHILD(iNode)) {
              # Apply the Modifier value for this node to 
              # all its offspring. 
              Result <- SECONDWALK(FIRSTCHILD(iNode),
                                   Level + 1,
                                   Modsum + MODIFIER[iNode])
            }
            if (Result==TRUE && HASRIGHTSIBLING(iNode)) {
                Result <- SECONDWALK(RIGHTSIBLING(iNode),
                                     Level,   ##DEBUG - not Level+1##
                                     Modsum)
            }
          } else {
            # Continuing would put the tree outside of the 
            # drawable extents range.
            Result <- FALSE
          }
        } else {
          # We are at a level deeper than that we want to draw. 
          Result <- TRUE
        }
        return(Result)
      }      
      # determine coordinates for each node in a tree
      POSITIONTREE <- function(iNode) {
        if (iNode != 0) {
          # Initialize the list of previous nodes at each level.  
          INITPREVNODELIST()
          # Do the preliminary positioning with a postorder walk.
          FIRSTWALK(iNode,0)
          # Determine how to adjust all the nodes with respect to 
          # the location of the root. 
          if (RootOrientation %in% c("NORTH","SOUTH")) {
            xTopAdjustment <- XCOORD[iNode] - PRELIM[iNode]
            yTopAdjustment <- YCOORD[iNode]
          } else {
            xTopAdjustment <- XCOORD[iNode] 
            yTopAdjustment <- YCOORD[iNode] + PRELIM[iNode]
          }
          # Do the final positioning with a preorder walk
          return(SECONDWALK(iNode, 0, 0))      
        } else {
          # Trivial: return TRUE if a null pointer was passed.
          return(TRUE)         
        }
      }
      # call Walker's main function
      iRoot <- self$vertex_index(self$root())
      rc <- POSITIONTREE(iRoot)
      # create and populate the coordinate data frame
      XY <- data.frame(
        n = seq(1:self$order()),
        x = XCOORD,
        y = YCOORD
      )
      # return the coordinate data frame
      return(XY)
    }
    
  )
  
)
