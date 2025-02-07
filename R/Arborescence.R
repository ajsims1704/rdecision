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
    initialize = function(V, A) {
      # initialize the base Digraph class (checks V, A)
      super$initialize(V, A)
      # check that the graph is an arborescence
      abortifnot(self$is_arborescence(),
        message = "The graph must be an arborescence",
        class = "not_arborescence"
      )
      # return new Arborescence object
      return(invisible(self))
    },

    #' @description Find the parent of a Node.
    #' @param v Index node, or a list of index Nodes.
    #' @return A list of Nodes of the same length as v, if v is a list, or a
    #' scalar Node if v is a single node. NA if v (or an element of v) is the
    #' root node.
    parent = function(v) {
      # coerce v to a list, if it is not
      v <- if (is.list(v)) v else list(v)
      # find the indices of the index vertexes
      iv <- self$vertex_index(v)
      # find the parent of each vertex, or none if root.
      A <- self$digraph_adjacency_matrix()
      pnodes <- lapply(X = iv, FUN = function(n) {
        p <- which(A[, n] > 0L)
        if (length(p) > 0L) {
          p <- self$vertex_at(p)
        } else {
          p <- NA
        }
        return(p)
      })
      # coerce to scalar if necessary
      pnodes <- if (length(pnodes) == 1L) pnodes[[1L]] else pnodes
      return(pnodes)
    },

    #' @description Test whether the given node(s) is (are) parent(s).
    #' @details In an arborescence, \code{is_parent()} and \code{is_leaf()} are
    #' mutually exclusive.
    #' @param v Node to test, or a list of Nodes.
    #' @return A logical vector of the same length as v, if v is a list, or a
    #' logical scalar if v is a single node.
    is_parent = function(v) {
      # coerce v to a list, if it is not
      v <- if (is.list(v)) v else list(v)
      # test each node (direct_successors is not vectorized)
      isp <- vapply(X = v, FUN.VALUE = TRUE, FUN = function(n) {
        # check if this vertex has direct successors (also checks that v is in
        # the graph and is a single node)
        children <- self$direct_successors(n)
        # an empty list means no children
        return(!identical(children, list()))
      })
      return(isp)
    },

    #' @description Test whether the given node is a leaf.
    #' @details In an arborescence, \code{is_parent()} and \code{is_leaf()} are
    #' mutually exclusive.
    #' @param v Node to test, or a list of Nodes.
    #' @return A logical vector of the same length as v, if v is a list, or a
    #' logical scalar is v is a single node.
    is_leaf = function(v) {
      # coerce v to a list, if it is not
      v <- if (is.list(v)) v else list(v)
      # test each node (direct_successors is not vectorized)
      isp <- vapply(X = v, FUN.VALUE = TRUE, FUN = function(n) {
        # check if this vertex has direct successors (also checks that v is in
        # the graph and is a single node)
        children <- self$direct_successors(n)
        # an empty list means no children
        return(identical(children, list()))
      })
      return(isp)
    },

    #' @description Find the root vertex of the arborescence.
    #' @return The root vertex.
    root = function() {
      # vertex with no incoming edges (only one, checked in initialize)
      B <- self$digraph_incidence_matrix()
      iu <- which(
        apply(B, MARGIN = 1L, function(r) !any(r > 0L))
      )
      u <- self$vertex_at(iu)
      return(u)
    },

    #' @description Is the specified node the root?
    #' @param v Vertex to test, or list of vertexes
    #' @return A logical vector if v is a list, or a logical scalar if v is a
    #' single node.
    is_root = function(v) {
      # coerce v to a list, if it is not
      v <- if (is.list(v)) v else list(v)
      isr <- vapply(X = v, FUN.VALUE = FALSE, FUN = function(n) {
        return(identical(n, self$root()))
      })
      return(isr)
    },

    #' @description Find the siblings of a vertex in the arborescence.
    #' @param v Vertex to test (only accepts a scalar Node).
    #' @return A (possibly empty) list of siblings.
    siblings = function(v) {
      abortif(
        is.list(v),
        message = "Argument 'v' must be a single node",
        class = "invalid_argument"
      )
      # list of siblings
      S <- list()
      # if v is the root, there are no siblings
      if (!self$is_root(v)) {
        # find parent (also checks if v is in the graph). In an arborescence,
        # there will only be a single parent (if v is not the root).
        p <- self$direct_predecessors(v)
        # get all children of the parent, which includes v
        C <- self$direct_successors(p[[1L]])
        # exclude self
        for (c in C) {
          if (!identical(c, v)) {
            S <- c(S, c)
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
      for (iv in self$vertex_along()) {
        v <- self$vertex_at(iv)
        if (self$is_leaf(v)) {
          pp <- self$paths(r, v)
          P[[length(P) + 1L]] <- pp[[1L]]
        }
      }
      # return the paths
      return(P)
    },

    #' @description Implements function \verb{POSITIONTREE} (Walker, 1989) to
    #' determine the coordinates for each node in an arborescence.
    #' @details In the \code{rdecision} implementation, the
    #' sibling order is taken to be the lexicographic order of the node
    #' labels, if they are unique among siblings, or the node indexes otherwise.
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
      private$PREVNODE <- vector(mode = "integer", length = self$order())
      # per-node arrays
      private$LEFTNEIGHBOR <- vector(mode = "integer", length = self$order())
      private$MODIFIER <- vector(mode = "numeric", length = self$order())
      private$PRELIM <- vector(mode = "numeric", length = self$order())
      private$XCOORD <- vector(mode = "numeric", length = self$order())
      private$YCOORD <- vector(mode = "numeric", length = self$order())
      # initialize list of previous nodes at each level
      INITPREVNODELIST <- function() {
      }
      # get previous node at this level
      GETPREVNODEATLEVEL <- function(Level) {
        # Level is zero-based
        return(private$PREVNODE[[Level + 1L]])
      }
      # set an element in the list
      SETPREVNODEATLEVEL <- function(Level, iNode) {
        # Level is zero-based
        private$PREVNODE[[Level + 1L]] <- iNode
      }
      # test if node is a leaf
      ISLEAF <- function(iNode) {
        v <- self$vertex_at(iNode)
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
        if (RootOrientation %in% c("NORTH", "SOUTH")) {
          NodeSize <- NodeSize + RIGHTSIZE(LeftNode)
          NodeSize <- NodeSize + LEFTSIZE(RightNode)
        } else {
          NodeSize <- NodeSize + TOPSIZE(LeftNode)
          NodeSize <- NodeSize + BOTTOMSIZE(RightNode)
        }
        return(NodeSize)
      }
      # @description Definition of order for tree drawing
      # @param children a list of nodes
      # @return an integer vector of the same length as children, with
      # each entry equal to the order, based on lexicographical sort.
      childorder <- function(children) {
        # fetch the index of each vertex
        ikids <- self$vertex_index(children)
        # fetch the label (name) of each child
        lkids <- self$vertex_label(ikids)
        # use node indexes if any sibling labels are duplicates
        okids <- rank(ikids)
        if (anyDuplicated(lkids) == 0L) {
          okids <- rank(lkids)
        }
        return(okids)
      }
      # test if a node has a left sibling
      HASLEFTSIBLING <- function(iNode) {
        v <- self$vertex_at(iNode)
        sibs <- self$siblings(v)
        hasleft <- FALSE
        if (length(sibs) > 0L) {
          children <- c(v, sibs)
          okids <- childorder(children)
          hasleft <- (okids[[1L]] > 1L)
        }
        return(hasleft)
      }
      # find the node's closest sibling on the left (0 if none)
      LEFTSIBLING <- function(iNode) {
        rn <- 0L
        v <- self$vertex_at(iNode)
        sibs <- self$siblings(v)
        if (length(sibs) > 0L) {
          children <- c(v, sibs)
          okids <- childorder(children)
          ikids <- self$vertex_index(children)
          ome <- okids[[1L]]
          if (ome > 1L) {
            rn <- ikids[[which(okids == (ome - 1L))]]
          }
        }
        rno <- 0L
        isibs <- self$vertex_index(sibs)
        lsibs <- isibs[which(isibs < iNode)]
        if (any(isibs < iNode)) {
          rno <- max(lsibs)
        }
        return(rn)
      }
      # test if a node has a right sibling
      HASRIGHTSIBLING <- function(iNode) {
        v <- self$vertex_at(iNode)
        sibs <- self$siblings(v)
        hasright <- FALSE
        if (length(sibs) > 0L) {
          children <- c(v, sibs)
          okids <- childorder(children)
          hasright <- (okids[[1L]] < length(okids))
        }
        return(hasright)
      }
      # find the node's closest sibling on the right (0 if none)
      RIGHTSIBLING <- function(iNode) {
        rn <- 0L
        v <- self$vertex_at(iNode)
        sibs <- self$siblings(v)
        if (length(sibs) > 0L) {
          children <- c(v, sibs)
          okids <- childorder(children)
          ikids <- self$vertex_index(children)
          ome <- okids[[1L]]
          if (ome < length(okids)) {
            rn <- ikids[[which(okids == (ome + 1L))]]
          }
        }
        isibs <- self$vertex_index(sibs)
        rs <- isibs[which(isibs > iNode)]
        rno <- 0L
        if (any(isibs > iNode)) {
          rno <- min(rs)
        }
        return(rn)
      }
      # parent of the node (0 if none)
      PARENT <- function(iNode) {
        rn <- 0L
        v <- self$vertex_at(iNode)
        if (!self$is_root(v)) {
          p <- self$parent(v)
          rn <- self$vertex_index(p)
        }
        return(rn)
      }
      # does the specified node have a child?
      HASCHILD <- function(iNode) {
        v <- self$vertex_at(iNode)
        children <- self$direct_successors(v)
        ichildren <- self$vertex_index(children)
        return(length(ichildren) > 0L)
      }
      # find the first child of iNode (0 if none)
      FIRSTCHILD <- function(iNode) {
        rn <- 0L
        v <- self$vertex_at(iNode)
        children <- self$direct_successors(v)
        ichildren <- self$vertex_index(children)
        if (length(ichildren) > 0L) {
          okids <- childorder(children)
          rn <- ichildren[[which(okids == 1L)]]
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
        CompareDepth <- 1L
        DepthToStop <- MaxDepth - Level
        while (CompareDepth <= DepthToStop) {
          if (Leftmost == 0L) break
          Neighbor <- private$LEFTNEIGHBOR[[Leftmost]]
          if (Neighbor == 0L) break
          # Compute the location of Leftmost and where it should
          # be with respect to Neighbor.
          LeftModsum <- 0.0
          RightModsum <- 0.0
          AncestorLeftmost <- Leftmost
          AncestorNeighbor <- Neighbor
          for (i in seq(0L, CompareDepth - 1L)) {
            AncestorLeftmost <- PARENT(AncestorLeftmost)
            AncestorNeighbor <- PARENT(AncestorNeighbor)
            RightModsum <- RightModsum + private$MODIFIER[[AncestorLeftmost]]
            LeftModsum <- LeftModsum + private$MODIFIER[[AncestorNeighbor]]
          }
          # Find the MoveDistance, and apply it to Node's subtree.
          # Add appropriate portions to smaller interior subtrees.
          MoveDistance <- (private$PRELIM[[Neighbor]] +
                             LeftModsum +
                             SubtreeSeparation +
                             MEANNODESIZE(Leftmost, Neighbor)) -
            (private$PRELIM[[Leftmost]] + RightModsum)
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
                private$PRELIM[[TempPtr]] <- private$PRELIM[[TempPtr]] +
                  MoveDistance
                private$MODIFIER[[TempPtr]] <- private$MODIFIER[[TempPtr]] +
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
        }
        return
      }
      # function for first postorder walk
      FIRSTWALK <- function(iNode, Level) {
        # Set the pointer to the previous node at this level.
        private$LEFTNEIGHBOR[[iNode]] <- GETPREVNODEATLEVEL(Level)
        SETPREVNODEATLEVEL(Level, iNode) # This is now the previous.
        private$MODIFIER[[iNode]] <- 0.0   # Set the default modifier value.
        if (ISLEAF(iNode) || Level == MaxDepth) {
          if (HASLEFTSIBLING(iNode)) {
            # Determine the preliminary x-coordinate based on:
            #   the preliminary x-coordinate of the left sibling,
            #   the separation between sibling nodes, and
            #   the mean size of left sibling and current node.
            private$PRELIM[[iNode]] <- private$PRELIM[[LEFTSIBLING(iNode)]] +
              SiblingSeparation +
              MEANNODESIZE(LEFTSIBLING(iNode), iNode)
          } else {
            # No sibling on the left to worry about.
            private$PRELIM[[iNode]] <- 0.0
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
            (private$PRELIM[[Leftmost]] + private$PRELIM[[Rightmost]]) / 2.0
          if (HASLEFTSIBLING(iNode)) {
            private$PRELIM[[iNode]] <- private$PRELIM[[LEFTSIBLING(iNode)]] +
              SiblingSeparation +
              MEANNODESIZE(LEFTSIBLING(iNode), iNode)
            private$MODIFIER[[iNode]] <- private$PRELIM[[iNode]] - Midpoint
            APPORTION(iNode, Level)
          } else {
            private$PRELIM[[iNode]] <- Midpoint
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
            xTemp <- xTopAdjustment + (private$PRELIM[[iNode]] + Modsum)
            yTemp <- yTopAdjustment - (Level * LevelSeparation)
          } else if (RootOrientation == "SOUTH") {
            xTemp <- xTopAdjustment + (private$PRELIM[[iNode]] + Modsum)
            yTemp <- yTopAdjustment + (Level * LevelSeparation)
          } else if (RootOrientation == "EAST") {
            xTemp <- xTopAdjustment + (Level * LevelSeparation)
            yTemp <- yTopAdjustment - (private$PRELIM[[iNode]] + Modsum)
          } else if (RootOrientation == "WEST") {
            xTemp <- xTopAdjustment - (Level * LevelSeparation)
            yTemp <- yTopAdjustment - (private$PRELIM[[iNode]] + Modsum)
          }
          # Check to see that xTemp and yTemp are of the proper
          # size for your application.
          if (CHECKEXTENTSRANGE(xTemp, yTemp)) {
            private$XCOORD[[iNode]] <- xTemp
            private$YCOORD[[iNode]] <- yTemp
            Result <- TRUE
            if (HASCHILD(iNode)) {
              # Apply the Modifier value for this node to
              # all its offspring.
              Result <- SECONDWALK(FIRSTCHILD(iNode),
                                   Level + 1L,
                                   Modsum + private$MODIFIER[[iNode]])
            }
            if (Result && HASRIGHTSIBLING(iNode)) {
              Result <- SECONDWALK(
                ##DEBUG - not Level+1##
                RIGHTSIBLING(iNode), Level, Modsum
              )
            }
          } else {
            # Continuing would put the tree outside of the
            # drawable extent's range.
            # nocov start
            # there are no limits on the drawable extent, so never reaches here
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
        # check iNode
        abortif(
          iNode == 0L,
          message = "Argument to POSITIONTREE must not be a null pointer",
          class = "invalid_inode"
        )
        # Initialize the list of previous nodes at each level.
        INITPREVNODELIST()
        # Do the preliminary positioning with a postorder walk.
        FIRSTWALK(iNode, 0L)
        # Determine how to adjust all the nodes with respect to
        # the location of the root.
        if (RootOrientation %in% c("NORTH", "SOUTH")) {
          xTopAdjustment <- private$XCOORD[[iNode]] - private$PRELIM[[iNode]]
          yTopAdjustment <- private$YCOORD[[iNode]]
        } else {
          xTopAdjustment <- private$XCOORD[[iNode]]
          yTopAdjustment <- private$YCOORD[[iNode]] + private$PRELIM[[iNode]]
        }
        # Do the final positioning with a preorder walk
        return(SECONDWALK(iNode, 0L, 0L))
      }
      # call Walker's main function
      iRoot <- self$vertex_index(self$root())
      rc <- POSITIONTREE(iRoot)
      abortifnot(
        rc, message = "Error in POSITIONTREE", class = "POSITIONTREE_error"
      )
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
