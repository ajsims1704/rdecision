#' @title A chance node in a decision tree
#'
#' @description An R6 class representing a chance node in a decision tree.
#'
#' @details A chance node is associated with at least two branches to other
#' nodes, each of which has a conditional probability (the probability of
#' following that branch given that the node has been reached). Inherits from
#' class \code{Node}.
#'
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'
ChanceNode <- R6::R6Class(
  classname = "ChanceNode",
  lock_class = TRUE,
  inherit = Node,
  private = list(
  ),
  public = list(

    #' @description Create a new \code{ChanceNode} object
    #' @param label An optional label for the chance node.
    #' @return A new \code{ChanceNode} object
    initialize = function(label = "") {
      # ensure base class fields are initialized
      super$initialize(label)
      # return a ChanceNode object
      return(invisible(self))
    },

    #' @description Creates a grid::grob for a chance node.
    #' @param x x coordinate of the node, grid::unit object.
    #' @param y y coordinate of the node, grid::unit object.
    #' @param bb Logical. If TRUE, function returns the bounding box.
    #' @return A grob containing the symbol and label, or a bounding box
    #' as a grid::unit vector with elements: left, right, bottom, top.
    grob = function(x, y, bb = FALSE) {
      # check arguments
      abortifnot(
        grid::is.unit(x),
        grid::is.unit(y)
      )
      # size of symbol
      a <- grid::unit(1.0, "char")
      dy <- grid::unit(0.4, "char")
      # find the bounding box relative to (0, 0) for the node and its symbol
      if (nchar(self$label()) > 0L) {
        sw <- grid::stringWidth(self$label())
        sh <- grid::stringHeight(self$label()) + dy
      } else {
        sw <- grid::unit(0.0, "char")
        sh <- grid::unit(0.0, "char")
      }
      bbxl <- -max(sw, a)
      bbxr <- a
      bbyb <- -a
      bbyt <- sh + a
      # symbol
      gsym <- grid::circleGrob(
        x = x,
        y = y,
        r = a,
        gp = grid::gpar(col = "black", fill = "lightgray"),
        vp = NULL
      )
      # label
      glab <- grid::textGrob(
        label = self$label(),
        x = x,
        y = y + a + dy,
        just = c("right", "bottom"),
        vp = NULL
      )
      # return the leaf object as a gTree, or its bounding box
      if (bb) {
        rv <- grid::unit.c(x + bbxl, x + bbxr, y + bbyb, y + bbyt)
      } else {
        rv <- grid::gTree(children = grid::gList(gsym, glab))
      }
      return(rv)
    }
  )
)
