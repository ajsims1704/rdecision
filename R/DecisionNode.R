#' @title A decision node in a decision tree
#' @description An R6 class representing a decision node in a decision tree.
#' @details A class to represent a decision node in a decision tree. The node
#' is associated with one or more branches to child nodes. Inherits from class
#' \code{Node}.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'
DecisionNode <- R6::R6Class(
  classname = "DecisionNode",
  lock_class = TRUE,
  inherit = Node,
  public = list(

    #' @description Create a new decision node.
    #' @param label A label for the node. Must be defined because the label is
    #' used in tabulation of strategies. The label is automatically converted
    #' to a syntactically valid (in R) name to ensure it can be used as a column
    #' name in a data frame.
    #' @return A new \code{DecisionNode} object.
    initialize = function(label) {
      # check there is a valid label
      abortif(
        missing(label),
        !is.character(label),
        nchar(label) == 0L,
        message = "'label' must be syntactically valid with length > 0",
        class = "invalid_label"
      )
      label <- make.names(label)
      # ensure base class fields are initialized
      super$initialize(label)
      return(invisible(self))
    },

    #' @description Creates a grid::grob for drawing a decision node.
    #' @param x x coordinate of the node, unit object.
    #' @param y y coordinate of the node, unit object.
    #' @param bb Logical. If TRUE, function returns the bounding box.
    #' @return A grob containing the symbol and label, or a bounding box
    #' as a grid::unit vector with 4 values: left, right, bottom, top.
    grob = function(x, y, bb = FALSE) {
      # check
      abortifnot(
        grid::is.unit(x),
        grid::is.unit(y)
      )
      # size of symbol
      a <- grid::unit(sqrt(pi / 4.0), "char")
      dy <- grid::unit(0.4, "char")
      # find the bounding box relative to (0, 0) for the node and its symbol
      sw <- grid::stringWidth(self$label())
      sh <- grid::stringHeight(self$label()) + dy
      bbxl <- -max(sw, a)
      bbxr <- a
      bbyb <- -a
      bbyt <- sh + a
      # symbol
      gsym <- grid::rectGrob(
        x = x, y = y,
        width = a * 2.0, height = a * 2.0,
        just = c("centre", "centre"),
        gp = grid::gpar(col = "black", fill = "lightgray"),
        vp = NULL
      )
      # label
      glab <- grid::textGrob(
        label = self$label(), x = x, y = y + a + dy,
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
