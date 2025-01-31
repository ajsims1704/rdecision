#' @title An action in a decision tree
#' @description R6 class representing an action (choice) edge.
#' @details A specialism of class \code{Arrow} which is used in a decision tree
#' to represent an edge whose source node is a \code{DecisionNode}.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'
Action <- R6::R6Class(
  classname = "Action",
  lock_class = TRUE,
  inherit = Arrow,
  private = list(
    edge_cost = NULL,
    edge_benefit = NULL
  ),
  public = list(

    #' @description Create an object of type \code{Action}. Optionally, a cost
    #' and a benefit may be associated with traversing the edge. A \dfn{pay-off}
    #' (benefit minus cost)  is sometimes used in edges of decision trees; the
    #' parametrization used here is more general.
    #' @param source_node Decision node from which the arrow leaves.
    #' @param target_node Node to which the arrow points.
    #' @param label Character string containing the arrow label. This
    #' must be defined for an action because the label is used in
    #' tabulation of strategies. It is recommended to choose labels that are
    #' brief and not punctuated with spaces, dots or underscores.
    #' @param cost Cost associated with traversal of this edge (numeric or
    #' \code{ModVar}), not NA.
    #' @param benefit Benefit associated with traversal of the edge, (numeric or
    #' \code{ModVar}), not NA.
    #' @return A new \code{Action} object.
    initialize = function(source_node, target_node, label, cost = 0.0,
                          benefit = 0.0) {
      # check label
      abortifnot(
        is.character(label),
        nchar(label) > 0L,
        message = "Argument 'label' must be a string",
        class = "invalid_label"
      )
      # initialize base class (checks that source and target are both Nodes)
      super$initialize(
        source = source_node, target = target_node, label = label
      )
      # check that source inherits from DecisionNode
      abortifnot(
        inherits(source_node, what = "DecisionNode"),
        message = "Node 'source_node' must be a DecisionNode",
        class = "invalid_source"
      )
      # check and set cost, ensuring initialization
      self$set_cost(cost)
      # check and set benefit, ensuring initialization
      self$set_benefit(benefit)
      # Return Action node
      return(invisible(self))
    },

    #' @description Creates a grid::grob for an action edge.
    #' @param xs x coordinate of source of edge, grid::unit object.
    #' @param ys y coordinate of source of edge, grid::unit object.
    #' @param xt x coordinate of target of edge, grid::unit object.
    #' @param yt y coordinate of target of edge, grid::unit object.
    #' @param fs Fraction of the edge which slopes.
    #' @return A grid::grob containing the symbol and label.
    grob = function(xs, ys, xt, yt, fs = 0.2) {
      # check arguments
      abortifnot(
        grid::is.unit(xs),
        grid::is.unit(ys),
        grid::is.unit(xt),
        grid::is.unit(yt)
      )
      # create a gTree object for the line and its label
      gedge <- grid::grobTree()
      # draw the articulated line
      gedge <- grid::addGrob(
        gTree = gedge,
        child = grid::moveToGrob(x = xs, y = ys)
      )
      xj <- (xt - xs) * fs + xs
      yj <- yt
      gedge <- grid::addGrob(
        gTree = gedge,
        child = grid::lineToGrob(x = xj, y = yj)
      )
      gedge <- grid::addGrob(
        gTree = gedge,
        child = lineToGrob(x = xt, y = yt)
      )
      # add label above or below
      vp <- grid::viewport(
        x = xj, y = yj, just = c("left", "bottom")
      )
      ytn <- grid::convertUnit(yt, "native", valueOnly = TRUE)
      ysn <- grid::convertUnit(ys, "native", valueOnly = TRUE)
      if (ytn < ysn) {
        yl <- grid::unit(0.4, "char")
        jl <- c("left", "bottom")
      } else {
        yl <- grid::unit(-0.4, "char")
        jl <- c("left", "top")
      }
      gedge <- grid::addGrob(
        gTree = gedge,
        child = grid::textGrob(
          label = self$label(),
          x = grid::unit(0.2, "char"), y = yl, just = jl,
          vp = vp
        )
      )
      return(gedge)
    },

    #' @description Find all the model variables of type \code{ModVar} that have
    #' been specified as values associated with this \code{Action}. Includes
    #' operands of these \code{ModVar}s, if they are expressions.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # build list, possibly including duplicates
      iv <- c(private$edge_cost, private$edge_benefit)
      ov <- iv[which(is_class(iv, what = "ModVar"))]
      ev <- iv[which(is_class(iv, what = "ExprModVar"))]
      for (v in ev) {
        ov <- c(ov, unlist(v$operands()))
      }
      # return the unique list
      return(unique(ov))
    },

    #' @description Return the current value of the edge probability, i.e., the
    #' conditional probability of traversing the edge.
    #' @return Numeric value equal to 1.
    p = function() {
      return(1.0)
    },

    #' @description Set the cost associated with the action edge.
    #' @param c Cost associated with traversing the action edge. Of type numeric
    #' or \code{ModVar}, not NA.
    #' @return Updated \code{Action} object.
    set_cost = function(c = 0.0) {
      abortifnot(
        inherits(c, what = c("numeric", "ModVar")),
        message = "Argument 'c' must be of type 'numeric' or 'ModVar'.",
        class = "invalid_cost"
      )
      if (inherits(c, what = "numeric")) {
        abortif(
          is.na(c),
          message = "Setting parameter 'c' to NA is not allowed.",
          class = "invalid_cost"
        )
      }
      private$edge_cost <- c
      return(invisible(self))
    },

    #' @description Return the cost associated with traversing the edge.
    #' @return Cost.
    cost = function() {
      rv <- as_numeric(private$edge_cost)
      return(rv)
    },

    #' @description Set the benefit associated with the action edge.
    #' @param b Benefit associated with traversing the action edge. Of type
    #' numeric or \code{ModVar}.
    #' @return Updated \code{Action} object.
    set_benefit = function(b = 0.0) {
      abortifnot(
        inherits(b, what = c("numeric", "ModVar")),
        message = "Argument 'b' must be of type 'numeric' or 'ModVar'.",
        class = "invalid_benefit"
      )
      if (inherits(b, what = "numeric")) {
        abortif(
          is.na(b),
          message = "Setting parameter 'b' to NA is not allowed.",
          class = "invalid_benefit"
        )
      }
      private$edge_benefit <- b
      return(invisible(self))
    },

    #' @description Return the benefit associated with traversing the edge.
    #' @return Benefit.
    benefit = function() {
      rv <- as_numeric(private$edge_benefit)
      return(rv)
    }
  )
)
