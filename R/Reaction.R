#' @title A reaction (chance) edge in a decision tree
#' @description An R6 class representing a reaction (chance) edge in a decision
#' tree.
#' @details A specialism of class \code{Arrow} which is used in a decision tree
#' to represent edges whose source nodes are \code{ChanceNode}s.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#'
Reaction <- R6::R6Class(
  classname = "Reaction",
  lock_class = TRUE,
  inherit = Arrow,
  private = list(
    edge_cost = NULL,
    edge_benefit = NULL,
    edge_p = NULL
  ),
  public = list(

    #' @description Create an object of type \code{Reaction}. A probability
    #' must be assigned
    #' to the edge. Optionally, a cost and a benefit may be associated
    #' with traversing the edge. A \dfn{pay-off} (benefit-cost) is sometimes
    #' used in edges of decision trees; the parametrization used here is more
    #' general.
    #' @param source_node Chance node from which the reaction leaves.
    #' @param target_node Node which the reaction enters.
    #' @param p Conditional probability of traversing the reaction edge. At most
    #' one Reaction from each chance node may have this set to `NA_real_`, in
    #' which case it will be replaced by one minus the sum of conditional
    #' probabilities of the other reaction edges from the node.
    #' @param cost Cost associated with traversal of this edge (numeric or
    #' \code{ModVar}), not NA.
    #' @param benefit Benefit associated with traversal of the edge (numeric or
    #' \code{ModVar}), not NA.
    #' @param label Character string containing the reaction label.
    #' @return A new \code{Reaction} object.
    initialize = function(source_node, target_node, p = 0.0, cost = 0.0,
                          benefit = 0.0, label = "") {
      # initialize base class
      super$initialize(
        source_node = source_node, target_node = target_node, label = label
      )
      # check that source inherits from ChanceNode
      abortifnot(
        inherits(source_node, what = "ChanceNode"),
        message = "Node 'source' must be a ChanceNode",
        class = "invalid_source"
      )
      # check and set probability, ensuring initialization
      self$set_probability(p)
      # check and set cost, ensuring initialization
      self$set_cost(cost)
      # check and set benefit, ensuring initialization
      self$set_benefit(benefit)
      # Return reaction node
      return(invisible(self))
    },

    #' @description Creates a grid::grob for a reaction edge.
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
        child = grid::lineToGrob(x = xt, y = yt)
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

    #' @description Find all the model variables of type \code{ModVar} that
    #' have been specified as values associated with this Action. Includes
    #' operands of these \code{ModVar}s, if they are expressions.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # build list, possibly including duplicates
      iv <- c(private$edge_cost, private$edge_benefit, private$edge_p)
      ov <- iv[which(is_class(iv, what = "ModVar"))]
      ev <- iv[which(is_class(iv, what = "ExprModVar"))]
      for (v in ev) {
        ov <- c(ov, unlist(v$operands()))
      }
      # return the unique list
      return(unique(ov))
    },

    #' @description Set the probability associated with the reaction edge.
    #' @param p Conditional probability of traversing the reaction edge. Of type
    #' numeric or \code{ModVar}. If numeric, \code{p} must be in the range
    #' [0,1], or \code{NA_real_}. Note that setting \code{p = NA} will cause
    #' an error.
    #' @return Updated \code{Reaction} object.
    set_probability = function(p) {
      # check argument
      abortif(
        missing(p),
        !inherits(p, what = c("numeric", "ModVar")),
        inherits(p, what = "numeric") && !is.na(p) && (p < 0.0 || p > 1.0),
        message = paste(
          "'p' must not be missing",
          "NA_real_ or in range [0,1] if type 'numeric'",
          "or of type 'ModVar",
          sep = ","
        ),
        class = "invalid_probability"
      )
      private$edge_p <- p
      return(invisible(self))
    },

    #' @description Return the current value of the edge probability, i.e., the
    #' conditional probability of traversing the edge.
    #' @return Numeric value in range [0,1].
    p = function() {
      prob <- as_numeric(private$edge_p)
      return(prob)
    },

    #' @description Set the cost associated with the reaction edge.
    #' @param c Cost associated with traversing the reaction edge. Of type
    #' numeric or \code{ModVar}.
    #' @return Updated \code{Reaction} object.
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

    #' @description Set the benefit associated with the reaction edge.
    #' @param b Benefit associated with traversing the reaction edge. Of type
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
