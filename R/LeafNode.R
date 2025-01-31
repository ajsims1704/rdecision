#' @title A leaf node in a decision tree
#' @description An R6 class representing a leaf (terminal) node in a decision
#' tree.
#' @details Represents a terminal state in a tree, and is associated with an
#' incremental utility. Inherits from class \code{Node}.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
LeafNode <- R6::R6Class(
  classname = "LeafNode",
  lock_class = TRUE,
  inherit = Node,
  private = list(
    node.utility = NULL,
    node.interval = NULL,
    ru = NULL
  ),
  public = list(

    #' @description Create a new \code{LeafNode} object; synonymous with a
    #' health state.
    #' @param label Character string; a label for the state; must be
    #' defined because it is used in tabulations. The label is automatically
    #' converted to a syntactically valid (in R) name to ensure it can be used
    #' as a column name in a data frame.
    #' @param utility The utility that a user associates with being in the
    #' health state for the interval. Can be \code{numeric} or
    #' a type of \code{ModVar}. If the type is \code{numeric}, the allowed
    #' range is \code{-Inf} to 1; if it is of type \code{ModVar}, it is
    #' unchecked.
    #' @param interval The time horizon, or duration for which a user is
    #' expected to occupy the health state and experience a health-related
    #' quality of life of \code{utility}, expressed as an R \code{difftime}
    #' object; default 1 year.
    #' @param ru Annual discount rate for future utility. Note
    #' this is a rate, not a probability (i.e., use 0.035 for 3.5\%).
    #' @return A new \code{LeafNode} object
    initialize = function(
      label,
      utility = 1.0,
      interval = as.difftime(365.25, units = "days"),
      ru = 0.0
    ) {
      # check arguments
      abortifnot(
        !rlang::is_missing(label),
        is.character(label),
        nchar(label) > 0L,
        message =
          "Argument `label` must not be missing and must be a non-empty string",
        class = "invalid_label"
      )
      abortifnot(
        inherits(interval, what = "difftime"),
        message = "Argument 'interval' must be of class 'difftime'.",
        class = "invalid_interval"
      )
      abortifnot(
        is.numeric(ru),
        message = "Discount rate must be of type numeric",
        class = "invalid_discount"
      )
      # initialize Node base class
      label <- make.names(label)
      super$initialize(label)
      # check and set utility
      self$set_utility(utility)
      # check and set the interval
      self$set_interval(interval)
      # set the utility discount
      private$ru <- ru
      # return updated node object
      return(invisible(self))
    },

    #' @description Creates a grid::grob for a leaf node.
    #' @param x x coordinate of the node, grid::unit object.
    #' @param y y coordinate of the node, grid::unit object.
    #' @param bb Logical. If TRUE, function returns the bounding box.
    #' @return A grid::grob containing the symbol and label, or a bounding box
    #' as a grid::unit vector with 4 elements: left, right, bottom, top.
    grob = function(x, y, bb = FALSE) {
      # check arguments
      abortifnot(
        grid::is.unit(x),
        grid::is.unit(y)
      )
      # symbol size
      a <- grid::unit(1.5 * sqrt(pi / sqrt(3.0)), "char")
      dx <- grid::unit(0.25, "char")
      # find the bounding box relative to (0, 0) for the node and its symbol
      sw <- grid::stringWidth(self$label()) + dx
      sh <- grid::stringHeight(self$label())
      bbxl <- -a / sqrt(3.0)
      bbxr <- sw + sqrt(3.0) * a / 6.0
      bbyb <- -max(a / 2.0, sh / 2.0)
      bbyt <- max(a / 2.0, sh / 2.0)
      # symbol
      gsym <- grid::polygonGrob(
        x = grid::unit.c(
          x - a / sqrt(3.0),
          x + sqrt(3.0) * a / 6.0,
          x + sqrt(3.0) * a / 6.0
        ),
        y = grid::unit.c(
          y, y + a / 2.0, y - a / 2.0
        ),
        gp = grid::gpar(fill = "lightgray", col = "black")
      )
      # label
      glab <- grid::textGrob(
        label = self$label(),
        x = x + dx + sqrt(3.0) * a / 6.0, y = y,
        just = c("left", "centre")
      )
      # return the leaf object as a gTree, or its bounding box
      if (bb) {
        rv <- grid::unit.c(x + bbxl, x + bbxr, y + bbyb, y + bbyt)
      } else {
        rv <- grid::gTree(children = grid::gList(gsym, glab))
      }
      return(rv)
    },

    #' @description Find all the model variables of type \code{ModVar} that have
    #' been specified as values associated with this \code{LeafNode}. Includes
    #' operands of these \code{ModVar}s, if they are expressions.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # build list, possibly including duplicates
      iv <- c(private$node.utility)
      ov <- iv[which(is_class(iv, what = "ModVar"))]
      ev <- iv[which(is_class(iv, what = "ExprModVar"))]
      for (v in ev) {
        ov <- c(ov, unlist(v$operands()))
      }
      # return the unique list
      return(unique(ov))
    },

    #' @description Set the utility associated with the node.
    #' @param utility The utility that a user associates with being in the
    #' health state for the interval. Can be \code{numeric} or
    #' a type of \code{ModVar}. If the type is \code{numeric}, the allowed
    #' range is \code{-Inf} to 1, not NA; if it is of type \code{ModVar}, it is
    #' unchecked.
    #' @return Updated \code{Leaf} object.
    set_utility = function(utility) {
      # check and set utility
      abortif(
        rlang::is_missing(utility),
        !(
          (is.numeric(utility) && !is.na(utility) && utility <= 1.0) ||
            (inherits(utility, what = "ModVar"))
        ),
        message = paste(
          "Argument 'utility' must be provided, of type numeric or ModVar,",
          "and in range [-Inf,1} if numeric."
        ),
        class = "invalid_utility"
      )
      private$node.utility <- utility
      return(invisible(self))
    },

    #' @description Set the time interval associated with the node.
    #' @param interval The time interval over which the \code{utility}
    #' parameter applies, expressed as an R \code{difftime} object; default
    #' 1 year, not NA.
    #' @return Updated \code{Leaf} object.
    set_interval = function(interval) {
      # check and set interval
      abortif(
        rlang::is_missing(interval),
        is.na(interval),
        !inherits(interval, what = "difftime"),
        message = "Argument 'interval' must be of class 'difftime'.",
        class = "invalid_interval"
      )
      private$node.interval <- interval
      return(invisible(self))
    },

    #' @description Return the utility associated with being in the
    #' state for the interval.
    #' @return Utility (numeric value).
    utility = function() {
      u <- as_numeric(private$node.utility)
      return(u)
    },

    #' @description Return the interval associated with being in the state.
    #' @return Interval (as a \code{difftime}).
    interval = function() {
      return(private$node.interval)
    },

    #' @description Return the discounted quality adjusted life years
    #' associated with being in the state.
    #' @details
    #' The present value of utility at future time \eqn{t} can be expressed as
    #' \eqn{u_t = u_0 e^{-rt}}, where \eqn{u_0} is the utility at time
    #' \eqn{t = 0} and \eqn{r} is the discount rate for utility
    #' (O'Mahony, 2015), under the assumption of continuous discount. The
    #' quality adjusted life years (QALYs) gained by occupying the state for
    #' time \eqn{t'} is \deqn{\int_{0}^{t'} u_t dt.} For \eqn{r > 0}, the QALY
    #' gain is equal to \deqn{\frac{u_0}{r}(1 - e^{-rt'}),} and for
    #' \eqn{r=0}, it is \eqn{u_0 t}'. For example, at a discount rate of 3.5\%,
    #' the number of QALYs gained after occupying a state for 1 year with a
    #' present value utility of 0.75 is 0.983 \eqn{\times} 0.75 \eqn{\approx}
    #' 0.737 QALYs, and after 2 years the gain is 1.449 QALYs.
    #' @references{
    #'   O'Mahony JF, Newall AT, van Rosmalen J. Dealing with time in health
    #'   economic evaluation: methodological issues and recommendations for
    #'   practice. \emph{PharmacoEconomics} 2015;\strong{33}:1255--1268.
    #' }
    #' @return \acronym{QALY}.
    QALY = function() {
      tp <- as.numeric(private$node.interval, units = "days") / 365.25
      u0 <- self$utility()
      ru <- private$ru
      if (ru > 0.0) {
        Q <- (u0 / ru) * (1.0 - exp(-ru * tp))
      } else {
        Q <- u0 * tp
      }
      return(Q)
    }
  )
)
