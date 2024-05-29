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
    node.interval = NULL
  ),
  public = list(

    #' @description
    #' Create a new \code{LeafNode} object; synonymous with a clinical outcome.
    #' @param label Character string; a label for the state; must be
    #' defined because it is used in tabulations. The label is automatically
    #' converted to a syntactically valid (in R) name to ensure it can be used
    #' as a column name in a data frame.
    #' @param utility The incremental utility that a user associates with
    #' being in the health state for the interval.
    #' Intended for use with cost benefit analysis. Can be \code{numeric} or
    #' a type of \code{ModVar}. If the type is \code{numeric}, the allowed
    #' range is \code{-Inf} to 1; if it is of type \code{ModVar}, it is
    #' unchecked.
    #' @param interval The time interval over which the \code{utility}
    #' parameter applies, expressed as an R \code{difftime} object; default
    #' 1 year.
    #' @return A new \code{LeafNode} object
    initialize = function(
      label, utility = 1.0, interval = as.difftime(365.25, units = "days")
    ) {
      # check there is a valid label
      abortifnot(
        !rlang::is_missing(label),
        is.character(label),
        nchar(label) > 0L,
        message =
          "Argument `label` must not be missing and must be a non-empty string",
        class = "invalid_label"
      )
      # initialize Node base class
      label <- make.names(label)
      super$initialize(label)
      # check and set utility
      self$set_utility(utility)
      # check and set the interval
      self$set_interval(interval)
      # return updated node object
      return(invisible(self))
    },

    #' @description Find all the model variables of type \code{ModVar} that have
    #' been specified as values associated with this \code{LeafNode}. Includes
    #' operands of these \code{ModVar}s, if they are expressions.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # create lists of input variables and output ModVars
      iv <- c(private$node.utility)
      ov <- list()
      for (v in iv) {
        if (inherits(v, what = "ModVar")) {
          ov <- c(ov, v)
          if (inherits(v, what = "ExprModVar")) {
            for (o in v$operands()) {
              ov <- c(ov, o)
            }
          }
        }
      }
      # return the unique list
      return(unique(ov))
    },

    #' @description Set the incremental utility associated with the node.
    #' @param utility The incremental utility that a user associates with
    #' being in the health state for the interval.
    #' Intended for use with cost benefit analysis. Can be \code{numeric} or
    #' a type of \code{ModVar}. If the type is \code{numeric}, the allowed
    #' range is \code{-Inf} to 1, not NA; if it is of type \code{ModVar}, it is
    #' unchecked.
    #' @return Updated \code{Leaf} object.
    set_utility = function(utility) {
      # check and set utility
      abortif(
        rlang::is_missing(utility),
        !inherits(utility, what = c("numeric", "ModVar")),
        message = paste(
          "Argument 'utility' must be provided, and of type numeric or ModVar."
        ),
        class = "invalid_utility"
      )
      if (is.numeric(utility)) {
        abortif(
          is.na(utility),
          utility > 1.0,
          message = "Argument 'utility' must be in the range [-Inf,1].",
          class = "invalid_utility"
        )
      }
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

    #' @description Return the incremental utility associated with being in the
    #' state for the interval.
    #' @return Incremental utility (numeric value).
    utility = function() {
      u <- as_numeric(private$node.utility)
      return(u)
    },

    #' @description Return the interval associated with being in the state.
    #' @return Interval (as a \code{difftime}).
    interval = function() {
      return(private$node.interval)
    },

    #' @description Return the quality adjusted life years associated with
    #' being in the state.
    #' @return \acronym{QALY}.
    QALY = function() {
      dt <- as.numeric(private$node.interval, units = "days")
      Q <- dt * self$utility() / 365.25
      return(Q)
    }
  )
)
