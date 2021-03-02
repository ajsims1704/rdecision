#' @title \verb{LeafNode} class
#' 
#' @description 
#' An R6 class for a leaf node in a decision tree representing a clinical state.
#'
#' @details Represents a state of being, and is associated with an
#' incremental utility. 
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
LeafNode <- R6::R6Class(
  classname = "LeafNode",
  inherit = Node,
  private = list(
    node.utility = NULL,
    node.interval = NULL
  ),
  public = list(
    
    #' @description
    #' Create a new \verb{LeafNode} object; synonymous with a clinical outcome.
    #' @param label Character string; a label for the state; must be
    #' defined because it is used in tabulations. The label is automatically
    #' converted to a syntactically valid (in R) name to ensure it can be used
    #' as a column name in a data frame.
    #' @param utility The incremental utility that a user associates with
    #' being in the health state (range \verb{-Inf} to 1) for the interval.
    #' Intended for use with cost benefit analysis.
    #' @param interval The time interval over which the \verb{utility}
    #' parameter applies, expressed as an R \verb{difftime} object; default 
    #' 1 year.
    #' @return A new \verb{LeafNode} object
    initialize = function(
      label, utility=1, interval=as.difftime(365.25, units="days")
    ) {
      # check there is a label
      if (rlang::is_missing(label)) {
        rlang::abort(
          "Argument label must not be missing", 
          class="missing_label"
        )
      }
      # check label and make syntactically valid
      if (!is.character(label)) {
        rlang::abort(
          "Argument label must be a string", 
          class="non-string_label"
        )
      }
      if (nchar(label)==0) {
        rlang::abort(
          "Argument label must be defined", 
          class="empty_label"
        )
      }
      label <- make.names(label)
      # ensure base class is initialized
      super$initialize(label)
      # check and set utility
      if (is.numeric(utility)) {
        if (utility > 1) {
          rlang::abort(
            "Argument 'utility' must be in the range [-Inf,1].",
            class = "invalid_utility"
          )
        } else {
          private$node.utility <- utility
        }
      } else if (inherits(utility, what="ModVar")) {
        # TODO: consider checking distribution is appropriate
        private$node.utility <- utility
      } else {
        rlang::abort(
          "Argument 'utility' must be numeric or ModVar",
          class = "invalid_utility"
        )
      }
      # check and set the interval
      if (class(interval) != "difftime") {
        rlang::abort(
          "Argument 'interval' must be of class 'difftime'.",
          class = "invalid_interval"
        )
      }
      private$node.interval <- interval
      # return updated node object
      return(invisible(self))
    },

    #' @description 
    #' Find all the model variables of type \verb{ModVar} that have been 
    #' specified as values associated with this \verb{LeafNode}. Includes 
    #' operands of these \verb{ModVar}s, if they are expressions.
    #' @return A list of \verb{ModVar}s.
    modvars = function() {
      # create lists of input variables and output ModVars
      iv <- c(private$node.utility)
      ov <- list()
      sapply(iv, function(v) {
        if (inherits(v, what="ModVar")) {
          ov <<- c(ov, v)
          if (inherits(v, what="ExprModVar")) {
            ov <<- c(ov, v$operands())
          } 
        }
      })
      # return the unique list
      return(unique(ov))
    },

    #' @description 
    #' Return the incremental utility associated with being in the state for
    #' the interval.
    #' @return Incremental utility (numeric value).
    utility = function() {
      if (inherits(private$node.utility, what="ModVar")) {
        rv <- private$node.utility$get()
      } else {
        rv <- private$node.utility
      }
      return(rv)
    },

    #' @description 
    #' Return the interval associated with being in the state.
    #' @return Interval (as a \verb{difftime}).
    interval = function() {
      return(private$node.interval)
    },
    
    #' @description 
    #' Return the quality adjusted life years associated with being in 
    #' the state.
    #' @return \acronym{QALY}.
    QALY = function() {
      dt <- as.numeric(private$node.interval, units="days")
      Q <- dt * self$utility() / 365.25
      return(Q)
    }
    
    
  )
)
