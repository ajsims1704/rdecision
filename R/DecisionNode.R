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
  private = list(
  ),
  public = list(

    #' @description
    #' Create a new decision node.
    #' @param label A label for the node. Must be defined because the label is
    #' used in tabulation of strategies. The label is automatically converted
    #' to a syntactically valid (in R) name to ensure it can be used as a column
    #' name in a data frame.
    #' @return A new \code{DecisionNode} object.
    initialize = function(label) {
      # check there is a label
      if (rlang::is_missing(label)) {
        rlang::abort(
          "Argument label must not be missing",
          class = "missing_label"
        )
      }
      # check label and make syntactically valid
      if (!is.character(label)) {
        rlang::abort(
          "Argument label must be a string",
          class = "non-string_label"
        )
      }
      if (nchar(label) == 0L) {
        rlang::abort("Argument label must be defined", class = "empty_label")
      }
      label <- make.names(label)
      # ensure base class fields are initialized
      super$initialize(label)
      return(invisible(self))
    }
  )
)
