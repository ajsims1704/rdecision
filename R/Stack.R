#' @title A stack
#' @description An R6 class representing a stack of objects of any type.
#' @details Conventional implementation of a stack. Used extensively in graph
#' algorithms and offered as a separate class for ease of programming and to
#' ensure that implementations of stacks are optimized. By intention, there is
#' only minimal checking of method arguments. This is to maximize performance
#' and because the class is mainly intended for use internally to
#' \pkg{rdecision}.
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
Stack <- R6::R6Class(
  classname = "Stack",
  lock_class = TRUE,
  private = list(
    items = NULL
  ),
  public = list(

    #' @description Create a stack.
    #' @return A new \code{Stack} object.
    initialize = function() {
      private$items <- list()
      return(invisible(self))
    },

    #' @description Push an item onto the stack.
    #' @param x The item to push onto the top of the stack. It should be
    #' of the same class as items previously pushed on to the stack. It is not
    #' checked.
    #' @return An updated \code{Stack} object
    push = function(x) {
      private$items[[length(private$items) + 1L]] <- x
      return(invisible(self))
    },

    #' @description Pop an item from the stack. Stack underflow and
    #' raises an error.
    #' @return The item previously at the top of the stack.
    pop = function() {
      if (length(private$items) > 0L) {
        x <- private$items[[length(private$items)]]
        private$items[[length(private$items)]] <- NULL
      } else {
        abortifnot(FALSE,
          message = "Stack underflow",
          class = "underflow"
        )
      }
      return(x)
    },

    #' @description Gets the number of items on the stack.
    #' @return Number of items.
    size = function() {
      return(length(private$items))
    },

    #' @description Inspect items in the stack.
    #' @return A list of items.
    as_list = function() {
      return(private$items)
    }
  )
)
