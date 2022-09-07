#' @title rlang analogue of base::stopifnot
#' @description If the value of a condition is not TRUE, execution is halted
#' via the \code{rlang::abort} system. There is minimal checking of the 
#' arguments because this function is intended to be used internally to the 
#' package.
#' @param cond A boolean value or expression which should be TRUE. If cond
#' does not evaluate to a boolean value, it will be treated as FALSE. 
#' @param message The message to display.
#' @param class Subclass of the condition
#' @noRd
abortifnot <- function(cond, message = NULL, class = NULL) {
  if (!rlang::is_logical(cond) || !cond) {
    rlang::abort(message = message, class = class)
  }
}

#' @title The numeric value of a \code{ModVar} or numeric object
#' @description Intended as an internal function to \pkg{rdecision}.
#' @param x An object, possibly a vector.
#' @return A numeric vector of the same length as \var{x}, with each element
#' equal to the value from the \code{get} method if the corresponding element
#' of \var{x} is a \code{ModVar}, its value if \var{x} is numeric or \code{NA}
#' otherwise.
#' @noRd
as_value <- function(x) {
  # coerce argument to a vector
  if (!is.vector(x)) {
    x <- c(x)
  }
  # find the ModVars in x
  mv <- is_ModVar(x)
  # create return object
  rv <- vector(mode = "numeric", length = length(mv))
  # extract values
  for (i in seq_along(rv)) {
    xi <- x[[i]]
    if (mv[[i]]) {
      rv[[i]] <- xi$get()
    } else if (is.numeric(xi)) {
      rv[[i]] <- xi
    } else {
      rv[[i]] <- as.numeric(NA)
    }
  }
  return(rv)
}

#' @title Is an object a model variable?
#' @description Tests whether an object inherits from class \class{ModVar} or a
#' class derived from \class{ModVar}. Intended for internal use with 
#' \pkg{rdecision}.
#' @param x An object to test, possibly a vector.
#' @return A logical vector of the same length as \var{x} with TRUE values if
#' the corresponding element of \var{x} inherits from class \code{ModVar}.
#' @noRd
is_ModVar <- function(x) {
  # if scalar, coerce to a vector
  if (!is.vector(x)) {
    x <- c(x)
  }
  # prepare return value
  rv <- vector(mode = "logical", length = length(x))
  # test each element
  for (i in seq_along(x)) {
    rv[i] <- inherits(x[[i]], what = "ModVar")
  }
  return(rv)
}
