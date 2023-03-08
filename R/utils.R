#' @title rlang analogue of base::stopifnot with negated condition
#' @description If the value of any of the conditions is TRUE, execution is
#' halted via the \code{rlang::abort} system.
#' @details Raises the condition \code{rlang::rlang_error}, optionally with
#' subclass as defined by parameter \code{class}. The header of
#' the message is the \code{message} argument and the body is the first
#' expression that evaluated to \code{TRUE}.
#' @param ... A number of R expressions which should all evaluate to FALSE. If
#' any expression does not evaluate to a boolean value, it will be treated as
#' TRUE. If there are no expressions, no condition will be raised.
#' @param message The message to display (used as the condition header)
#' @param class Subclass of the condition
#' @noRd
abortif <- function(..., message = NULL, class = NULL, 
                    call = rlang::caller_env()) {
  # get the arguments as a list of quosures before they are evaluated
  qargs <- rlang::quos(...)
  # test each argument
  for (i in seq_len(...length())) {
    arg <- ...elt(i)
    if (!rlang::is_logical(arg) || arg) {
      rlang::abort(
        message = c(
          ifelse(!is.null(message), message, ""), 
          paste(
            rlang::expr_deparse(rlang::quo_get_expr(qargs[[i]])), 
            "is not FALSE"
          )
        ), 
        class = class,
        call = call
      )
    }
  }
}

#' @title rlang analogue of base::stopifnot
#' @description If the value of any of the conditions is FALSE, execution is
#' halted via the \code{rlang::abort} system.
#' @details Raises the condition \code{rlang::rlang_error}, optionally with
#' subclass as defined by parameter \code{class}. The header of
#' the message is the \code{message} argument and the body is the first
#' expression that evaluated to \code{FALSE}.
#' @param ... A number of R expressions which should all evaluate to TRUE. If
#' any expression does not evaluate to a boolean value, it will be treated as
#' FALSE. If there are no expressions no condition will be raised.
#' @param message The message to display (used as the condition header)
#' @param class Subclass of the condition
#' @noRd
abortifnot <- function(..., message = NULL, class = NULL, 
                       call = rlang::caller_env()) {
  # get the arguments as a list of quosures before they are evaluated
  qargs <- rlang::quos(...)
  # test each argument
  for (i in seq_len(...length())) {
    arg <- ...elt(i)
    if (!rlang::is_logical(arg) || !arg) {
      rlang::abort(
        message = c(
          ifelse(!is.null(message), message, ""), 
          paste(
            rlang::expr_deparse(rlang::quo_get_expr(qargs[[i]])), 
            "is not TRUE"
          )
        ), 
        class = class,
        call = call
      )
    }
  }
}

#' @title The numeric value of a \code{ModVar} or numeric object
#' @description Intended as an internal function to \pkg{rdecision}.
#' @details The function has similar behaviour to \code{base:is.numeric}, in
#' that it coerces values of type \code{integer} to \code{numeric}.
#' @param x An object, possibly a vector.
#' @return A numeric vector of the same length as \var{x}, with each element
#' equal to the value from the \code{get} method if the corresponding element
#' of \var{x} is a \code{ModVar}, its numeric value if \var{x} is \code{numeric}
#' or \code{integer} and \code{NA} otherwise.
#' @noRd
as_numeric <- function(x) {
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
      rv[[i]] <- NA_real_
    }
  }
  return(rv)
}

#' @title Is an object of a given class?
#' @description Tests whether an object inherits from a given class, as 
#' detectable via \code{inherits}. Intended for internal use with 
#' \pkg{rdecision} and is the template for specific \var{is_X} convenience
#' functions. 
#' @param x An object to test, possibly a vector.
#' @param class_name A character string giving the name of the class.
#' @return A logical vector of the same length as \var{x} with TRUE values if
#' the corresponding element of \var{x} inherits from \code{classname}.
#' @noRd
is_class <- function(x, class_name) {
  # if scalar, coerce to a vector
  if (!is.vector(x)) {
    x <- c(x)
  }
  # prepare return value
  rv <- vector(mode = "logical", length = length(x))
  # test each element
  for (i in seq_along(x)) {
    rv[i] <- inherits(x[[i]], what = class_name)
  }
  return(rv)
}

#' @title Is an object an Arrow?
#' @description Tests whether an object inherits from class \class{Arrow} or a
#' class derived from it. Intended for internal use with \pkg{rdecision}.
#' @param x An object to test, possibly a vector.
#' @return A logical vector of the same length as \var{x} with TRUE values if
#' the corresponding element of \var{x} inherits from class \code{Arrow}.
#' @noRd
is_Arrow <- function(x) {
  return(is_class(x, "Arrow"))
}

#' @title Is an object a model variable?
#' @description Tests whether an object inherits from class \class{ModVar} or a
#' class derived from it. Intended for internal use with \pkg{rdecision}.
#' @param x An object to test, possibly a vector.
#' @return A logical vector of the same length as \var{x} with TRUE values if
#' the corresponding element of \var{x} inherits from class \code{ModVar}.
#' @noRd
is_ModVar <- function(x) {
  return(is_class(x, "ModVar"))
}
