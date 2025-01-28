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

#' @title Write a monetary value
#' @description Formats a number, or list of numbers, into currency.
#' @details If x is defined using the c() operator and contains one or more
#' character elements, all elements of x will be coerced to characters, and this
#' function will return "NA" for all elements. It is safer to define x as a
#' list, in which all non-numeric elements will be translated to "NA".
#' @param x Monetary value, or list of values
#' @param p Logical; if TRUE show value to nearest penny, cent etc. If FALSE
#' show it to the nearest pound, dollar, euro etc.
#' @param char Logical; if TRUE format the currency values into a character
#' vector, with pretty printing, including comma separators for thousands,
#' otherwise convert to rounded numeric values.
#' @returns A character vector with pretty formatted currency values (if
#' \code{char} is TRUE), or a vector of rounded numeric values.
#' @export
gbp <- function(x, p = FALSE, char = TRUE) {
  nums <- vapply(X = x, FUN.VALUE = TRUE, FUN = is.numeric)
  x[which(!nums)] <- NA_real_
  digits <- if (p) 2L else 0L
  x <- vapply(X = x, FUN.VALUE = 1.0, FUN = round, digits = digits)
  if (char) {
    x <- format(
      x = x,
      trim = TRUE,
      digits = NULL,
      nsmall = digits,
      scientific = FALSE,
      big.mark = ","
    )
  }
  return(x)
}

#' @title Is an object of a given class?
#' @description Tests whether an object inherits from a given class, as
#' detectable via \code{inherits}. Intended for internal use with
#' \pkg{rdecision} and is the template for specific \var{is_X} convenience
#' functions.
#' @param x An object to test, possibly a vector.
#' @param what A character vector giving the name(s) of the class.
#' @return A logical vector of the same length as \var{x} with TRUE values if
#' the corresponding element of \var{x} inherits from any of the elements of
#' \code{what}.
#' @noRd
is_class <- function(x, what) {
  # if scalar, coerce to a vector
  if (!is.vector(x)) {
    x <- c(x)
  }
  # prepare return value
  rv <- vector(mode = "logical", length = length(x))
  # test each element
  for (i in seq_along(x)) {
    rv[[i]] <- inherits(x[[i]], what = what)
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

#' @title Draw a tornado diagram.
#' @description A function to plot a tornado diagram, given a data frame of
#' model variable names, their confidence limits and the values of the outcome
#' variable for each. The outcome measure (x axis) is expected to be cost, ICER,
#' net monetary benefit, etc., but can be any meaningful quantity.
#' @param to A data frame with one row per horizontal bar of the tornado plot,
#' with columns:
#' \describe{
#'   \item{Description}{Description of the variable.}
#'   \item{Units}{Units of the variable.}
#'   \item{LL}{Lower limit of the variable.}
#'   \item{UL}{Upper limit of the variable.}
#'   \item{outcome.min}{Minimum value of the outcome measure.}
#'   \item{outcome.max}{Maximum value of the outcome measure.}
#' }
#' @param outcome_mean Mean (base case) outcome.
#' @param xlab Label for x axis
tornado_plot <- function(to, outcome_mean, xlab = "") {
  # check the arguments
  abortifnot(
    is.data.frame(to),
    all(is.element(
      c("Description", "Units", "LL", "UL", "outcome.min", "outcome.max"),
      names(to)
    )),
    is.character(to[, "Description"]),
    is.character(to[, "Units"]),
    is.numeric(to[, "LL"]),
    is.numeric(to[, "UL"]),
    is.numeric(to[, "outcome.min"]),
    is.numeric(to[, "outcome.max"]),
    all(complete.cases(to)),
    message = "'to' must be a data frame with specific columns and types",
    class = "invalid_parameter"
  )
  # make labels (description + units)
  to[, "Label"] <- paste(to[, "Description"], to[, "Units"], sep = ", ")
  # width of the plot in inches
  dsize <- grDevices::dev.size(unit = "in")
  figw <- dsize[[1L]]
  # width of the left outer margin as a proportion of figw
  louter <- 0.4
  # size of the inner margins as lines of text (0.2 inches per line)
  binner <- 4.1
  linner <- 2.1
  tinner <- 1.1
  rinner <- linner
  # create the plot frame
  withr::with_par(
    new = list(
      omi = c(0.0, figw * louter, 0.0, 0.0),
      mar = c(binner, linner, tinner, rinner),  # lines of text
      cex = 0.75
    ),
    code = {
      # set up the plot axes
      plot(
        x = NULL,
        y = NULL,
        xlim = c(
          min(min(to[, "outcome.min"]), min(to[, "outcome.max"])),
          max(max(to[, "outcome.min"]), max(to[, "outcome.max"]))
        ),
        ylim = c(0.5, nrow(to) + 0.5),
        xlab = xlab,
        ylab = "",
        yaxt = "n",
        frame.plot = FALSE
      )
      # find the longest label and scale text size accordingly
      lw <- max(graphics::strwidth(s = to[, "Label"], unit = "in"))
      lw <- lw + graphics::strwidth("MM", unit = "in")
      cex_axis <- min((louter * figw) / lw, 1.0)
      # label the y axis
      graphics::axis(
        side = 2L,
        at = seq_len(nrow(to)),
        labels = to$Label,
        lty = 0L,
        tick = FALSE,
        las = 2L,
        hadj = 1.0,
        cex.axis = cex_axis,
        outer = TRUE
      )
      # function to return a limit value (vectorized)
      limtxt <- function(x) {
        signif(x, 3L)
      }
      # find longest limit labels and adjust label text size
      lmin <- max(
        graphics::strwidth(s = limtxt(to[, "outcome.min"]), unit = "in")
      )
      lmax <- max(
        graphics::strwidth(s = limtxt(to[, "outcome.max"]), unit = "in")
      )
      lw <- max(lmin, lmax)
      # add bars and limits
      for (i in seq_len(nrow(to))) {
        xleft <- min(to[[i, "outcome.min"]], to[[i, "outcome.max"]])
        xright <- max(to[[i, "outcome.min"]], to[[i, "outcome.max"]])
        graphics::rect(
          xleft,
          xright,
          ybottom = i - 0.25,
          ytop = i + 0.25,
          border = "black",
          col = "lightgray",
          xpd = TRUE
        )
        ll <- to[[i, "LL"]]
        ul <- to[[i, "UL"]]
        if (to[[i, "outcome.max"]] > to[[i, "outcome.min"]]) {
          labels <- limtxt(c(ll, ul))
        } else {
          labels <- limtxt(c(ul, ll))
        }
        graphics::text(
          x = c(xleft, xright),
          y = c(i, i),
          labels = labels,
          pos = c(2.0, 4.0),
          offset = 0.25,
          xpd = TRUE
        )
      }
      # add mean (base case)
      graphics::abline(v = outcome_mean, lty = "dashed")
      # remove label column
      to[, "Label"] <- NULL
    }
  )
}
