# ---------------------------------------------------------------------------
# fprintf.R
#
# Description:
# ============
# A version of fprintf for R
# 
# History:
# ========
# 16.10.2013. A.J. Sims. Created.
# ---------------------------------------------------------------------------

#' @title A version of fprintf for R
#'
#' @description \code{fprintf} writes a formatted string to the file open on
#' connection \code{con}. It is intended to be equivalent to
#' the Matlab and C function of the same name. 
#' 
#' @param con an R connection, opened with the \link{open} command
#' or one its overloaded methods, such as \link{file} or \link{url}.
#' @param fmt a format string, of the syntax accepted by the R function
#' \link{sprintf}.
#' @param ... a list of arguments, matching those specified in the
#' \code{fmt} argument,, to be written to the connection.
#' @return The number of characters written to the connection.
#' @seealso \link{sprintf}
#' @examples  
#' ofid <- file("marvin.txt", open='wt')
#' fprintf(ofid, "The answer is %i\n", 42)
#' close(ofid)
#' @export
fprintf <- function(con, fmt, ...) {
  s <- sprintf(fmt, ...)
  cat(s, file=con)
  return(nchar(s))
}

