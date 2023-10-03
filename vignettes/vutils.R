# Vignette building utility functions. These are not part of the package, but
# are available for use by vignettes by reading the code chunks.

## @knitr gbp -----------------------------------------------------------------

#' @title Write a monetary value
#' @param x Monetary value
#' @param p Logical; if TRUE show value to nearest penny, cent etc. If FALSE
#' show it to the nearest pound, dollar, euro etc.
#' @noRd
gbp <- function(x, p = FALSE) {
  digits <- if (p) 2L else 0L
  s <- format(
    round(x, digits = digits),
    digits = NULL,
    nsmall = digits,
    scientific = FALSE,
    big.mark = ","
  )
  return(s)
}


## @knitr gv2png --------------------------------------------------------------

#' @title Create a placeholder image in a png file
#' @description Draws a rectangle with a diagonal using the grid package.
#' @param pngfile name of png file to create.
#' @param width width of image in pixels
#' @param height height of image in pixels
#' @return Name of the png file written to.
#' @noRd
placeholder <- function(pngfile = tempfile(fileext = ".png"), width = 480L,
                        height = 320L) {
  grDevices::png(pngfile, width = width, height = height)
  grid::grid.newpage()
  grid::grid.move.to(
    x = grid::unit(0.0, "npc"),
    y = grid::unit(0.0, "npc")
  )
  grid::grid.line.to(
    x = grid::unit(1.0, "npc"),
    y = grid::unit(1.0, "npc")
  )
  grid::grid.move.to(
    x = grid::unit(0.0, "npc"),
    y = grid::unit(1.0, "npc")
  )
  grid::grid.line.to(
    x = grid::unit(1.0, "npc"),
    y = grid::unit(0.0, "npc")
  )
  invisible(grDevices::dev.off())
  return(pngfile)
}

#' @title Render a DOT format graph as a png file.
#' @description Uses the \code{dot} command line tool from the graphviz project,
#' if it is available on the host system, or creates a placeholder image if not.
#' @param dot GraphViz dot representation in character vector form.
#' @param pngfile path of png file to create.
#' @return pathname of the png file created (including extension).
#' @noRd
gv2png <- function(dot, pngfile = tempfile(fileext = ".png")) {
  cmddot <- Sys.which("dot")
  if (nchar(cmddot["dot"]) > 0L) {
    dotfile <- tempfile(fileext = ".gv")
    writeLines(dot, con = dotfile)
    system2(command = cmddot["dot"], args = c("-Tpng", "-o", pngfile, dotfile))
  } else {
    pngfile <- placeholder(pngfile = pngfile)
  }
  return(pngfile)
}
