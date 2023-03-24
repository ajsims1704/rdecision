# if graphviz is installed, create the .png file from the .gv file with the dot
# command line tool, otherwise fail gracefully and draw a placeholder image
pdot <- Sys.which("dot")
if (nchar(pdot["dot"]) > 0L) {
  system2(command = pdot["dot"], args = c("-Tpng", "-o", pngfile, dotfile))
} else {
  knitr::include_graphics(path = pngfile)
}  
  
  
  
placeholder <- function() {
  
  # create placeholder image
  grDevices::png(pngfile, width = 480L, height = 480L * 0.21)
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
}
