# -----------------------------------------------------------------------------
# check.R
#
# Description:
# ============
# R script which uses the 'devtools' library to check a package, equivalent
# to calling 'R CMD check rdecision' from the command line. 
#
# History:
# ========
# 19.05.2020. A.J. Sims. Created.
# ----------------------------------------------------------------------------

rm(list=ls())

local({
  
  # save original path
  OPATH <- Sys.getenv("PATH")
  
  # System dependencies
  if (grepl("Windows", Sys.getenv("OS"))) {
    HOME <- "H:"
    PATHSEP <- ";"
    # add Rtools to path to help find qpdf
    PATH <- Sys.getenv("PATH")
    RTOOLS <- paste("C:", "Rtools", "bin", sep="/")
    PATH <- paste(PATH, RTOOLS, sep=PATHSEP)
    Sys.setenv(PATH=PATH)
    # don't use GhostScript
    Sys.setenv(R_GSCMD="")
    Sys.setenv(GS_QUALITY="none")
  } else {
    HOME <- Sys.getenv("HOME")
    PATHSEP <- ":"
  }
  REPOSITORY <- paste(HOME, "GitHub", sep="/")

  # add pandoc to PATH, if it is installed
  PATH <- Sys.getenv("PATH")
  PANDOC <- Sys.getenv("RSTUDIO_PANDOC")
  if (nchar(PANDOC)>0) {
    PATH <- paste(PATH, PANDOC, sep=PATHSEP)
    Sys.setenv(PATH=PATH)
  }

  # run the check
  devtools::check(
    pkg = paste(REPOSITORY, "rdecision", sep="/"),
    document = TRUE,
    manual = TRUE,
    cran = TRUE,
    incoming = FALSE
  )

  # clear up
  Sys.setenv(PATH=OPATH)
    
})
