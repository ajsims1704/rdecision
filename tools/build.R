# -----------------------------------------------------------------------------
# build.R
#
# Description:
# ============
# R script which uses the 'devtools' library to build the package, equivalent
# to calling 'R CMD build rdecision' from the command line. 
#
# History:
# ========
# 14.05.2019. A.J. Sims. Created.
# 21.05.2010. A.J. Sims. Updated for cross-platform use.
# ----------------------------------------------------------------------------

rm(list=ls())

local({
  
  # save original path
  OPATH <- Sys.getenv("PATH")
  owd <- getwd()
  
  # System dependencies
  if (grepl("Windows", Sys.getenv("OS"))) {
    HOME <- "H:"
    PATHSEP <- ";"
    # add Rtools to path to help find qpdf
    PATH <- Sys.getenv("PATH")
    RTOOLS <- paste("C:", "Rtools", "bin", sep="/")
    PATH <- paste(PATH, RTOOLS, sep=PATHSEP)
    Sys.setenv(PATH=PATH)
    # add Ghostscript to the path
    PATH <- Sys.getenv("PATH")
    RTOOLS <- paste("C:", "Program Files", "gs", "gs9.52", "bin", sep="/")
    PATH <- paste(PATH, RTOOLS, sep=PATHSEP)
    Sys.setenv(PATH=PATH)
  } else {
    HOME <- Sys.getenv("HOME")
    PATHSEP <- ":"
  }
  REPOSITORY <- paste(HOME, "GitHub", sep="/")
  setwd(REPOSITORY)

  # add pandoc to PATH, if it is installed
  PATH <- Sys.getenv("PATH")
  PANDOC <- Sys.getenv("RSTUDIO_PANDOC")
  if (nchar(PANDOC)>0) {
    PATH <- paste(PATH, PANDOC, sep=PATHSEP)
    Sys.setenv(PATH=PATH)
  }

  # build the package
  srcpkg <- devtools::build(
    pkg = paste(REPOSITORY, "rdecision", sep="/"),
    path = paste(REPOSITORY, "rdecision", "archive", sep="/"),
    manual = TRUE
  )
  
  # build a binary package from the source package
  devtools::build(
    pkg = srcpkg,
    path = paste(REPOSITORY, "rdecision", "archive", sep="/"),
    binary = TRUE,
    vignettes = TRUE,
    manual = TRUE
  )
  
  # build package manual
  devtools::build_manual(
    pkg = paste(REPOSITORY, "rdecision", sep="/"),
    path = paste(REPOSITORY, "rdecision", "archive", sep="/")
  )
  
  # clear up
  Sys.setenv(PATH=OPATH)
  setwd(owd)

})

