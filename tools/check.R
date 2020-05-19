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
  
  # add pandoc and qpdf to PATH
  OPATH <- Sys.getenv("PATH")
  PATH <- Sys.getenv("PATH")
  sep <- ifelse(grepl(":", PATH), ":", ";")
  PANDOC <- Sys.getenv("RSTUDIO_PANDOC")
  PATH <- paste(PATH, PANDOC, sep=sep)
  Sys.setenv(PATH=PATH)
  QPDF <- Sys.getenv("R_QPDF")
  PATH <- paste(PATH, QPDF, sep=sep)
  Sys.setenv(PATH=PATH)
  
  # run the check
  devtools::check(
    pkg = paste(Sys.getenv("HOME"), "GitHub/rdecision", sep="/"),
    document=TRUE,
    manual=TRUE,
    cran=TRUE,
    incoming=T
  )

  # clear up
  Sys.setenv(PATH=OPATH)
    
})
