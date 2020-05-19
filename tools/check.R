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
  
  # set working directory to package source 
  #setwd("h:/GitHub/rdecision")

  # add pandoc to PATH
  PATH <- Sys.getenv("PATH")
  PANDOC <- Sys.getenv("RSTUDIO_PANDOC")
  PATH <- paste(PATH, PANDOC, sep=";")
  Sys.setenv(PATH=PATH)
  print(Sys.getenv())
  
  # run the check
  devtools::check(
    pkg="h:/GitHub/rdecision",
    document=TRUE,
    manual=TRUE,
    cran=TRUE,
    incoming=T
  )
  
})
