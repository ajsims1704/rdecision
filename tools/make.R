# -----------------------------------------------------------------------------
# make.R
#
# Description:
# ============
# R script which uses the 'devtools' library to build the package.
#
# History:
# ========
# 14.05.2019. A.J. Sims.
# ----------------------------------------------------------------------------

rm(list=ls())

local({
  
  # set working directory to package source and define repository
  setwd("h:/GitHub/rdecision")
  repo <- "h:/Sources/repository/rdecision"

  # get version
  DESC <- read.dcf('DESCRIPTION')
  VERSION <- DESC[1,'Version']
  
  # build manual
  devtools::build_manual(pkg='.', path=repo)
  
  # build vignettes
  devtools::build_vignettes(pkg='.')
  
  # build documentation (identifies errors in documentation which can
  # upset the build process)
  devtools::document()

  # build source package with vignettes
  devtools::build(path=repo, binary=F, vignettes=T, manual=F)
  
  # build windows binary from the source package
  pkg <- paste(repo, '/rdecision_', VERSION, '.tar.gz', sep='')
  devtools::build(pkg=pkg, path=repo, binary=T)

})

