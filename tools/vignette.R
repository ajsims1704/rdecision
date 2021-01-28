rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/DT02-Tegaderm.Rmd")
rm(list=ls())

