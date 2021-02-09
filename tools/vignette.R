rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/DT03-ShaleGas.Rmd")
rm(list=ls())

