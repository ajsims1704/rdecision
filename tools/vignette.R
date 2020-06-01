rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/Tegaderm.Rmd")
rm(list=ls())

