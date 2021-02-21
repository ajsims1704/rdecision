rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/DT01-Sumatriptan.Rmd")
rm(list=ls())

