rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/Sutures.Rmd")
rm(list=ls())

