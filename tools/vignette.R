rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/DT04-Sutures.Rmd")
rm(list=ls())

