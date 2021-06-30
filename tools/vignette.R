rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/MC01-HIV.Rmd")
rm(list=ls())


