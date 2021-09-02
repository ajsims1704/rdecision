rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/SM01-HIV.Rmd")
rm(list=ls())


