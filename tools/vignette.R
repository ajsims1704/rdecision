rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/GT00-BurgerRun.Rmd")
rm(list=ls())

