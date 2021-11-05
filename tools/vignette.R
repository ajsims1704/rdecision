rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/DT00-DecisionTreeTutorial.Rmd")
rm(list=ls())


