rm(list=ls())
devtools::load_all()
rmarkdown::render("../vignettes/Ex25.Rmd")
rm(list=ls())

