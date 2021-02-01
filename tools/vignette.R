rm(list=ls())
devtools::load_all()
rmarkdown::render("vignettes/GT01-NewScientistPuzzle.Rmd")
rm(list=ls())

