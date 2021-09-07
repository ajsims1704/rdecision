rm(list=ls())
devtools::load_all()
knitr::render_markdown("README.Rmd", strict=TRUE)
rm(list=ls())