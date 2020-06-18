rm(list=ls())
library("grid")
popViewport(0)

vp <- viewport(
  width = 30,
  height = 30,
  default.units = "mm"
)

# grid
for (x in seq(5,25,5)) {
  grid.move.to(x=unit(x,"mm"), y=unit(5,"mm"), vp=vp)
  grid.line.to(x=unit(x,"mm"), y=unit(25,"mm"), vp=vp)
}
for (y in seq(5,25,5)) {
  grid.move.to(x=unit(5,"mm"), y=unit(y,"mm"), vp=vp)
  grid.line.to(x=unit(25,"mm"), y=unit(y,"mm"), vp=vp)
}
# restaurants
BB <- data.frame(
  x0 = c(15,10,21,16,25,11,5),
  y0 = c(6,11,10,15,16,20,21),
  x1 = c(15,24,10,19,25,14,5),
  y1 = c(9,10,14,15,19,20,24)
)
apply(BB, MARGIN=1, function(r){
  grid.move.to(x=unit(r["x0"],"mm"), y=unit(r["y0"],"mm"), vp=vp)
  grid.line.to(x=unit(r["x1"],"mm"), y=unit(r["y1"],"mm"), gp=gpar(col="red"), vp=vp)
})


pushViewport(vp)


# grid.rect(gp = gpar(lty = "dashed"))
# vp1 <- viewport(x = 0, y = 0.5, w = 0.5, h = 0.5,
#  just = c("left", "bottom"), name = "vp1")
# vp2 <- viewport(x = 0.5, y = 0, w = 0.5, h = 0.5,
#  just = c("left", "bottom"))
# pushViewport(vp1)
# grid.rect(gp = gpar(col = "grey"))
# grid.text("Some drawing in graphics region 1", y = 0.8)
# upViewport()
# pushViewport(vp2)
# grid.rect(gp = gpar(col = "grey"))
# grid.text("Some drawing in graphics region 2", y = 0.8)
# upViewport()
# downViewport("vp1")
# grid.text("MORE drawing in graphics region 1", y = 0.2)
# popViewport()

popViewport()
