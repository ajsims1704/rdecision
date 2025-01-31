test_that("labels are checked", {
  expect_silent(ChanceNode$new())
  expect_error(ChanceNode$new(42L), class = "non-string_label")
  expect_silent(ChanceNode$new(""))
  expect_silent(ChanceNode$new("my node"))
})

test_that("graphical representation of the node is as expected", {
  n <- ChanceNode$new(label = "chance")
  grDevices::pdf(file = NULL)
  grid::grid.newpage()
  vp <- grid::viewport()
  grid::pushViewport(vp)
  x <- grid::unit(0.5, "npc")
  y <- grid::unit(0.5, "npc")
  bb <- n$grob(x = x, y = y, bb = TRUE)
  expect_s3_class(bb, "unit")
  expect_length(bb, 4L)
  ng <- n$grob(x = x, y = y)
  expect_s3_class(ng, "grob")
  grid::grid.draw(ng)
  rg <- grid::rectGrob(
    x = bb[[1L]], y = bb[[3L]],
    width = bb[[2L]] - bb[[1L]], height = bb[[4L]] - bb[[3L]],
    just = c("left", "bottom")
  )
  grid::grid.draw(rg)
  grid::popViewport()
  grDevices::dev.off()
})
