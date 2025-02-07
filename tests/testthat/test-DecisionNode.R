
test_that("missing, non-character or empty labels are rejected", {
  expect_error(DecisionNode$new(), class = "invalid_label")
  expect_error(DecisionNode$new(42L), class = "invalid_label")
  expect_error(DecisionNode$new(""), class = "invalid_label")
  expect_silent(DecisionNode$new("my node"))
})

test_that("syntactically invalid labels are corrected", {
  d <- DecisionNode$new("my_label")
  expect_identical(d$label(), "my_label")
  d <- DecisionNode$new("my label")
  expect_identical(d$label(), "my.label")
  d <- DecisionNode$new("2_label")
  expect_identical(d$label(), "X2_label")
  d <- DecisionNode$new("2.label")
  expect_identical(d$label(), "X2.label")
})

test_that("graphical representation of the node is as expected", {
  d <- DecisionNode$new(label = "label")
  grDevices::pdf(file = NULL)
  grid::grid.newpage()
  vp <- grid::viewport()
  grid::pushViewport(vp)
  x <- grid::unit(0.5, "npc")
  y <- grid::unit(0.5, "npc")
  bb <- d$grob(x = x, y = y, bb = TRUE)
  expect_s3_class(bb, "unit")
  expect_length(bb, 4L)
  dg <- d$grob(x = x, y = y)
  expect_s3_class(dg, "grob")
  grid::grid.draw(dg)
  rg <- grid::rectGrob(
    x = bb[[1L]], y = bb[[3L]],
    width = bb[[2L]] - bb[[1L]], height = bb[[4L]] - bb[[3L]],
    just = c("left", "bottom")
  )
  grid::grid.draw(rg)
  grid::popViewport()
  grDevices::dev.off()
})
