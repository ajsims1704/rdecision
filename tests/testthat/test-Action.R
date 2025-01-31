test_that("new object arguments are checked", {
  d1 <- DecisionNode$new("d1")
  t1 <- LeafNode$new("t1")
  expect_error(
    Action$new(t1, d1, cost = "200", label = "e"),
    class = "invalid_source"
  )
  expect_error(Action$new(d1, t1, 42L), class = "invalid_label")
  expect_error(Action$new(d1, t1, ""), class = "invalid_label")
  expect_error(
    Action$new(d1, t1, cost = "200", label = "e"),
    class = "invalid_cost"
  )
  expect_error(
    Action$new(d1, t1, benefit = "200", label = "e"),
    class = "invalid_benefit"
  )
  expect_silent(Action$new(d1, t1, "mychoice"))
})

test_that("class parameters are updated dynamically", {
  # create edge and check it
  d1 <- DecisionNode$new("d1")
  t1 <- LeafNode$new("t1")
  e1 <- Action$new(source = d1, target = t1, label = "e")
  expect_identical(e1$cost(), 0.0)
  expect_identical(e1$benefit(), 0.0)
  # attempt to set an invalid cost
  expect_error(e1$set_cost("42"), class = "invalid_cost")
  expect_error(e1$set_cost(NA_real_), class = "invalid_cost")
  # set cost to default, and check it
  e1$set_cost()
  expect_identical(e1$cost(), 0.0)
  # set cost and check it
  e1$set_cost(42.0)
  expect_identical(e1$cost(), 42.0)
  # attempt to set an invalid benefit
  expect_error(e1$set_benefit("42"), class = "invalid_benefit")
  expect_error(e1$set_benefit(NA_real_), class = "invalid_benefit")
  # set benefit to default, and check it
  e1$set_benefit()
  expect_identical(e1$benefit(), 0.0)
  # set cost and check it
  e1$set_benefit(42.0)
  expect_identical(e1$benefit(), 42.0)
})

test_that("argument name matching supports old and new parameter names", {
  d1 <- DecisionNode$new("d1")
  t1 <- LeafNode$new("t1")
  expect_silent(Action$new(source = d1, target = t1, label = "e"))
  expect_silent(Action$new(d1, target_node = t1, label = "e"))
  expect_silent(Action$new(source = d1, target_node = t1, label = "e"))
  expect_silent(Action$new(s = d1, t = t1, label = "e"))
})

test_that("conditional probability is 1", {
  n1 <- DecisionNode$new("d")
  n2 <- LeafNode$new("n2")
  e1 <- Action$new(n1, n2, "mychoice")
  expect_intol(e1$p(), 1.0, 0.01)
})

test_that("modvars are identified", {
  n1 <- DecisionNode$new("d")
  n2 <- LeafNode$new("n2")
  fortytwo <- ConstModVar$new("fortytwo", "GBP", 42.0)
  free <- ConstModVar$new("free", "GBP", 0.0)
  # one modvar
  e <- Action$new(n1, n2, "label", cost = 42.0, benefit = fortytwo)
  mv <- e$modvars()
  expect_length(mv, 1L)
  # two modvars
  e <- Action$new(n1, n2, "label", cost = free, benefit = fortytwo)
  mv <- e$modvars()
  expect_length(mv, 2L)
  d <- vapply(X = mv, FUN.VALUE = "x", FUN = function(v) {
    return(v$description())
  })
  expect_setequal(d, c("fortytwo", "free"))
  expect_intol(e$benefit(), 42.0, 0.01)
  # add a modvar dynamically
  e <- Action$new(
    n1, n2, cost = 42.0, benefit = fortytwo, label = "label"
  )
  mv <- e$modvars()
  expect_length(mv, 1L)
  e$set_cost(c = free)
  mv <- e$modvars()
  expect_length(mv, 2L)
  # add an expression model variable with two operands
  o1 <- ConstModVar$new("o1", "GBP", 100.0)
  o2 <- ConstModVar$new("o2", "GBP", 200.0)
  c1 <- ExprModVar$new("o1+o2", "GBP", rlang::quo(o1 + o2))
  e$set_cost(c = c1)
  mv <- e$modvars()
  expect_length(mv, 4L)
  d <- vapply(X = mv, FUN.VALUE = "x", FUN = function(v) {
    return(v$description())
  })
  expect_setequal(d, c("fortytwo", "o1+o2", "o1", "o2"))
})

test_that("graphical representation of the edge is as expected", {
  # source and target nodes
  n1 <- DecisionNode$new("d")
  n2 <- LeafNode$new("n2")
  # one modvar
  e <- Action$new(n1, n2, "label")
  grDevices::pdf(file = NULL)
  grid::grid.newpage()
  vp <- grid::viewport()
  grid::pushViewport(vp)
  xs <- grid::unit(0.25, "npc")
  ys <- grid::unit(0.5, "npc")
  xt <- grid::unit(0.75, "npc")
  yt <- grid::unit(0.75, "npc")
  eg <- e$grob(xs = xs, ys = ys, xt = xt, yt = yt)
  expect_s3_class(eg, "grob")
  grid::grid.draw(eg)
  grid::popViewport()
  grDevices::dev.off()
})
