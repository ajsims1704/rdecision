
test_that("new object arguments are checked", {
  c1 <- ChanceNode$new("c1")
  t1 <- LeafNode$new("t1")
  p <- 0.5
  expect_error(Reaction$new(c1, t1))
  expect_error(Reaction$new(c1,t1,p,label = 42L), class = "non-string_label")
  expect_error(Reaction$new(t1,c1,p), class="invalid_source")
  expect_silent(Reaction$new(c1,t1,p))
  expect_error(Reaction$new(c1,t1,"0.5"), class="invalid_probability")
  expect_error(Reaction$new(c1,t1,p,cost="200"), class="invalid_cost")
  expect_error(Reaction$new(c1,t1,p,benefit="200"), class="invalid_benefit")
  expect_silent(Reaction$new(c1,t1,p,label=""))
  expect_silent(Reaction$new(c1,t1,p,label="mychance"))
})

test_that("class parameters are updated dynamically", {
  # create edge and check it
  c1 <- ChanceNode$new("d1")
  t1 <- LeafNode$new("t1")
  e1 <- Reaction$new(source = c1, target = t1, p = 0.5, label = "e")
  expect_identical(e1$cost(), 0.0)
  expect_identical(e1$benefit(), 0.0)
  expect_identical(e1$p(), 0.5)
  # attempt to set an invalid probability
  expect_error(e1$set_probability("0.42"), class = "invalid_probability")
  expect_error(e1$set_probability(), class = "invalid_probability")
  # set probability and check it
  e1$set_probability(0.42)
  expect_identical(e1$p(), 0.42)
  # attempt to set an invalid cost
  expect_error(e1$set_cost("42"), class = "invalid_cost")
  # set cost to default, and check it
  e1$set_cost()
  expect_identical(e1$cost(), 0.0)
  # set cost and check it
  e1$set_cost(42.0)
  expect_identical(e1$cost(), 42.0)
  # attempt to set an invalid benefit
  expect_error(e1$set_benefit("42"), class = "invalid_benefit")
  # set benefit to default, and check it
  e1$set_benefit()
  expect_identical(e1$benefit(), 0.0)
  # set cost and check it
  e1$set_benefit(42.0)
  expect_identical(e1$benefit(), 42.0)
})

test_that("argument name matching supports old names", {
  ns <- ChanceNode$new("c")
  nt <- LeafNode$new("l")
  expect_silent(Reaction$new(source = ns, target = nt, p = 0.5, label = "x"))
  expect_silent(
    Reaction$new(source = ns, target_node = nt, p = 0.5, label = "x")
  )
  expect_silent(
    Reaction$new(source_node = ns, target = nt, p = 0.5, label = "x")
  )
})

test_that("modvars are identified", {
  n1 <- ChanceNode$new("c")
  n2 <- LeafNode$new("n2")
  fortytwo <- ConstModVar$new("fortytwo", "GBP", 42.0)
  free <- ConstModVar$new("free", "GBP", 0.0)
  evens <- ConstModVar$new("evens", "P", 0.5)
  # one modvar
  e <- Reaction$new(
    n1, n2, p = 0.5, cost = 42.0, benefit = fortytwo, label = "label"
  )
  mv <- e$modvars()
  expect_length(mv, 1L)
  # two modvars
  e <- Reaction$new(
    n1, n2, p = 0.5, cost = free, benefit = fortytwo, label = "label"
  )
  mv <- e$modvars()
  expect_length(mv, 2L)
  d <- vapply(mv, FUN.VALUE = "x", FUN = function(v) {
    return(v$description())
  })
  expect_setequal(d, c("fortytwo", "free"))
  # three modvars
  e <- Reaction$new(
    n1, n2, p = evens, cost = free, benefit = fortytwo, label = "label"
  )
  mv <- e$modvars()
  expect_length(mv, 3L)
  d <- vapply(mv, FUN.VALUE = "x", FUN = function(v) {
    return(v$description())
  })
  expect_setequal(d, c("evens", "fortytwo", "free"))
  expect_identical(e$benefit(), 42.0)
  # add a modvar dynamically
  e <- Reaction$new(
    n1, n2, p = 0.5, cost = 42.0, benefit = fortytwo, label = "label"
  )
  mv <- e$modvars()
  expect_length(mv, 1L)
  e$set_cost(c = free)
  mv <- e$modvars()
  expect_length(mv, 2L)
})
