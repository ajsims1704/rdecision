

test_that("unlabelled actions are rejected", {
  d1 <- DecisionNode$new("d1")
  t1 <- LeafNode$new("t1")
  expect_error(Action$new(t1,d1,cost="200",label="e"), class="invalid_source")
  expect_error(Action$new(d1,t1,42), class="invalid_label")
  expect_error(Action$new(d1,t1,""), class="empty_label")
  expect_error(Action$new(d1,t1,cost="200",label="e"), class="invalid_cost")
  expect_error(
    Action$new(d1,t1,benefit="200",label="e"), 
    class="invalid_benefit"
  )
  expect_silent(Action$new(d1,t1,"mychoice"))
})

test_that("conditional probability is 1", {
  n1 <- DecisionNode$new("d")
  n2 <- LeafNode$new("n2")
  e1 <- Action$new(n1,n2,"mychoice")
  expect_equal(e1$p(),1)
})

test_that("modvars are identified", {
  n1 <- DecisionNode$new("d")
  n2 <- LeafNode$new("n2")
  fortytwo <- ConstModVar$new("fortytwo", "GBP", 42)
  free <- ConstModVar$new("free", "GBP", 0)
  # one modvar
  e <- Action$new(n1,n2,"label",cost=42,benefit=fortytwo)
  mv <- e$modvars()
  expect_length(mv,1)
  # two modvars
  e <- Action$new(n1,n2,"label",cost=free,benefit=fortytwo)
  mv <- e$modvars()
  expect_length(mv,2)
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_setequal(d, c("fortytwo", "free"))
  expect_equal(e$benefit(),42)
})
