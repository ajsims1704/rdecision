
test_that("initialize parameters are checked", {
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  expect_silent(Transition$new(s1,s2))
  expect_error(Transition$new(s1,s2,label=42), class="non-string_label")
  n1 <- Node$new()
  expect_error(Transition$new(n1,s2), class="invalid_source")
  expect_error(Transition$new(s1,n1), class="invalid_target")
  expect_silent(Transition$new(s1,s2))
  expect_error(Transition$new(s1,s2,cost="200"), class="invalid_cost")
  expect_silent(Transition$new(s1,s2,label=""))
  expect_silent(Transition$new(s1,s2,label="mychance"))
  # check values are retrieved
  MT <- Transition$new(s1,s2,cost=20)
  expect_equal(MT$cost(),20)
})

test_that("costs can be modified", {
  # default
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  t <- Transition$new(s1, s2)
  expect_equal(t$cost(), 0)
  # defined at creation
  t <- Transition$new(s1, s2, cost=42)
  expect_equal(t$cost(), 42)
  t$set_cost(1000)
  expect_equal(t$cost(), 1000)
  # modvars
  c <- ConstModVar$new("c1", "GBP", 42)
  t <- Transition$new(s1, s2, cost=c)
  expect_equal(t$cost(), 42)
  t <- Transition$new(s1, s2)
  t$set_cost(c)
  expect_equal(t$cost(), 42)
})

test_that("ModVars are identified and their values are returned", {
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  fortytwo <- ConstModVar$new("fortytwo", "GBP", 42)
  # one modvar
  e <- Transition$new(s1,s2,cost=fortytwo,label="label")
  expect_equal(e$cost(), 42)
  mv <- e$modvars()
  expect_equal(length(mv),1)
  # expression modvar
  discount <- ConstModVar$new("discount", "rate", 0.1)
  dcost <- ExprModVar$new(
    "true cost", "GPB", 
    rlang::quo(fortytwo*(1-discount))
  )
  e <- Transition$new(s1,s2,cost=dcost,label="label")
  expect_equal(e$cost(), 42*0.9)
  mv <- e$modvars()
  expect_equal(length(mv),3)
})
