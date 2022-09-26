
test_that("initialize parameters are checked", {
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  expect_silent(Transition$new(s1, s2))
  expect_error(Transition$new(s1, s2, label = 42L), class = "non-string_label")
  n1 <- Node$new()
  expect_error(Transition$new(n1,s2), class="invalid_source")
  expect_error(Transition$new(s1,n1), class="invalid_target")
  expect_silent(Transition$new(s1,s2))
  expect_error(Transition$new(s1,s2,cost="200"), class="invalid_cost")
  expect_silent(Transition$new(s1,s2,label=""))
  expect_silent(Transition$new(s1,s2,label="mychance"))
  # check values are retrieved
  MT <- Transition$new(s1, s2, cost = 20.0)
  expect_identical(MT$cost(), 20.0)
})

test_that("argument name matching supports old names", {
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  expect_silent(Transition$new(source = s1, target = s2))
  expect_silent(Transition$new(source = s1, target_state = s2))
  expect_silent(Transition$new(source_state = s1, target = s2))
})

test_that("costs can be modified", {
  # default
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  t <- Transition$new(s1, s2)
  expect_identical(t$cost(), 0.0)
  # defined at creation
  t <- Transition$new(s1, s2, cost = 42.0)
  expect_identical(t$cost(), 42.0)
  t$set_cost(1000.0)
  expect_identical(t$cost(), 1000.0)
  # modvars
  c <- ConstModVar$new("c1", "GBP", 42.0)
  t <- Transition$new(s1, s2, cost = c)
  expect_identical(t$cost(), 42.0)
  t <- Transition$new(s1, s2)
  t$set_cost(c)
  expect_identical(t$cost(), 42.0)
})

test_that("ModVars are identified and their values are returned", {
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  fortytwo <- ConstModVar$new("fortytwo", "GBP", 42.0)
  # one modvar
  e <- Transition$new(s1, s2, cost = fortytwo, label = "label")
  expect_equal(e$cost(), 42.0)
  mv <- e$modvars()
  expect_length(mv, 1L)
  # expression modvar
  discount <- ConstModVar$new("discount", "rate", 0.1)
  dcost <- ExprModVar$new(
    "true cost", "GPB", 
    rlang::quo(fortytwo * (1.0 - discount))
  )
  e <- Transition$new(s1, s2, cost = dcost, label = "label")
  expect_identical(e$cost(), 42.0 * 0.9)
  mv <- e$modvars()
  expect_length(mv, 3L)
})
