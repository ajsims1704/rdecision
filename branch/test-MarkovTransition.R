
test_that("initialize parameters are checked", {
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  r <- 0.1
  expect_silent(MarkovTransition$new(s1,s2))
  expect_error(MarkovTransition$new(s1,s2,r,label=42), class="non-string_label")
  n1 <- Node$new()
  expect_error(MarkovTransition$new(n1,s2,r), class="invalid_source")
  expect_error(MarkovTransition$new(s1,n1,r), class="invalid_target")
  expect_silent(MarkovTransition$new(s1,s2,r))
  expect_error(MarkovTransition$new(s1,s2,"0.5"), class="invalid_rate")
  expect_error(MarkovTransition$new(s1,s2,r,cost="200"), class="invalid_cost")
  expect_silent(MarkovTransition$new(s1,s2,r,label=""))
  expect_silent(MarkovTransition$new(s1,s2,r,label="mychance"))
  # check values are retrieved
  MT <- MarkovTransition$new(s1,s2,r=0.1,cost=20)
  expect_equal(MT$rate(),0.1)
  expect_equal(MT$cost(),20)
})

test_that("rates and costs can be modified", {
  # default
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  t <- MarkovTransition$new(s1, s2)
  expect_true(is.na(t$rate()))
  expect_equal(t$cost(), 0)
  # defined at creation
  t <- MarkovTransition$new(s1, s2, cost=42)
  expect_equal(t$cost(), 42)
  t$set_rate(2)  
  expect_equal(t$rate(), 2)
  t$set_cost(1000)
  expect_equal(t$cost(), 1000)
  # modvars
  c <- ConstModVar$new("c1", "GBP", 42)
  t <- MarkovTransition$new(s1, s2, cost=c)
  expect_equal(t$cost(), 42)
  t <- MarkovTransition$new(s1, s2)
  t$set_cost(c)
  expect_equal(t$cost(), 42)
})

test_that("ModVars are identified and their values are returned", {
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  fortytwo <- ConstModVar$new("fortytwo", "GBP", 42)
  risky <- BetaModVar$new("risky", "R", alpha=10, beta=90)
  # one modvar
  e <- MarkovTransition$new(s1,s2,r=0.1,cost=fortytwo,label="label")
  expect_equal(e$cost(), 42)
  mv <- e$modvars()
  expect_equal(length(mv),1)
  # two modvars
  e <- MarkovTransition$new(s1,s2,r=risky,cost=fortytwo,label="label")
  expect_equal(e$cost(), 42)
  expect_equal(e$rate(), 0.1)  # mean = 10/(10+90)
  mv <- e$modvars()
  expect_length(mv,2)
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_setequal(d, c("risky", "fortytwo"))
})
