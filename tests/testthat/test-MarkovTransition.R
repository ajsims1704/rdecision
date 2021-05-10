
test_that("initialize parameters are checked", {
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  r <- 0.1
  expect_silent(MarkovTransition$new(s1,s2))
  expect_error(MarkovTransition$new(s1,s2,r,label=42), class="non-string_label")
  n1 <- Node$new()
  expect_error(MarkovTransition$new(n1,s2,r), class="invalid_source")
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

test_that("modvars are identified", {
  s1 <- MarkovState$new("s1")
  s2 <- MarkovState$new("s2")
  fortytwo <- ConstModVar$new("fortytwo", "GBP", 42)
  free <- ConstModVar$new("free", "GBP", 0)
  risky <- ConstModVar$new("risky", "R", 0.1)
  # one modvar
  e <- MarkovTransition$new(s1,s2,r=0.1,cost=fortytwo,label="label")
  mv <- e$modvars()
  expect_equal(length(mv),1)
  # two modvars
  e <- MarkovTransition$new(s1,s2,r=risky,cost=free,label="label")
  mv <- e$modvars()
  expect_length(mv,2)
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_setequal(d, c("risky", "free"))
})
