
test_that("initialize parameters are checked", {
  c1 <- ChanceNode$new("c1")
  t1 <- LeafNode$new("t1")
  p <- 0.5
  expect_error(Reaction$new(c1,t1,p,label=42), class="non-string_label")
  expect_error(Reaction$new(t1,c1,p), class="invalid_source")
  expect_silent(Reaction$new(c1,t1,p))
  expect_error(Reaction$new(c1,t1,"0.5"), class="invalid_p")
  expect_error(Reaction$new(c1,t1,p,cost="200"), class="invalid_cost")
  expect_error(Reaction$new(c1,t1,p,benefit="200"), class="invalid_benefit")
  expect_silent(Reaction$new(c1,t1,p,label=""))
  expect_silent(Reaction$new(c1,t1,p,label="mychance"))
})

test_that("modvars are identified", {
  n1 <- ChanceNode$new("c")
  n2 <- LeafNode$new("n2")
  fortytwo <- ConstModVar$new("fortytwo", "GBP", 42)
  free <- ConstModVar$new("free", "GBP", 0)
  evens <- ConstModVar$new("evens", "P", 0.5)
  # one modvar
  e <- Reaction$new(n1,n2,p=0.5,cost=42,benefit=fortytwo,label="label")
  mv <- e$modvars()
  expect_equal(length(mv),1)
  # two modvars
  e <- Reaction$new(n1,n2,p=0.5,cost=free,benefit=fortytwo,label="label")
  mv <- e$modvars()
  expect_length(mv,2)
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_setequal(d, c("fortytwo", "free"))
  # three modvars
  e <- Reaction$new(n1,n2,p=evens,cost=free,benefit=fortytwo,label="label")
  mv <- e$modvars()
  expect_length(mv,3)
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_setequal(d, c("evens", "fortytwo", "free"))
  expect_equal(e$benefit(),42)
})
