

test_that("unlabelled actions are rejected", {
  n1 <- DecisionNode$new("d")
  n2 <- LeafNode$new("n2")
  expect_error(Action$new(n1,n2,42), class="non-string_label")
  expect_error(Action$new(n1,n2,""), class="empty_label")
  expect_silent(Action$new(n1,n2,"mychoice"))
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
  expect_equal(length(mv),1)
  # two modvars
  e <- Action$new(n1,n2,"label",cost=free,benefit=fortytwo)
  mv <- e$modvars()
  expect_equal(length(mv),2)
  d <- sapply(mv, function(v) {
    return(v$description())
  })
  expect_equal(d[order(d)], c("fortytwo", "free"))
})