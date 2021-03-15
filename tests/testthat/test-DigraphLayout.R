

test_that("argument types are checked", {
  expect_error(DigraphLayout$new())
  expect_error(DigraphLayout$new(42))
  expect_error(DigraphLayout$new("G"))
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1,n2)
  e2 <- Edge$new(n1,n3)
  G <- Graph$new(V=list(n1,n2,n3), E=list(e1, e2))
  expect_error(DigraphLayout$new(G))
  e1 <- Arrow$new(n1,n2)
  e2 <- Arrow$new(n1,n3)
  G <- Digraph$new(V=list(n1,n2,n3), A=list(e1, e2))
  expect_silent(DigraphLayout$new(G))
})

