
test_that("unlabelled reactions are accepted", {
  n1 <- ChanceNode$new("c")
  n2 <- LeafNode$new("n2")
  p <- 0.5
  expect_error(Reaction$new(n1,n2,p,label=42), class="non-string_label")
  expect_silent(Reaction$new(n1,n2,p))
  expect_silent(Reaction$new(n1,n2,p,label=""))
  expect_silent(Reaction$new(n1,n2,p,label="mychance"))
})
