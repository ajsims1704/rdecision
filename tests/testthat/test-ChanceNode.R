

test_that("missing or empty labels are accepted; non-character labels are rejected", {
  expect_silent(ChanceNode$new())
  expect_error(ChanceNode$new(42), class="non-string_label")
  expect_silent(ChanceNode$new(""))
  expect_silent(ChanceNode$new("my node"))
})


# test of creating a chance node
# test_that("child nodes are accepted and rejected correctly", {
#   s1 <- State$new("state 1")
#   s2 <- State$new("state 2")
#   labels <- c("to 1", "to 2")
#   p <- c(0.5, 0.5)
#   expect_error(ChanceNode$new(c(s1), labels, p, ), 
#                class="incorrect_child_count")
#   expect_error(ChanceNode$new(c(s1, TRUE), labels, p), 
#                class="non-Node_child")
#   c <- ChanceNode$new(c(s1,s2), labels, p)
#   expect_equal(c$node_type(), "ChanceNode")
#   expect_true(inherits(c, what="ChanceNode"))
#   expect_true(c$has_child_nodes())
#   expect_equal(length(c$child_nodes()), 2)
# })

# test_that("edge labels are accepted and rejected correctly", {
#   s1 <- State$new("state 1")
#   s2 <- State$new("state 2")
#   labels <- c("to 1", "to 2")
#   p <- c(0.5, 0.5)
#   expect_error(ChanceNode$new(c(s1, s2), c("to 1"), p),
#                class="incorrect_edge_label_count")
#   expect_error(ChanceNode$new(c(s1, s2), list("to 1", 42), p),
#                 class="non-string_edge_label")
#   expect_silent(ChanceNode$new(c(s1, s2), c("to 1", 42), p))
#   c <- ChanceNode$new(c(s1, s2), labels, p)
#   expect_equal(c$get_label(s1), "to 1")
#   expect_equal(c$get_label(s2), "to 2")
# })

# test_that("probabilities are accepted and rejected correctly", {
#   s1 <- State$new("state 1")
#   s2 <- State$new("state 2")
#   labels <- c("to 1", "to 2")
#   p <- c(0.5, 0.5)
#   expect_error(ChanceNode$new(c(s1,s2), labels, p, ptype=TRUE),
#                class="non-string_ptype")  
# })
