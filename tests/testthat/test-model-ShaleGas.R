# Script to construct, in rdecision, a decision tree with multiple decision
# nodes, as described by Kaminsky et al, Central European Journal of Operational
# Research 26, 135–159 (2018).
#
# The checks are done as part of the testthat framework, ensuring that
# changes in the package code which unintentionally result in deviations
# from the expected results of the model are identified.
#
# Code to construct and run the model is contained within labelled knitr code
# chunks and do not contain test expectations, so can be used by a vignette.
# Unlabelled code chunks may contain testthat expectations and should be
# ignored by a vignette.

## @knitr create-model --------------------------------------------------------
# nodes
d1 <- DecisionNode$new("d1")
d2 <- DecisionNode$new("d2")
d3 <- DecisionNode$new("d3")
c1 <- ChanceNode$new("c1")
c2 <- ChanceNode$new("c2")
c3 <- ChanceNode$new("c3")
c4 <- ChanceNode$new("c4")
t1 <- LeafNode$new("t1")
t2 <- LeafNode$new("t2")
t3 <- LeafNode$new("t3")
t4 <- LeafNode$new("t4")
t5 <- LeafNode$new("t5")
t6 <- LeafNode$new("t6")
t7 <- LeafNode$new("t7")
t8 <- LeafNode$new("t8")
t9 <- LeafNode$new("t9")
# probabilities
p.sens <- 0.9
p.spec <- 0.7
p.gas <- 0.7
p.nogas <- 1.0 - p.gas
p.ptest <- p.sens * p.gas + (1.0 - p.spec) * p.nogas
p.ntest <- (1.0 - p.sens) * p.gas + p.spec * p.nogas
p.gas.ptest <- p.sens * p.gas / p.ptest
p.gas.ntest <- (1.0 - p.sens) * p.gas / p.ntest
# edges
E <- list(
  Action$new(d1, t1, "sell", benefit = 800.0),
  Action$new(d1, c1, "dig", cost = 300.0),
  Reaction$new(c1, t2, p = p.gas, benefit = 2500.0, label = "gas"),
  Reaction$new(c1, t3, p = p.nogas, label = "no gas"),
  Action$new(d1, c2, "test", cost = 50.0),
  Reaction$new(c2, d2, p = p.ntest, label = "negative"),
  Action$new(d2, t4, "sell", benefit = 600.0),
  Action$new(d2, c3, "dig", cost = 300.0),
  Reaction$new(c3, t5, p = p.gas.ntest, benefit = 2500.0, label = "gas"),
  Reaction$new(c3, t6, p = (1.0 - p.gas.ntest), label = "no gas"),
  Reaction$new(c2, d3, p = p.ptest, label = "positive"),
  Action$new(d3, t7, "sell", benefit = 1000.0),
  Action$new(d3, c4, "dig", cost = 300.0),
  Reaction$new(c4, t8, p = p.gas.ptest, benefit = 2500.0, label = "gas"),
  Reaction$new(c4, t9, p = (1.0 - p.gas.ptest), label = "no gas")
)
# tree
V <- list(d1, d2, d3,  c1, c2, c3, c4,  t1, t2, t3, t4, t5, t6, t7, t8, t9)
DT <- DecisionTree$new(V, E)

## @knitr ----------------------------------------------------------------------
test_that("strategies are identified correctly", {
  # strategies
  expect_error(
    DT$strategy_table(select = list(E[[1L]], E[[2L]])),
    class = "invalid_strategy"
  )
  S <- DT$strategy_table("label")
  expect_setequal(
    colnames(S),
    list("d1", "d2", "d3")
  )
  expect_setequal(
    rownames(S),
    list(
      "sell_sell_sell", "sell_sell_dig", "sell_dig_sell", "sell_dig_dig",
      "dig_sell_sell", "dig_sell_dig", "dig_dig_sell", "dig_dig_dig",
      "test_sell_sell", "test_sell_dig", "test_dig_sell", "test_dig_dig"
    )
  )
  expect_identical(nrow(S), 12L)
  expect_identical(sum(S[, "d1"] == "sell"), 4L)
  expect_identical(sum(S[, "d2"] == "sell"), 6L)
  expect_identical(sum(S[, "d3"] == "sell"), 6L)
  # single strategy
  s <- list(E[[1L]], E[[7L]], E[[12L]]) # sell sell sell
  expect_true(DT$is_strategy(s))
  S <- DT$strategy_table("label", select = s)
  expect_identical(nrow(S), 1L)
  expect_identical(S[[1L, "d1"]], "sell")
  expect_identical(S[[1L, "d2"]], "sell")
  expect_identical(S[[1L, "d3"]], "sell")
  # test incorrect strategy prescription
  expect_false(DT$is_strategy(list(E[[1L]], E[[5L]], E[[7L]])))
  ddum <- DecisionNode$new("dummy")
  adum <- Action$new(source_node = ddum, target_node = t1, label = "dummy")
  expect_error(
    DT$is_strategy(list(E[[1L]], E[[7L]], adum)),
    class = "not_in_graph"
  )
})

## @knitr ----------------------------------------------------------------------
test_that("root to leaf paths are identified", {
  # evaluate all root-to-leaf paths
  P <- DT$root_to_leaf_paths()
  W <- lapply(P, DT$walk)
  expect_length(W, 9L)
  WX <- lapply(P, function(p) {
    pp <- p
    if (length(pp) > 2L) {
      pp[[length(pp)]] <- NULL
    }
    DT$walk(pp)
  })
  expect_error(DT$evaluate_walks(WX), class = "not_to_leaf")
  M <- DT$evaluate_walks(W)
  expect_identical(nrow(M), 9L)
  expect_identical(ncol(M), 9L)
  ileaf <- DT$vertex_index(list(t1, t2, t3, t4, t5, t6, t7, t8, t9))
  expect_setequal(rownames(M), as.character(ileaf))
  it8 <- DT$vertex_index(t8)
  expect_intol(M[[as.character(it8), "Cost"]], 220.5, 1.0)
})

## @knitr evaluate ------------------------------------------------------------
# find optimal strategies
RES <- DT$evaluate()
RES[, "Payoff"] <- RES[, "Benefit"] - RES[, "Cost"]

## @knitr ---------------------------------------------------------------------
test_that("tree identifies the optimal strategy", {
  expect_s3_class(RES, "data.frame")
  expect_identical(
    colnames(RES),
    c("Run", "d1", "d2", "d3", "Probability", "Cost", "Benefit", "Utility",
      "QALY", "Payoff")
  )
  expect_identical(nrow(RES), 12L)
  itss <- which(
    RES[, "d1"] == "test" & RES[, "d2"] == "sell" & RES[, "d3"] == "sell"
  )
  expect_intol(sum(RES[itss, "Probability"]), 1.0, 0.01)
  expect_intol(sum(RES[itss, "Cost"]), 50.0, 5.0)
  expect_intol(sum(RES[itss, "Benefit"]), 888.0, 5.0)
  # find optimal strategies
  imax <- which.max(RES[, "Payoff"])
  popt <- paste(
    RES[[imax, "d1"]], RES[[imax, "d2"]], RES[[imax, "d3"]],
    sep = "_"
  )
  expect_identical(popt, "test_sell_dig")
})
