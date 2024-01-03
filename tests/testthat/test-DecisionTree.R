
# arborescences that are not decision trees are rejected
test_that("arborescences that are not decision trees are rejected", {
  # some nodes not {D,C,L}
  d1 <- Node$new("d1")
  c1 <- ChanceNode$new()
  t1 <- LeafNode$new("t1")
  t2 <- LeafNode$new("t2")
  t3 <- LeafNode$new("t3")
  e1 <- Arrow$new(d1, c1, label = "e1")
  e2 <- Arrow$new(c1, t1, label = "e2")
  e3 <- Arrow$new(c1, t2, label = "e3")
  e4 <- Arrow$new(d1, t3, label = "e4")
  expect_error(
    DecisionTree$new(V = list(d1, c1, t1, t2, t3), E = list(e1, e2, e3, e4)),
    class = "incorrect_node_type"
  )
  # terminal nodes are not Leaf
  d1 <- DecisionNode$new("d1")
  e1 <- Arrow$new(d1, c1, label = "e1")
  e4 <- Arrow$new(d1, t3, label = "e4")
  expect_error(
    DecisionTree$new(V = list(d1, c1, t3), E = list(e1, e4)),
    class = "leaf_non-child_sets_unequal"
  )
  # some leafs are not terminal
  e2 <- Arrow$new(c1, t1, label = "e2")
  e3 <- Arrow$new(c1, t2, label = "e3")
  t4 <- LeafNode$new("t4")
  e5 <- Arrow$new(t3, t4, label = "e5")
  expect_error(
    DecisionTree$new(
      V = list(d1, c1, t1, t2, t3, t4),
      E = list(e1, e2, e3, e4, e5)
    ),
    class = "leaf_non-child_sets_unequal"
  )
  # edge (arrow) types must be Action or Reaction
  e1 <- Action$new(d1, c1, label = "e1")
  e2 <- Reaction$new(c1, t1, p = 0.5, label = "e2")
  e3 <- Reaction$new(c1, t2, p = 0.5, label = "e3")
  e4 <- Arrow$new(d1, t3, label = "e4")
  expect_error(
    DecisionTree$new(V = list(d1, c1, t1, t2, t3), E = list(e1, e2, e3, e4)),
    class = "incorrect_edge_type"
  )
  # labels of actions from a common DecisionNode must be unique
  e4 <- Action$new(d1, t3, label = "e1")
  expect_error(
    DecisionTree$new(V = list(d1, c1, t1, t2, t3), E = list(e1, e2, e3, e4)),
    class = "non_unique_labels"
  )
  # decision node labels must be unique
  d2 <- DecisionNode$new("d1")
  e4 <- Action$new(d1, d2, label = "e4")
  e5 <- Action$new(d2, t3, label = "e5")
  expect_error(
    DecisionTree$new(
      V = list(d1, d2, c1, t1, t2, t3), E = list(e1, e2, e3, e4, e5)
    ),
    class = "non_unique_labels"
  )
  # construct a legal tree
  e4 <- Action$new(d1, t3, label = "e4")
  DT <- DecisionTree$new(V = list(d1, c1, t1, t2, t3), E = list(e1, e2, e3, e4))
  # decision nodes
  expect_error(DT$decision_nodes(42L), class = "unknown_what_value")
  expect_r6_setequal(DT$decision_nodes(), list(d1))
  expect_setequal(DT$decision_nodes(what = "label"), list("d1"))
  ev <- DT$vertex_index(d1)
  expect_setequal(DT$decision_nodes(what = "index"), ev)
  # chance nodes
  expect_error(DT$chance_nodes(42L), class = "unknown_what_value")
  expect_r6_setequal(DT$chance_nodes(), list(c1))
  expect_setequal(DT$chance_nodes(what = "label"), list(""))
  ev <- DT$vertex_index(c1)
  expect_setequal(DT$chance_nodes(what = "index"), ev)
  # leaf nodes
  expect_error(DT$leaf_nodes(42L), class = "unknown_what_value")
  expect_r6_setequal(DT$leaf_nodes(), list(t1, t2, t3))
  expect_setequal(DT$leaf_nodes("label"), list("t1", "t2", "t3"))
  ev <- DT$vertex_index(list(t1, t2, t3))
  expect_setequal(DT$leaf_nodes("index"), ev)
  # actions
  expect_error(DT$actions(d2), class = "not_in_tree")
  expect_error(DT$actions(c1), class = "not_decision_node")
  expect_error(DT$actions(),  class = "decision_node_not_defined")
  expect_r6_setequal(DT$actions(d1), list(e1, e4))
  # evaluate
  expect_error(DT$evaluate(N = "forty two"), class = "N_not_numeric")
  expect_error(DT$evaluate(by = 42L), class = "by_not_character")
  expect_error(DT$evaluate(by = "forty two"), class = "by_invalid")
  # tornado with no model variables
  grDevices::pdf(file = NULL)
  expect_null(DT$tornado(index = list(e1), ref = list(e4)))
  grDevices::dev.off()
})

# tests of basic tree properties (Kaminski et al CEJOR 2018;26:135-139, fig 1)
test_that("simple decision trees are modelled correctly", {
  # nodes and edges
  d1 <- DecisionNode$new("d 1") # replaced by "d.1".
  c1 <- ChanceNode$new("c1")
  t1 <- LeafNode$new("t1")
  t2 <- LeafNode$new("t2")
  t3 <- LeafNode$new("t3")
  e1 <- Action$new(d1, c1, benefit = 10.0, label = "e1")
  e2 <- Reaction$new(
    c1, t1, p = 0.75, cost = 42.0, benefit = 20.0, label = "e2"
  )
  e3 <- Reaction$new(c1, t2, p = 0.25, label = "e3")
  e4 <- Action$new(d1, t3, benefit = 20.0, label = "e4")
  # tree
  V <- list(d1, c1, t1, t2, t3)
  E <- list(e1, e2, e3, e4)
  DT <- DecisionTree$new(V, E)
  # properties
  A <- DT$actions(d1)
  expect_setequal(
    vapply(A, FUN.VALUE = "x", function(a) {
      a$label()
    }),
    c("e1", "e4")
  )
  expect_setequal(DT$decision_nodes("label"), "d.1")
  # edge property matrix
  ep <- DT$edge_properties()
  expect_identical(nrow(ep), 4L)
  expect_identical(ncol(ep), 4L)
  expect_true(is.matrix(ep))
  expect_setequal(colnames(ep), c("index", "probability", "cost", "benefit"))
  expect_setequal(rownames(ep), c("e1", "e2", "e3", "e4"))
  expect_identical(ep["e1", "probability"], 1.0)
  expect_intol(ep["e2", "cost"], 42.0, 0.1)
  expect_intol(ep["e3", "probability"], 0.25, 0.01)
  expect_intol(ep[["e4", "benefit"]], 20.0, 0.1)
  # draw it
  grDevices::pdf(NULL)
  expect_silent(DT$draw(border = TRUE))
  grDevices::dev.off()
  # strategy validity
  expect_false(DT$is_strategy(list(e2, e3)))
  expect_false(DT$is_strategy(list()))
  expect_false(DT$is_strategy(list(e1, e4)))
  # strategy paths
  P <- DT$root_to_leaf_paths()
  expect_length(P, 3L)
  expect_false(DT$is_strategy(list(e2)))
  expect_false(DT$is_strategy(list(e1, e4)))
  PS <- DT$strategy_paths()
  expect_identical(nrow(PS), 3L)
  # strategy table
  expect_error(DT$strategy_table(42L), class = "unknown_what_value")
  expect_error(
    DT$strategy_table(select = list(e2, e3)), class = "invalid_strategy"
  )
  S <- DT$strategy_table()
  expect_identical(nrow(S), 2L)
  expect_setequal(rownames(S), c("e1", "e4"))
  S <- DT$strategy_table("label", select = list(e1))
  expect_identical(S[[1L, "d.1"]], "e1")
  expect_setequal(rownames(S), "e1")
  # evaluate valid walks
  paths <- DT$root_to_leaf_paths()
  walks <- lapply(X = paths, FUN = DT$walk)
  expect_length(walks, 3L)
  result <- DT$evaluate_walks(walks)
  expect_identical(nrow(result), 3L)
  ileaf <- DT$vertex_index(list(t1, t2, t3))
  expect_setequal(rownames(result), as.character(ileaf))
  # invalid walks should fail gracefully
  walks <- list()
  result <- DT$evaluate_walks(walks)
  expect_identical(nrow(result), 0L)
  expect_error(DT$evaluate_walks(paths))
  # check row order and results of evaluation
  RES <- DT$evaluate(by = "path")
  expect_identical(RES[, "d.1"], c("e1", "e1", "e4"))
  expect_identical(RES[, "Leaf"], c("t1", "t2", "t3"))
  expect_intol(RES[[which(RES[, "Leaf"] == "t1"), "Benefit"]], 22.5, 0.05)
})

# test with utility > 1
test_that("decision trees with utility > 1 are supported", {
  # fictitious scenario
  u_normdel <- ConstModVar$new("Utility for pregancy", "", const = 2.0)
  u_miscarriage <- 0.8
  c_normdel <- 100.0
  c_miscarriage <- 1000.0
  p_miscarriage_notest <- 0.010
  p_miscarriage_test <- 0.005
  c_test <- 50.0
  # construct model
  ta1 <- LeafNode$new(label = "normdelivery", utility = u_normdel)
  ta2 <- LeafNode$new(label = "miscarriage", utility = u_miscarriage)
  ca <- ChanceNode$new()
  ra1 <- Reaction$new(
    source = ca,
    target = ta1,
    p = 1.0 - p_miscarriage_notest,
    cost = c_normdel
  )
  ra2 <- Reaction$new(
    source = ca,
    target = ta2,
    p = p_miscarriage_notest,
    cost = c_miscarriage
  )
  tb1 <- LeafNode$new(label = "normdelivery", utility = u_normdel)
  tb2 <- LeafNode$new(label = "miscarriage", utility = u_miscarriage)
  cb <- ChanceNode$new()
  rb1 <- Reaction$new(
    source = cb,
    target = tb1,
    p = 1.0 - p_miscarriage_test,
    cost = c_normdel
  )
  rb2 <- Reaction$new(
    source = cb,
    target = tb2,
    p = p_miscarriage_test,
    cost = c_miscarriage
  )
  d1 <- DecisionNode$new("PE")
  a1 <- Action$new(source = d1, target = ca, cost = 0.0, label = "notest")
  a2 <- Action$new(source = d1, target = cb, cost = c_test, label = "PEtest")
  dt <- DecisionTree$new(
    V = list(d1, ca, cb, ta1, ta2, tb1, tb2),
    E = list(a1, a2, ra1, ra2, rb1, rb2)
  )
  # evaluate it
  ev <- dt$evaluate()
  cn <- ev[[which(ev[, "PE"] == "notest"), "Cost"]]
  ct <- ev[[which(ev[, "PE"] == "PEtest"), "Cost"]]
  un <- ev[[which(ev[, "PE"] == "notest"), "QALY"]]
  ut <- ev[[which(ev[, "PE"] == "PEtest"), "QALY"]]
  oicer <- (ct - cn) / (ut - un)
  cn <- c_normdel * (1.0 - p_miscarriage_notest) +
    c_miscarriage * p_miscarriage_notest
  ct <- c_normdel * (1.0 - p_miscarriage_test) +
    c_miscarriage * p_miscarriage_test +
    c_test
  un <- u_normdel$get() * (1.0 - p_miscarriage_notest) +
    u_miscarriage * p_miscarriage_notest
  ut <- u_normdel$get() * (1.0 - p_miscarriage_test) +
    u_miscarriage * p_miscarriage_test
  eicer <- (ct - cn) / (ut - un)
  expect_intol(oicer, eicer, 100.0)
})

# ----------------------------------------------------------------------------
# A decision tree with paths common to >1 strategy
# ----------------------------------------------------------------------------
test_that("paths common to >1 strategy are analyzed", {
  # variables
  p.disease <- BetaModVar$new("P(Test+ve)", "P", alpha = 10.0, beta = 40.0)
  q.disease <- ExprModVar$new("1-P(Test+ve)", "P", rlang::quo(1.0 - p.disease))
  # create tree
  c1 <- ChanceNode$new("c1")
  d1 <- DecisionNode$new("d1")
  t1 <- LeafNode$new("expensive drug")
  d2 <- DecisionNode$new("d2")
  t2 <- LeafNode$new("conventional management")
  t3 <- LeafNode$new("watchful waiting")
  t4 <- LeafNode$new("discharge")
  e1 <- Reaction$new(c1, d1, p = p.disease, cost = 0.0, label = "test +ve")
  e2 <- Reaction$new(c1, t4, p = q.disease, cost = 0.0, label = "test -ve")
  e3 <- Action$new(d1, t1, label = "treat", cost = 1000.0)
  e4 <- Action$new(d1, d2, label = "manage", cost = 0.0)
  e5 <- Action$new(d2, t2, label = "conservatively", cost = 200.0)
  e6 <- Action$new(d2, t3, label = "watch", cost = 50.0)
  DT <- DecisionTree$new(
    V = list(c1, d1, d2, t1, t2, t3, t4),
    E = list(e1, e2, e3, e4, e5, e6)
  )
  # there are 8 paths walked by the strategies (two end on leaf t1, one each on
  # t2 and t3 and four on t4) out of 16 possible (4 paths and 4 strategies);
  # each strategy walks 2 paths
  st <- DT$strategy_table()
  expect_s3_class(st, "data.frame")
  expect_type(st[, "d1"], "integer")
  expect_type(st[, "d2"], "integer")
  expect_identical(nrow(st), 4L)
  expect_setequal(colnames(st), c("d1", "d2"))
  expect_setequal(
    rownames(st),
    c("treat_conservatively", "treat_watch",
      "manage_conservatively", "manage_watch"
    )
  )
  st <- DT$strategy_table(what = "label")
  expect_s3_class(st, "data.frame")
  expect_type(st[, "d1"], "character")
  expect_type(st[, "d2"], "character")
  expect_identical(nrow(st), 4L)
  expect_setequal(colnames(st), c("d1", "d2"))
  expect_setequal(
    rownames(st),
    c("treat_conservatively", "treat_watch",
      "manage_conservatively", "manage_watch"
    )
  )
  # check the strategy paths (2 paths per strategy)
  sp <- DT$strategy_paths()
  expect_s3_class(sp, "data.frame")
  expect_identical(nrow(sp), 8L)
  it1 <- DT$vertex_index(t1)
  it2 <- DT$vertex_index(t2)
  it3 <- DT$vertex_index(t3)
  it4 <- DT$vertex_index(t4)
  expect_identical(sum(sp[, "Leaf"] == it1), 2L)
  expect_identical(sum(sp[, "Leaf"] == it2), 1L)
  expect_identical(sum(sp[, "Leaf"] == it3), 1L)
  expect_identical(sum(sp[, "Leaf"] == it4), 4L)
  ag <- aggregate(x = Leaf ~ d1 + d2, data = sp, FUN = length)
  expect_identical(nrow(ag), 4L)
  expect_true(all(ag[, "Leaf"] == 2L))
  # evaluate by path checking row order follows modified leaf names
  # lexicographically.
  RES <- DT$evaluate(by = "path")
  expect_identical(
    colnames(RES),
    c("Run", "d1", "d2", "Leaf", "Probability",
      "Cost", "Benefit", "Utility", "QALY")
  )
  expect_identical(RES[, "d1"], c(rep("manage", 4L), rep("treat", 4L)))
  expect_identical(
    RES[, "Leaf"],
    c("conventional.management", "discharge", "discharge", "watchful.waiting",
      "discharge", "expensive.drug", "discharge", "expensive.drug"
    )
  )
  # evaluate by strategy (4 strategies x 2 runs).
  RES <- DT$evaluate(N = 2L)
  expect_identical(nrow(RES), 8L)
  expect_identical(RES[, "Run"], c(rep(1L, 4L), rep(2L, 4L)))
  expect_identical(
    RES[, "d1"],
    rep(c("manage", "treat"), each = 2L, times = 2L)
  )
  # evaluate by run and check format of result
  RES <- DT$evaluate(setvars = "random", by = "run", N = 2L)
  expect_s3_class(RES, "data.frame")
  expect_identical(nrow(RES), 2L)
  eg <- expand.grid(
    c("Probability", "Cost", "Benefit", "Utility", "QALY"),
    rownames(st)
  )
  cnames <- sprintf("%s.%s", eg[, 1L], eg[, 2L])
  expect_setequal(colnames(RES), c("Run", cnames))
})
