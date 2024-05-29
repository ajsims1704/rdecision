
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
    class = "invalid_labels"
  )
  # decision node labels must be unique
  d2 <- DecisionNode$new("d1")
  e4 <- Action$new(d1, d2, label = "e4")
  e5 <- Action$new(d2, t3, label = "e5")
  expect_error(
    DecisionTree$new(
      V = list(d1, d2, c1, t1, t2, t3), E = list(e1, e2, e3, e4, e5)
    ),
    class = "invalid_labels"
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

# test that using reserved words for decision nodes is detected
test_that("invalid decision node labels are detected", {
  d1 <- DecisionNode$new(label = "Run")
  l1 <- LeafNode$new(label = "l1")
  l2 <- LeafNode$new(label = "l2")
  a1 <- Action$new(source = d1, target = l1, label = "run")
  a2 <- Action$new(source = d1, target = l2, label = "walk")
  expect_error(
    DecisionTree$new(V = list(d1, l1, l2), E = list(a1, a2)),
    class = "invalid_labels"
  )
  d1 <- DecisionNode$new(label = "Benefit")
  a1 <- Action$new(source = d1, target = l1, label = "run")
  a2 <- Action$new(source = d1, target = l2, label = "walk")
  expect_error(
    DecisionTree$new(V = list(d1, l1, l2), E = list(a1, a2)),
    class = "invalid_labels"
  )
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

# test that the sum of probabilities from each chance node must be 1, including
# when probability is modelled as a random variable
test_that("chance node probabilities sum to unity", {
  # create a tree
  d1 <- DecisionNode$new("D1")
  c1 <- ChanceNode$new()
  c2 <- ChanceNode$new()
  l11 <- LeafNode$new("L11")
  l12 <- LeafNode$new("L12")
  l21 <- LeafNode$new("L21")
  l22 <- LeafNode$new("L22")
  l23 <- LeafNode$new("L23")
  a1 <- Action$new(source = d1, target = c1, label = "Choice 1")
  a2 <- Action$new(source = d1, target = c2, label = "Choice 2")
  r11 <- Reaction$new(source = c1, target = l11)
  r12 <- Reaction$new(source = c1, target = l12)
  r21 <- Reaction$new(source = c2, target = l21)
  r22 <- Reaction$new(source = c2, target = l22)
  r23 <- Reaction$new(source = c2, target = l23)
  dt <- DecisionTree$new(
    V = list(d1, c1, c2, l11, l12, l21, l22, l23),
    E = list(a1, a2, r11, r12, r21, r22, r23)
  )
  # evaluate with default (zero) probabilities
  expect_error(dt$evaluate(), class = "invalid_probability_sum")
  # set the probabilities and check that the model evaluates when correct
  r11$set_probability(0.5)
  r12$set_probability(0.5)
  expect_error(dt$evaluate(), class = "invalid_probability_sum")
  r21$set_probability(1L / 3L)
  r22$set_probability(1L / 3L)
  r23$set_probability(2L / 3L)
  expect_error(dt$evaluate(), class = "invalid_probability_sum")
  r23$set_probability(1L / 3L)
  expect_silent(dt$evaluate())
  # create a Dirichlet distribution and associated ModVars for c1
  dtwo <- DirichletDistribution$new(c(1L, 9L))
  pc11 <- ModVar$new("p(success)", "P", D = dtwo, k = 1L)
  pc12 <- ModVar$new("p(failure)", "P", D = dtwo, k = 2L)
  r11$set_probability(pc11)
  r12$set_probability(pc12)
  expect_silent(dt$evaluate(setvars = "random", N = 20L))
  # and for c2
  dthree <- DirichletDistribution$new(c(1L, 9L, 6L))
  pc21 <- ModVar$new("p(outcome 1)", "P", D = dthree, k = 1L)
  pc22 <- ModVar$new("p(outcome 2)", "P", D = dthree, k = 2L)
  pc23 <- ModVar$new("p(outcome 3)", "P", D = dthree, k = 3L)
  r21$set_probability(pc21)
  r22$set_probability(pc22)
  r23$set_probability(pc23)
  expect_silent(dt$evaluate(setvars = "random", N = 20L))
})

# test that probabilities of NA are detected and checked
test_that("probabilities of NA are detected and checked", {
  # create a tree
  d1 <- DecisionNode$new("d1")
  c1 <- ChanceNode$new()
  c2 <- ChanceNode$new()
  l1 <- LeafNode$new(label = "l1")
  l2 <- LeafNode$new(label = "l2")
  l3 <- LeafNode$new(label = "l3")
  l4 <- LeafNode$new(label = "l4")
  a1 <- Action$new(s = d1, t = c1, label = "intervention")
  a2 <- Action$new(s = d1, t = c2, label = "comparator")
  r1 <- Reaction$new(s = c1, t = l1)
  r2 <- Reaction$new(s = c1, t = l2)
  r3 <- Reaction$new(s = c2, t = l3)
  r4 <- Reaction$new(s = c2, t = l4)
  dt <- DecisionTree$new(
    V = list(d1, c1, c2, l1, l2, l3, l4),
    E = list(a1, a2, r1, r2, r3, r4)
  )
  # check that evaluation fails due to probabilities not summing to 1
  expect_error(dt$evaluate(), class = "invalid_probability_sum")
  # check evaluation with NA probabilities from a chance node
  r1$set_probability(p = NA_real_)
  r2$set_probability(p = NA_real_)
  r3$set_probability(p = 0.4)
  r4$set_probability(p = NA_real_)
  expect_error(dt$evaluate(), class = "invalid_probability_sum")
  r1$set_probability(p = 0.5)
  dte <- dt$evaluate(by = "path")
  expect_intol(with(dte, Probability[Leaf == "l2"]), 0.5, 0.01)
  expect_intol(with(dte, Probability[Leaf == "l4"]), 0.6, 0.01)
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
    p = NA_real_,
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
    p = NA_real_,
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

# A decision tree with paths common to >1 strategy
test_that("paths common to >1 strategy are analyzed", {
  # variables
  p.disease <- BetaModVar$new("P(Test+ve)", "P", alpha = 10.0, beta = 40.0)
  # create tree
  c1 <- ChanceNode$new("c1")
  d1 <- DecisionNode$new("d1")
  t1 <- LeafNode$new("expensive drug")
  d2 <- DecisionNode$new("d2")
  t2 <- LeafNode$new("conventional management")
  t3 <- LeafNode$new("watchful waiting")
  t4 <- LeafNode$new("discharge")
  e1 <- Reaction$new(c1, d1, p = p.disease, cost = 0.0, label = "test +ve")
  e2 <- Reaction$new(c1, t4, p = NA_real_, cost = 0.0, label = "test -ve")
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

# A decision tree with a long leftmost node label (must be tested by sending
# the plot file to a pdf and reviewing it)
test_that("long node labels are not clipped", {
  d1 <- DecisionNode$new(label = "Home captain decision before the toss")
  l1 <- LeafNode$new("Bat")
  l2 <- LeafNode$new("Bowl")
  l3 <- LeafNode$new("Bat")
  l4 <- LeafNode$new("Bowl")
  l5 <- LeafNode$new("Bowl")
  l6 <- LeafNode$new("Bat")
  c1 <- ChanceNode$new()
  c2 <- ChanceNode$new()
  d2 <- DecisionNode$new("Away captain decision after winning toss")
  d3 <- DecisionNode$new("Away captain decision after losing toss")
  a1 <- Action$new(source = d1, target = c1, "Bat first")
  a2 <- Action$new(source = d1, target = c2, "Bowl first")
  r1 <- Reaction$new(source = c1, target = l1, p = 0.5, label = "Win toss")
  r2 <- Reaction$new(source = c1, target = d2, p = 0.5, label = "Lose toss")
  a3 <- Action$new(source = d2, target = l2, "Elect to bat")
  a4 <- Action$new(source = d2, target = l3, "Elect to bowl")
  r3 <- Reaction$new(source = c2, target = l4, p = 0.5, label = "Win toss")
  r4 <- Reaction$new(source = c2, target = d3, p = 0.5, label = "Lose toss")
  a5 <- Action$new(source = d3, target = l5, "Elect to bat")
  a6 <- Action$new(source = d3, target = l6, "Elect to bowl")
  dt <- DecisionTree$new(
    V = list(d1, d2, d3, c1, c2, l1, l2, l3, l4, l5, l6),
    E = list(a1, a2, a3, a4, a5, a6, r1, r2, r3, r4)
  )
  grDevices::pdf(NULL, width = 7.0, height = 5.0)
  expect_no_condition(dt$draw(border = TRUE, fontsize = 10.0))
  grDevices::dev.off()
})
