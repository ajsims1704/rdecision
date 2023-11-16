
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
    DecisionTree$new(V=list(d1, c1, t1, t2, t3), E=list(e1, e2, e3, e4)),
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
  e1 <- Action$new(d1,c1,label="e1")  
  e2 <- Reaction$new(c1,t1,p=0.5,label="e2")
  e3 <- Reaction$new(c1,t2,p=0.5,label="e3")
  e4 <- Arrow$new(d1,t3,label="e4")  
  expect_error(
    DecisionTree$new(V=list(d1,c1,t1,t2,t3), E=list(e1,e2,e3,e4)),
    class = "incorrect_edge_type"
  )
  # labels of actions from a common DecisionNode must be unique
  e4 <- Action$new(d1,t3,label="e1")
  expect_error(
    DecisionTree$new(V=list(d1,c1,t1,t2,t3), E=list(e1,e2,e3,e4)),
    class = "non_unique_labels"
  )
  # decision node labels must be unique
  d2 <- DecisionNode$new("d1")
  e4 <- Action$new(d1,d2,label="e4")
  e5 <- Action$new(d2,t3,label="e5")
  expect_error(
    DecisionTree$new(V=list(d1,d2,c1,t1,t2,t3), E=list(e1,e2,e3,e4,e5)),
    class = "non_unique_labels"
  )
  # construct a legal tree
  e4 <- Action$new(d1,t3,label="e4")
  DT <- DecisionTree$new(V=list(d1,c1,t1,t2,t3), E=list(e1,e2,e3,e4))
  # decision nodes
  expect_error(DT$decision_nodes(42L), class = "unknown_what_value")
  expect_r6_setequal(DT$decision_nodes(), list(d1))
  expect_setequal(DT$decision_nodes(what = "label"), list("d1"))
  ev <- as.vector(x = DT$vertex_index(d1), mode = "integer")
  expect_identical(DT$decision_nodes(what = "index"), ev)
  # chance nodes
  expect_error(DT$chance_nodes(42L), class = "unknown_what_value")
  expect_r6_setequal(DT$chance_nodes(), list(c1))
  expect_setequal(DT$chance_nodes(what = "label"), list(""))
  ev <- as.vector(x = DT$vertex_index(c1), mode = "integer")
  expect_identical(DT$chance_nodes(what = "index"), ev)
  # leaf nodes
  expect_error(DT$leaf_nodes(42L), class = "unknown_what_value")
  expect_r6_setequal(DT$leaf_nodes(), list(t1, t2, t3))
  expect_setequal(DT$leaf_nodes("label"), list("t1", "t2", "t3"))
  ev <- as.vector(
    x = c(DT$vertex_index(t1), DT$vertex_index(t2), DT$vertex_index(t3)),
    mode = "integer"
  )
  expect_setequal(DT$leaf_nodes("index"), ev)
  expect_identical(DT$leaf_nodes(what = "index"), ev)
  # actions
  expect_error(DT$actions(d2), class="not_in_tree")
  expect_error(DT$actions(c1), class="not_decision_node")
  expect_error(DT$actions(),  class="decision_node_not_defined")
  expect_r6_setequal(DT$actions(d1), list(e1,e4))
  # evaluate
  expect_error(DT$evaluate(N="forty two"), class = "N_not_numeric")
  expect_error(DT$evaluate(by = 42L), class = "by_not_character")
  expect_error(DT$evaluate(by="forty two"), class = "by_invalid")
  # tornado with no model variables
  grDevices::pdf(file = NULL)
  expect_null(DT$tornado(index=list(e1), ref=list(e4)))
  grDevices::dev.off()
})

# tests of basic tree properties (Kaminski et al CEJOR 2018;26:135-139, fig 1)
test_that("simple decision trees are modelled correctly", {
  # nodes and edges
  d1 <- DecisionNode$new("d1")
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
  expect_setequal(DT$decision_nodes("label"), "d1")
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
  expect_intol(ep[[4L, "benefit"]], 20.0, 0.1)
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
  expect_identical(S[[1L, "d1"]], "e1")
  expect_setequal(rownames(S), "e1")
  # evaluate valid walks
  paths <- DT$root_to_leaf_paths()
  walks <- lapply(X = paths, FUN = DT$walk)
  expect_length(walks, 3L)
  result <- DT$evaluate_walks(walks)
  expect_identical(nrow(result), 3L)
  # invalid walks should fail gracefully
  walks <- list()
  result <- DT$evaluate_walks(walks)
  expect_identical(nrow(result), 0L)
  expect_error(DT$evaluate_walks(paths))  
  # evaluations
  RES <- DT$evaluate(by = "path")
  expect_intol(RES[RES$Leaf == "t1", "Benefit"], 22.5, 0.05)
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
  cn <- ev$Cost[ev$PE == "notest"]
  ct <- ev$Cost[ev$PE == "PEtest"]
  un <- ev$QALY[ev$PE == "notest"]
  ut <- ev$QALY[ev$PE == "PEtest"]
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
  expect_true(TRUE)
})

# -----------------------------------------------------------------------------
# Kaminski et al CEJOR 2018;26:135-139, fig 7 (gas problem)
# -----------------------------------------------------------------------------
test_that("rdecision replicates Kaminski et al, fig 7", {
  # nodes
  d1 <- DecisionNode$new("d1")
  d2 <- DecisionNode$new("d 2")  # replaced with "d.2"
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
  p.ptest <- p.sens*p.gas + (1.0 - p.spec)*p.nogas
  p.ntest <- (1.0 - p.sens)*p.gas + p.spec*p.nogas
  p.gas.ptest <- p.sens*p.gas / p.ptest
  p.gas.ntest <- (1.0 - p.sens)*p.gas / p.ntest
  # edges
  E <- list(
    Action$new(d1, t1, "sell", benefit = 800.0),
    Action$new(d1, c1, "dig", cost = 300.0),
    Reaction$new(c1,t2,p=p.gas,benefit=2500.0,label="gas"),
    Reaction$new(c1,t3,p=p.nogas,label="no gas"),
    Action$new(d1,c2,"test",cost=50.0),
    Reaction$new(c2,d2,p=p.ntest,label="negative"),
    Action$new(d2,t4,"sell",benefit=600.0),
    Action$new(d2,c3,"dig",cost=300.0),
    Reaction$new(c3, t5, p = p.gas.ntest, benefit = 2500.0, label = "gas"),
    Reaction$new(c3, t6, p = (1.0 - p.gas.ntest), label = "no gas"),
    Reaction$new(c2,d3,p=p.ptest,label="positive"),
    Action$new(d3,t7,"sell",benefit=1000.0),
    Action$new(d3,c4,"dig",cost=300.0),
    Reaction$new(c4,t8,p=p.gas.ptest,benefit=2500.0,label="gas"),
    Reaction$new(c4, t9, p = (1.0 - p.gas.ptest), label = "no gas")
  )
  # tree
  V <- list(d1,d2,d3, c1,c2,c3,c4, t1,t2,t3,t4,t5,t6,t7,t8,t9)
  DT<-DecisionTree$new(V,E)
  # strategies
  expect_error(
    DT$strategy_table(select=list(E[[1L]],E[[2L]])), 
    class = "invalid_strategy"
  )
  S <- DT$strategy_table("label")
  expect_setequal(
    colnames(S),
    list("d1", "d.2", "d3")
  )
  expect_setequal(
    rownames(S),
    list("sell_sell_sell", "sell_sell_dig", "sell_dig_sell", "sell_dig_dig",
      "dig_sell_sell", "dig_sell_dig", "dig_dig_sell", "dig_dig_dig",
      "test_sell_sell", "test_sell_dig", "test_dig_sell", "test_dig_dig"
    )
  )
  expect_identical(nrow(S), 12L)
  expect_identical(sum(S$d1 == "sell"), 4L)
  expect_identical(sum(S$d.2 == "sell"), 6L)
  expect_identical(sum(S$d3 == "sell"), 6L)
  # single strategy
  s <- list(E[[1L]], E[[7L]], E[[12L]]) # sell sell sell
  expect_true(DT$is_strategy(s))
  S <- DT$strategy_table("label", select=s)
  expect_identical(nrow(S), 1L)
  expect_identical(S$d1[[1L]], "sell")
  expect_identical(S$d.2[[1L]], "sell")
  expect_identical(S$d3[[1L]], "sell")
  # test incorrect strategy prescription
  expect_false(DT$is_strategy(list(E[[1L]],E[[5L]],E[[7L]])))
  ddum <- DecisionNode$new("dummy")
  adum <- Action$new(source_node = ddum, target_node = t1, label = "dummy")
  expect_error(
    DT$is_strategy(list(E[[1L]], E[[7L]], adum)),
    class = "not_in_graph"
  )
  # evaluate all root-to-leaf paths
  P <- DT$root_to_leaf_paths()
  W <- lapply(P, DT$walk)
  expect_length(W, 9L)
  WX <- lapply(P,function(p) {
    pp <- p
    if (length(pp) > 2L) {
      pp[[length(pp)]] <- NULL
    }
    DT$walk(pp)
  })
  expect_error(DT$evaluate_walks(WX), class="not_to_leaf")
  M <- DT$evaluate_walks(W)
  expect_identical(nrow(M), 9L)
  expect_identical(ncol(M), 10L)
  expect_intol(M[[8L,"Cost"]], 220.5, 1.0)
  # evaluate one strategy (test/sell/sell)
  RES <- DT$evaluate()
  expect_s3_class(RES, "data.frame")
  expect_setequal(
    names(RES),
    c("d1", "d.2", "d3", "Run", "Probability", "Cost", "Benefit", "Utility",
      "QALY")
  )
  itss <- which(RES$d1=="test" & RES$d.2=="sell" & RES$d3=="sell")
  expect_intol(sum(RES$Probability[itss]), 1.0, 0.01)
  expect_intol(sum(RES$Cost[itss]), 50.0, 5.0)
  expect_intol(sum(RES$Benefit[itss]), 888.0, 5.0)
  # find optimal strategies
  RES <- DT$evaluate()
  expect_identical(nrow(RES), 12L)
  imax <- which.max(RES$Benefit-RES$Cost)
  popt <- paste(RES$d1[imax], RES$d.2[imax], RES$d3[imax], sep="_")
  expect_identical(popt, "test_sell_dig")
})

# ----------------------------------------------------------------------------
# A decision tree with paths common to >1 strategy
# ----------------------------------------------------------------------------
test_that("paths common to >1 strategy are analyzed", {
  # variables
  p.disease <- BetaModVar$new("P(Test+ve)","P", alpha = 10.0, beta = 40.0)
  q.disease <- ExprModVar$new("1-P(Test+ve)","P",rlang::quo(1.0 - p.disease))
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
  DT<-DecisionTree$new(V=list(c1,d1,d2,t1,t2,t3,t4),E=list(e1,e2,e3,e4,e5,e6))
  
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

  # evaluate by path
  RES <- DT$evaluate(by="path")
  # evaluate by strategy (4 strategies x 2 runs)
  RES <- DT$evaluate(N = 2L)
  expect_identical(nrow(RES), 8L)

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

# ----------------------------------------------------------------------------
# README example
# ----------------------------------------------------------------------------
test_that("readme example is correct, with thresholds", {
  # inputs
  c.diet <- 50.0
  c.exercise <- ConstModVar$new("Cost of exercise programme", "GBP", 500.0)
  c.stent <- 5000.0
  diet.s <- 12.0
  diet.f <- 56.0
  exercise.s <- 18.0
  exercise.f <- 40.0
  # define probabilities
  p.diet <- BetaModVar$new("P(diet)", "", alpha=diet.s, beta=diet.f)
  p.exercise <- BetaModVar$new(
    "P(exercise)", "", alpha=exercise.s, beta=exercise.f
  )
  q.diet <- ExprModVar$new("1-P(diet)", "", rlang::quo(1.0-p.diet))
  q.exercise <- ExprModVar$new("1-P(exercise)", "", rlang::quo(1.0-p.exercise))
  # construct tree
  t.ds <- LeafNode$new("no stent")
  t.df <- LeafNode$new("stent")
  t.es <- LeafNode$new("no stent")
  t.ef <- LeafNode$new("stent")
  c.d <- ChanceNode$new("Outcome")
  c.e <- ChanceNode$new("Outcome")
  d <- DecisionNode$new("Programme")
  e.d <- Action$new(d, c.d, cost = c.diet, label = "Diet")
  e.e <- Action$new(d, c.e, cost = c.exercise, label = "Exercise")
  e.ds <- Reaction$new(c.d, t.ds, p = p.diet, cost = 0.0, label = "success")
  e.df <- Reaction$new(c.d, t.df, p = q.diet, cost = c.stent, label = "failure")
  e.es <- Reaction$new(c.e, t.es, p = p.exercise, cost = 0.0, label = "success")
  e.ef <- Reaction$new(
    c.e, t.ef, p = q.exercise, cost = c.stent, label = "failure"
  )
  DT <- DecisionTree$new(
    V = list(d, c.d, c.e, t.ds, t.df, t.es, t.ef),
    E = list(e.d, e.e, e.ds, e.df, e.es, e.ef)
  )
  # evaluate
  RES <- DT$evaluate()
  # direct calculation of costs
  cost.diet <- c.diet + c.stent * (diet.f / (diet.s + diet.f))
  cost.exercise <- 
    c.exercise$mean() + c.stent * (exercise.f / (exercise.s + exercise.f))
  expect_intol(RES$Cost[RES$Programme=="Exercise"], cost.exercise, 5.0)
  expect_intol(RES$Cost[RES$Programme=="Diet"], cost.diet, 5.0)
  # threshold analysis on cost of new programme
  expect_error(
    DT$threshold(),
    class = "missing_strategy"
  )
  expect_error(
    DT$threshold(ref=list(e.e), index=list(e.ds)),
    class = "invalid_strategy"
  )
  expect_error(
    DT$threshold(ref=list(e.e), index=list(e.d), outcome="widgets"),
    class = "invalid_outcome"
  )
  expect_error(
    DT$threshold(ref=list(e.e), index=list(e.d), outcome="saving",
                 mvd="P(dyet)"),
    class = "invalid_mvd"
  )
  expect_error(
    DT$threshold(ref=list(e.e), index=list(e.d), outcome="saving",
                 mvd=42L),
    class = "invalid_mvd"
  )
  expect_error(
    DT$threshold(ref = list(e.e), index = list(e.d), outcome = "saving",
                 mvd=c.exercise$description(), a = 999.0, b = 1000.0),
    class = "invalid_tol"
  )
  expect_error(
    DT$threshold(ref=list(e.e), index=list(e.d), outcome="saving",
                 mvd=c.exercise$description(), a=1000.0, b=999.0, tol=10.0),
    class = "invalid_brackets"
  )
  expect_error(
    DT$threshold(ref=list(e.e), index=list(e.d), outcome="saving",
                 mvd=c.exercise$description(), a=c.exercise$mean(), b=1000.0,
                 tol=-2.0),
    class = "invalid_tol"
  )
  # threshold analysis on cost of the new programme
  expect_error(
    DT$threshold(
      index = list(e.e), ref = list(e.d),  
      mvd = c.exercise$description(),
      a = c.exercise$mean(), b = 1000.0, tol = 1.0
    ), class = "invalid_outcome"
  )
  c.exercise.t <- DT$threshold(
    index = list(e.e), ref = list(e.d), outcome = "saving", 
    mvd = c.exercise$description(),
    a = c.exercise$mean(), b = 1000.0, tol = 1.0
  )
  c.exercise.t.e <- 
    c.diet + c.stent * (diet.f / (diet.s + diet.f)) - 
    c.stent * (exercise.f / (exercise.s + exercise.f))
  expect_intol(c.exercise.t, c.exercise.t.e, 5.0)
  # threshold analysis on success of new programme
  p.ex.t <- DT$threshold(
    index = list(e.e), ref = list(e.d), outcome = "saving",
    mvd = p.exercise$description(),
    a = 0.0, b = p.exercise$mean(), tol = 0.001
  )
  p.ex.t.e <- 
    (c.exercise$mean() - c.diet) / c.stent - 
    (diet.f / (diet.f + diet.s)) + 1.0
  expect_intol(p.ex.t, p.ex.t.e, 0.03)
})
