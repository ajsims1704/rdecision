
# arborescences that are not decision trees are rejected
test_that("arborescences that are not decision trees are rejected", {
  # some nodes not {D,C,L}
  d1 <- Node$new("d1")
  c1 <- ChanceNode$new()
  t1 <- LeafNode$new("t1")
  t2 <- LeafNode$new("t2")
  t3 <- LeafNode$new("t3")
  e1 <- Arrow$new(d1,c1,label="e1")  
  e2 <- Arrow$new(c1,t1,label="e2")
  e3 <- Arrow$new(c1,t2,label="e3")
  e4 <- Arrow$new(d1,t3,label="e4")  
  expect_error(
    DecisionTree$new(V=list(d1,c1,t1,t2,t3), E=list(e1,e2,e3,e4)),
    class = "incorrect_node_type"
  )
  # terminal nodes are not Leaf
  d1 <- DecisionNode$new("d1")
  e1 <- Arrow$new(d1,c1,label="e1")  
  e4 <- Arrow$new(d1,t3,label="e4")  
  expect_error(
    DecisionTree$new(V=list(d1,c1,t3), E=list(e1,e4)),
    class = "leaf_non-child_sets_unequal"
  )
  # some leafs are not terminal
  e2 <- Arrow$new(c1,t1,label="e2")
  e3 <- Arrow$new(c1,t2,label="e3")
  t4 <- LeafNode$new("t4")
  e5 <- Arrow$new(t3,t4,label="e5")
  expect_error(
    DecisionTree$new(V=list(d1,c1,t1,t2,t3,t4), E=list(e1,e2,e3,e4,e5)),
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
  expect_silent(
    DT <- DecisionTree$new(V=list(d1,c1,t1,t2,t3), E=list(e1,e2,e3,e4))
  )
  # queries
  expect_error(DT$decision_nodes(42),class="unknown_what_value")
  expect_R6setequal(DT$decision_nodes(),list(d1))
  expect_R6setequal(DT$chance_nodes(),list(c1))
  expect_error(DT$leaf_nodes(42),class="unknown_what_value")
  expect_R6setequal(DT$leaf_nodes(),list(t1,t2,t3))
  expect_setequal(DT$leaf_nodes("label"),list("t1","t2","t3"))
  expect_setequal(
    DT$leaf_nodes("index"),
    list(DT$vertex_index(t1),DT$vertex_index(t2),DT$vertex_index(t3))
  )
  expect_error(DT$actions(d2), class="not_in_tree")
  expect_error(DT$actions(c1), class="not_decision_node")
  expect_error(DT$actions(),  class="decision_node_not_defined")
  expect_R6setequal(DT$actions(d1), list(e1,e4))
  # evaluate
  expect_error(DT$evaluate(N="forty two"), class = "N_not_numeric")
  expect_error(DT$evaluate(by=42), class = "by_not_character")
  expect_error(DT$evaluate(by="forty two"), class = "by_invalid")
  # tornado
  expect_null(DT$tornado(index=list(e1),ref=list(e4)))
})

# tests of basic tree properties (Kaminski et al CEJOR 2018;26:135-139, fig 1)
test_that("simple decision trees are modelled correctly", {
  # nodes & edges
  d1 <- DecisionNode$new("d1")
  c1 <- ChanceNode$new("c1")
  t1 <- LeafNode$new("t1")
  t2 <- LeafNode$new("t2")
  t3 <- LeafNode$new("t3")
  e1 <- Action$new(d1,c1,benefit=10,label="e1")
  e2 <- Reaction$new(c1,t1,p=0.75,benefit=20,label="e2")
  e3 <- Reaction$new(c1,t2,p=0.25,label="e3")
  e4 <- Action$new(d1,t3,benefit=20,label="e4")
  # tree
  V <- list(d1,c1,t1,t2,t3)
  E <- list(e1,e2,e3,e4)
  expect_silent(DT <- DecisionTree$new(V,E))
  # properties
  A <- DT$actions(d1)
  expect_setequal(sapply(A,function(a){a$label()}),c("e1","e4"))
  expect_setequal(DT$decision_nodes("label"), c("d1"))
  # draw it
  pdf(NULL)
  expect_silent(DT$draw(border=TRUE))
  dev.off()
  # strategy validity
  expect_error(DT$is_strategy(list(e2,e3)), class = "incorrect_strategy_type")
  expect_false(DT$is_strategy(list()))
  expect_false(DT$is_strategy(list(e1,e4)))
  # strategy paths
  P <- DT$root_to_leaf_paths()
  expect_length(P,3)
  expect_error(DT$is_strategy(list(e2)),class="incorrect_strategy_type")
  expect_false(DT$is_strategy(list(e1,e4)))
  PS <- DT$strategy_paths()
  expect_equal(nrow(PS),3)
  # strategies
  expect_error(DT$strategy_table(42), class="unknown_what_value")
  expect_error(
    DT$strategy_table(select=list(e2,e3)), 
    class = "incorrect_strategy_type"
  )
  S <- DT$strategy_table()
  expect_equal(nrow(S),2)
  expect_setequal(rownames(S), c("e1","e4"))
  S <- DT$strategy_table("label", select=list(e1))
  expect_identical(S$d1[1],"e1")
  expect_setequal(rownames(S), c("e1"))
  # evaluations
  RES <- DT$evaluate(by="path")
  expect_intol(RES[RES$Leaf=="t1","Benefit"], 22.5, 0.05)
})

# Evans et al, Pharmacoeconomics, 1997;12:565-577, Sumatriptan for migraine
# (base case)
test_that("rdecision replicates Evans et al, Sumatriptan base case", {
  # Time horizon
  th <- as.difftime(48, units="hours")
  # model variables
  c.sumatriptan <- ConstModVar$new("Sumatriptan","CAD",16.10)
  c.caffeine <- ConstModVar$new("Caffeine", "CAD", 1.32)
  c.ED <- 63.16
  c.admission <- 1093
  #
  # Sumatriptan branch
  #
  ta <- LeafNode$new("A", utility=1.0, interval=th)
  tb <- LeafNode$new("B", utility=0.9, interval=th)
  c3 <- ChanceNode$new("c3")
  e1 <- Reaction$new(c3, ta, p=0.594, label="No recurrence")
  e2 <- Reaction$new(c3, tb, p=0.406, cost=c.sumatriptan, 
                     label="Recurrence relieved with 2nd dose")
  #
  td <- LeafNode$new("D", utility=0.1, interval=th)
  te <- LeafNode$new("E", utility=-0.3, interval=th)
  c7 <- ChanceNode$new("c7")
  e3 <- Reaction$new(c7, td, p=0.998, label="Relief")
  e4 <- Reaction$new(c7, te, p=0.002, cost=c.admission, label="Hospitalization")
  #
  tc <- LeafNode$new("C", utility=-0.3, interval=th)
  c4 <- ChanceNode$new("c4")
  e5 <- Reaction$new(c4, tc, p=0.920, label="Endures attack")
  e6 <- Reaction$new(c4, c7, p=0.080, cost=c.ED, label="Emergency Department")
  #
  c1 <- ChanceNode$new("c1")
  e7 <- Reaction$new(c1, c3, p=0.558, label="Relief")
  e8 <- Reaction$new(c1, c4, p=0.442, label="No relief")
  #
  # Caffeine/Ergotamine branch
  #
  tf <- LeafNode$new("F", utility=1.0, interval=th)
  tg <- LeafNode$new("G", utility=0.9, interval=th)
  c5 <- ChanceNode$new("c5")
  e9 <- Reaction$new(c5, tf, p=0.703, label="No recurrence")
  e10 <- Reaction$new(c5, tg, p=0.297, cost=c.caffeine, 
                      label="Recurrence relieved with 2nd dose")
  #
  ti <- LeafNode$new("I", utility=0.1, interval=th)
  tj <- LeafNode$new("J", utility=-0.3, interval=th)
  c8 <- ChanceNode$new("c8")
  e11 <- Reaction$new(c8, ti, p=0.998, label="Relief")
  e12 <- Reaction$new(c8, tj, p=0.002, cost=c.admission, 
                      label="Hospitalization")
  # 
  th <- LeafNode$new("H", utility=-0.3, interval=th)
  c6 <- ChanceNode$new("c6")
  e13 <- Reaction$new(c6, th, p=0.920, label="Endures attack")
  e14 <- Reaction$new(c6, c8, p=0.080, cost=c.ED, label="Emergency Department")
  #
  c2 <- ChanceNode$new("c2")
  expect_identical(c2$label(), "c2")
  e15 <- Reaction$new(c2, c5, p=0.379, label="Relief")
  e16 <- Reaction$new(c2, c6, p=0.621, label="No relief")
  #
  # decision node
  d1 <- DecisionNode$new("d1")
  expect_identical(d1$label(), "d1")
  e17 <- Action$new(d1, c1, cost=c.sumatriptan, label="Sumatriptan")
  e18 <- Action$new(d1, c2, cost=c.caffeine, label="Caffeine/Ergotamine")
  # 
  # create lists of nodes and edges
  V <- list(
    d1, c1, c2, c3, c4, c5, c6, c7, c8,
    ta, tb, tc, td, te, tf, tg, th, ti, tj
  )
  E <- list(
    e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, 
    e17, e18
  )
  # tree
  expect_silent(dt <- DecisionTree$new(V,E))
  # evaluate
  RES <- dt$evaluate()
  # check
  expect_true(is.data.frame(RES))
  c.Sumatriptan <- round(RES[RES$Run==1 & RES$d1=="Sumatriptan", "Cost"],2)
  expect_intol(c.Sumatriptan, 22.06, 0.1)
  c.Caffeine <- round(RES[RES$Run==1 & RES$d1=="Caffeine/Ergotamine", "Cost"],2)
  expect_intol(c.Caffeine, 4.71, 0.1)
  u.Sumatriptan <- round(RES[RES$Run==1 & RES$d1=="Sumatriptan", "Utility"],2)
  expect_intol(u.Sumatriptan, 0.42, 0.1)
  u.Caffeine <- round(RES[RES$Run==1 & 
                          RES$d1=="Caffeine/Ergotamine", "Utility"],2)
  expect_intol(u.Caffeine, 0.20, 0.05)
  # tornado (only drug costs are modvars)
  TO <- dt$tornado(index=list(e17),ref=list(e18),outcome="ICER",draw=FALSE)
  expect_intol(TO[TO$Description=="Sumatriptan","outcome.min"],14692,2)
})

# -----------------------------------------------------------------------------
# Kaminski et al CEJOR 2018;26:135-139, fig 7 (gas problem)
# -----------------------------------------------------------------------------
test_that("rdecision replicates Kaminski et al, fig 7", {
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
  p.nogas <- 1-p.gas
  p.ptest <- p.sens*p.gas + (1-p.spec)*p.nogas
  p.ntest <- (1-p.sens)*p.gas + p.spec*p.nogas
  p.gas.ptest <- p.sens*p.gas / p.ptest
  p.gas.ntest <- (1-p.sens)*p.gas / p.ntest
  # edges
  E <- list(
    Action$new(d1,t1,"sell",benefit=800),
    Action$new(d1,c1,"dig",cost=300),
    Reaction$new(c1,t2,p=p.gas,benefit=2500,label="gas"),
    Reaction$new(c1,t3,p=p.nogas,label="no gas"),
    Action$new(d1,c2,"test",cost=50),
    Reaction$new(c2,d2,p=p.ntest,label="negative"),
    Action$new(d2,t4,"sell",benefit=600),
    Action$new(d2,c3,"dig",cost=300),
    Reaction$new(c3,t5,p=p.gas.ntest,benefit=2500,label="gas"),
    Reaction$new(c3,t6,p=(1-p.gas.ntest),label="no gas"),
    Reaction$new(c2,d3,p=p.ptest,label="positive"),
    Action$new(d3,t7,"sell",benefit=1000),
    Action$new(d3,c4,"dig",cost=300),
    Reaction$new(c4,t8,p=p.gas.ptest,benefit=2500,label="gas"),
    Reaction$new(c4,t9,p=(1-p.gas.ptest),label="no gas")
  )
  # tree
  V <- list(d1,d2,d3, c1,c2,c3,c4, t1,t2,t3,t4,t5,t6,t7,t8,t9)
  expect_silent(DT<-DecisionTree$new(V,E))
  # strategies
  expect_error(
    DT$strategy_table(select=list(E[[1]],E[[2]])), 
    class = "invalid_strategy"
  )
  S <- DT$strategy_table("label")
  expect_setequal(
    rownames(S),
    list("sell_sell_sell", "sell_sell_dig", "sell_dig_sell", "sell_dig_dig",
         "dig_sell_sell", "dig_sell_dig", "dig_dig_sell", "dig_dig_dig",
         "test_sell_sell", "test_sell_dig", "test_dig_sell", "test_dig_dig"
         )
  )
  expect_equal(nrow(S),12)
  expect_equal(sum(S$d1=="sell"),4)
  expect_equal(sum(S$d2=="sell"),6)
  expect_equal(sum(S$d3=="sell"),6)
  # single strategy
  s <- list(E[[1]], E[[7]], E[[12]]) # sell/sell/sell
  expect_true(DT$is_strategy(s))
  S <- DT$strategy_table("label", select=s)
  expect_equal(nrow(S),1)
  expect_identical(S$d1[1],"sell")
  expect_identical(S$d2[1],"sell")
  expect_identical(S$d3[1],"sell")
  # test incorrect strategy prescription
  expect_false(DT$is_strategy(list(E[[1]],E[[5]],E[[7]])))
  # evaluate all root-to-leaf paths
  P <- DT$root_to_leaf_paths()
  W <- sapply(P,function(p){DT$walk(p)})
  expect_length(W,9)
  WX <- sapply(P,function(p){
    pp <- p
    if (length(pp)>2) {
      pp[[length(pp)]] <- NULL
    }
    DT$walk(pp)
  })
  expect_error(DT$evaluate_walks(WX), class="not_to_leaf")
  M <- DT$evaluate_walks(W)
  expect_equal(nrow(M),9)
  expect_equal(ncol(M),10)
  expect_intol(M[8,"Cost"], 220.5, 1)
  # evaluate one strategy (test/sell/sell)
  RES <- DT$evaluate()
  expect_true(is.data.frame(RES))
  itss <- which(RES$d1=="test" & RES$d2=="sell" & RES$d3=="sell")
  expect_intol(sum(RES$Probability[itss]),1,0.01)
  expect_intol(sum(RES$Cost[itss]),50,5)
  expect_intol(sum(RES$Benefit[itss]),888,5)
  # find optimal strategies
  RES <- DT$evaluate()
  expect_equal(nrow(RES),12)
  imax <- which.max(RES$Benefit-RES$Cost)
  popt <- paste(RES$d1[imax], RES$d2[imax], RES$d3[imax], sep="_")
  expect_identical(popt, "test_sell_dig")
})

# ----------------------------------------------------------------------------
# A decision tree with paths common to >1 strategy
# ----------------------------------------------------------------------------
test_that("paths common to >1 strategy are analyzed", {
  # variables
  p.disease <- BetaModVar$new("P(Test+ve)","P",alpha=10,beta=40)
  q.disease <- ExprModVar$new("1-P(Test+ve)","P",rlang::quo(1-p.disease))
  # create tree
  c1 <- ChanceNode$new("c1")
  d1 <- DecisionNode$new("d1")
  t1 <- LeafNode$new("expensive drug")
  d2 <- DecisionNode$new("d2")
  t2 <- LeafNode$new("conventional management")
  t3 <- LeafNode$new("watchful waiting")
  t4 <- LeafNode$new("discharge")
  e1 <- Reaction$new(c1,d1,p=p.disease,cost=0,label="test +ve")
  e2 <- Reaction$new(c1,t4,p=q.disease,cost=0,label="test -ve")
  e3 <- Action$new(d1,t1,label="treat",cost=1000)
  e4 <- Action$new(d1,d2,label="manage",cost=0)
  e5 <- Action$new(d2,t2,label="conservatively",cost=200)
  e6 <- Action$new(d2,t3,label="watch",cost=50)
  expect_silent(
    DT<-DecisionTree$new(V=list(c1,d1,d2,t1,t2,t3,t4),E=list(e1,e2,e3,e4,e5,e6))
  )
  # there are 8 paths walked by the strategies (two end on leaf t1, one each on
  # t2 and t3 and four on t4) out of 16 possible (4 paths and 4 strategies); 
  # each strategy walks 2 paths
  expect_silent(SP <- DT$strategy_paths())
  expect_equal(nrow(SP),8)
  # evaluate by path
  RES <- DT$evaluate(by="path")
  # evaluate by strategy (4 strategies x 2 runs)
  RES <- DT$evaluate(N=2)
  expect_equal(nrow(RES),8)
  # evaluate by run
  RES <- DT$evaluate(setvars="random", by="run", N=2)
  expect_equal(nrow(RES),2)
  expect_equal(length(RES$Probability.manage_conservatively),2)
})

# ------------------------------------------------------------
# Jenks et al, App Health Econ & Health Policy 2016;14:135-149
# ------------------------------------------------------------
test_that("redecision replicates Jenks et al, 2016", {
  
  # standard normals
  n1 <- NormModVar$new("SN1","", 0, 1)
  n2 <- NormModVar$new("SN2","", 0, 1)
  n3 <- NormModVar$new("SN3","", 0, 1)
  
  # clinical variables
  r.CRBSI <- NormModVar$new(
    'Baseline CRBSI rate', '/1000 catheter days', mu=1.48, sigma=0.074
  )
  hr.CRBSI <- ExprModVar$new("Tegaderm CRBSI HR", "HR", 
                             rlang::quo(exp(-0.911-0.393*n1)))
  hr.LSI <- ExprModVar$new("Tegaderm LSI HR", "HR", 
                           rlang::quo(exp(-0.911-0.393*n2)))
  r.Dermatitis <- NormModVar$new(
    'Baseline dermatitis risk', '/catheter', mu=0.0026, sigma=0.00026
  )
  rr.Dermatitis <- ExprModVar$new("Tegaderm LSI HR", "HR", 
                                  rlang::quo(exp(1.482-0.490*n3)))

  # cost variables
  c.CRBSI <- GammaModVar$new(
    'CRBSI cost', 'GBP', shape=198.0, scale=50
  )
  c.Dermatitis <- GammaModVar$new(
    'Dermatitis cost', 'GBP', shape=30, scale=5
  )
  c.LSI <- GammaModVar$new(
    'LSI cost', 'GBP', shape=50, scale=5
  )
  n.catheters <- NormModVar$new(
    'No. catheters', 'catheters', mu=3, sigma=0.3 
  )
  c.Tegaderm <- ExprModVar$new(
    "Tegaderm CHG cost", "GBP", rlang::quo(6.21*n.catheters)
  )
  c.Standard <- ExprModVar$new(
    "Standard dressing cost", "GBP", rlang::quo(1.34*n.catheters)
  )
  n.cathdays <- NormModVar$new(
    'No. days with catheter', 'days', mu=10, sigma=2
  )  

  # probabilities
  p.Dermatitis.S <- ExprModVar$new(
    'P(dermatitis|standard dressing)', 'P', 
    rlang::quo(r.Dermatitis*n.catheters)
  )
  q.Dermatitis.S <- ExprModVar$new(
    'Q(dermatitis|standard dressing)', '1-P', 
    rlang::quo(1-p.Dermatitis.S)
  )
  p.Dermatitis.T <- ExprModVar$new(
    'P(dermatitis|Tegaderm)', 'P', 
    rlang::quo(r.Dermatitis*rr.Dermatitis*n.catheters)
  )
  q.Dermatitis.T <- ExprModVar$new(
    'Q(dermatitis|Tegaderm)', '1-P', 
    rlang::quo(1-p.Dermatitis.T)
  )

  p.LSI.S <- NormModVar$new(
    'P(LSI|Standard)', '/patient', mu=0.1, sigma=0.01 
  )
  q.LSI.S <- ExprModVar$new(
    'Q(LSI|Standard)', '1-P', rlang::quo(1-p.LSI.S) 
  )
  p.LSI.T <- ExprModVar$new(
    'P(LSI|Tegaderm)', 'P', rlang::quo(1-(1-p.LSI.S)^hr.LSI)
  )
  q.LSI.T <- ExprModVar$new(
    'Q(LSI|Tegaderm)', '1-P', rlang::quo(1-p.LSI.T)
  )
  
  p.CRBSI.S <- ExprModVar$new(
    'P(CRBSI|standard dressing)', 'P',  rlang::quo(r.CRBSI*n.cathdays/1000)
  )
  q.CRBSI.S <- ExprModVar$new(
    'Q(CRBSI|standard dressing)', '1-P',  rlang::quo(1-p.CRBSI.S)
  )
  p.CRBSI.T <- ExprModVar$new(
    'P(CRBSI|Tegaderm)', 'P', rlang::quo(1-(1-r.CRBSI*n.cathdays/1000)^hr.CRBSI)
  )
  q.CRBSI.T <- ExprModVar$new(
    'Q(CRBSI|Tegaderm)', '1-P', rlang::quo(1-p.CRBSI.T)
  )

  # create decision tree
  th <- as.difftime(7, units="days")
  # standard dressing
  t01 <- LeafNode$new("t01", interval=th)
  t02 <- LeafNode$new("t02", interval=th)
  c01 <- ChanceNode$new()
  e01 <- Reaction$new(c01,t01,p=p.Dermatitis.S,cost=c.Dermatitis)
  e02 <- Reaction$new(c01,t02,p=q.Dermatitis.S,cost=0)
  #
  t03 <- LeafNode$new("t03", interval=th)
  t04 <- LeafNode$new("t04", interval=th)
  c02 <- ChanceNode$new()
  e03 <- Reaction$new(c02,t03,p=p.Dermatitis.S,cost=c.Dermatitis)
  e04 <- Reaction$new(c02,t04,p=q.Dermatitis.S,cost=0)
  #
  c03 <- ChanceNode$new()
  e05 <- Reaction$new(c03,c01,p=p.LSI.S,cost=c.LSI)
  e06 <- Reaction$new(c03,c02,p=q.LSI.S,cost=0)
  #
  t11 <- LeafNode$new("t11", interval=th)
  t12 <- LeafNode$new("t12", interval=th)
  c11 <- ChanceNode$new()
  e11 <- Reaction$new(c11,t11,p=p.Dermatitis.S,cost=c.Dermatitis)
  e12 <- Reaction$new(c11,t12,p=q.Dermatitis.S,cost=0)
  #
  t13 <- LeafNode$new("t13", interval=th)
  t14 <- LeafNode$new("t14", interval=th)
  c12 <- ChanceNode$new()
  e13 <- Reaction$new(c12,t13,p=p.Dermatitis.S,cost=c.Dermatitis)
  e14 <- Reaction$new(c12,t14,p=q.Dermatitis.S,cost=0)
  #
  c13 <- ChanceNode$new()
  e15 <- Reaction$new(c13,c11,p=p.LSI.S,cost=c.LSI)
  e16 <- Reaction$new(c13,c12,p=q.LSI.S,cost=0)
  #
  c23 <- ChanceNode$new()
  e21 <- Reaction$new(c23,c03,p=p.CRBSI.S,cost=c.CRBSI)
  e22 <- Reaction$new(c23,c13,p=q.CRBSI.S,cost=0)
  #
  # Tegaderm branch  
  t31 <- LeafNode$new("t31", interval=th)
  t32 <- LeafNode$new("t32", interval=th)
  c31 <- ChanceNode$new()
  e31 <- Reaction$new(c31,t31,p=p.Dermatitis.T,cost=c.Dermatitis)
  e32 <- Reaction$new(c31,t32,p=q.Dermatitis.T,cost=0)
  #
  t33 <- LeafNode$new("t33", interval=th)
  t34 <- LeafNode$new("t34", interval=th)
  c32 <- ChanceNode$new()
  e33 <- Reaction$new(c32,t33,p=p.Dermatitis.T,cost=c.Dermatitis)
  e34 <- Reaction$new(c32,t34,p=q.Dermatitis.T,cost=0)
  #
  c33 <- ChanceNode$new()
  e35 <- Reaction$new(c33,c31,p=p.LSI.T,cost=c.LSI)
  e36 <- Reaction$new(c33,c32,p=q.LSI.T,cost=0)
  #
  t41 <- LeafNode$new("t41", interval=th)
  t42 <- LeafNode$new("t42", interval=th)
  c41 <- ChanceNode$new()
  e41 <- Reaction$new(c41,t41,p=p.Dermatitis.T,cost=c.Dermatitis)
  e42 <- Reaction$new(c41,t42,p=q.Dermatitis.T,cost=0)
  #
  t43 <- LeafNode$new("t43", interval=th)
  t44 <- LeafNode$new("t44", interval=th)
  c42 <- ChanceNode$new()
  e43 <- Reaction$new(c42,t43,p=p.Dermatitis.T,cost=c.Dermatitis)
  e44 <- Reaction$new(c42,t44,p=q.Dermatitis.T,cost=0)
  #
  c43 <- ChanceNode$new()
  e45 <- Reaction$new(c43,c41,p=p.LSI.T,cost=c.LSI)
  e46 <- Reaction$new(c43,c42,p=q.LSI.T,cost=0)
  #
  c53 <- ChanceNode$new()
  e51 <- Reaction$new(c53,c43,p=p.CRBSI.T,cost=c.CRBSI)
  e52 <- Reaction$new(c53,c33,p=q.CRBSI.T,cost=0)
  #  
  # decision node
  d1 <- DecisionNode$new("d1")
  e9 <- Action$new(d1,c23,label="Standard",cost=c.Standard)
  e10 <- Action$new(d1,c53,label="Tegaderm",cost=c.Tegaderm)
  #
  # create decision tree
  V <- list(d1,c01,c02,c03,c11,c12,c13,c23,c31,c32,c33,c41,c42,c43,c53,
            t01,t02,t03,t04,t11,t12,t13,t14,t31,t32,t33,t34,t41,t42,t43,t44)
  E <- list(e01,e02,e03,e04,e05,e06,e11,e12,e13,e14,e15,e16,e21,e22,
            e31,e32,e33,e34,e35,e36,e41,e42,e43,e44,e45,e46,e51,e52,e9,e10)
  expect_silent(DT <- DecisionTree$new(V,E))

  # direct calculation, from values in Table 3
  p.crbsi <- 10*1.48/1000
  p.lsi <- 0.1
  p.derm <- 0.0026
  c.crbsi <- 9900*p.crbsi
  c.lsi <- 250*p.lsi
  c.derm <- 150*3*p.derm
  c.std <- 1.34*3+c.crbsi+c.lsi+c.derm
  p.crbsi <- 1-((1-p.crbsi)^0.402)
  p.lsi <- 1-((1-p.lsi)^0.402)
  p.derm <- 4.4*p.derm
  c.crbsi <- 9900*p.crbsi
  c.lsi <- 250*p.lsi
  c.derm <- 150*3*p.derm
  c.teg <- 6.21*3+c.crbsi+c.lsi+c.derm

  # check the model variables
  mv <- DT$modvars()
  expect_length(mv, 27)
  MVT <- DT$modvar_table()
  expect_equal(nrow(MVT), 27)
  expect_equal(sum(MVT$Est),16)
  MVT <- DT$modvar_table(FALSE)
  expect_equal(nrow(MVT),11)

  # evaluate the tree (base case, no PSA)
  E <- DT$evaluate()
  expect_intol(E$Cost[E$d1=="Standard"], c.std, 2.00)
  expect_intol(E$Cost[E$d1=="Tegaderm"], c.teg, 2.00)

  # check that illegal arguments to evaluate are rejected
  expect_error(DT$evaluate(setvars=42), class="setvars_not_character")
  expect_error(DT$evaluate(setvars="mode"), class="setvars_invalid")
  expect_silent(DT$evaluate(setvars="q2.5"))
  expect_silent(DT$evaluate(setvars="q50"))
  expect_silent(DT$evaluate(setvars="q97.5"))
  expect_silent(DT$evaluate(setvars="current"))
  
  # tornado (diagram)
  pdf(NULL)
  # tornado
  expect_error(DT$tornado(), class="missing_strategy")
  expect_error(
    DT$tornado(index=list(e10),ref=list(e9),outcome="survival"),
    class = "invalid_outcome"
  )
  expect_error(
    DT$tornado(index=list(e10),ref=list(e9),draw=42),
    class = "invalid_draw"
  )
  expect_error(
    TO <- DT$tornado(
      index=list(e10), ref=list(e9), exclude=42,
      draw = TRUE
    ),
    class = "exclude_not_list"
  )
  expect_error(
    TO <- DT$tornado(
      index=list(e10), ref=list(e9), exclude=list("SN1", "SN2", "SNX"),
      draw = TRUE
    ),
    class = "exclude_element_not_modvar"
  )
  expect_silent(
    TO <- DT$tornado(
      index=list(e10), ref=list(e9), exclude=list("SN1", "SN2", "SN3"),
      draw = TRUE
    )
  )
  dev.off()
  expect_equal(nrow(TO),8)
  
  # threshold
  tHR <- DT$threshold(
    index = list(e10), 
    ref = list(e9),
    outcome = "cost",
    mvd = "SN1",
    a = -2.31,
    b = 0,
    tol = 0.01
  )
  print(tHR)
  
  # PSA (skip on CRAN)
  skip_on_cran()
  PSA <- DT$evaluate(setvars="random",by="run",N=250)
  PSA$Difference <- PSA$Cost.Standard - PSA$Cost.Tegaderm
  expect_intol(mean(PSA$Difference), 77.76, 10.00)
  expect_intol(mean(PSA$Cost.Standard), 176.89, 10.00)
  expect_intol(mean(PSA$Cost.Tegaderm), 99.63, 10.00)

})
