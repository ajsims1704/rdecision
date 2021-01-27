
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
  expect_true(setequal(sapply(A,function(a){a$label()}),c("e1","e4")))
  expect_equal(DT$decision_nodes("label"), "d1")
  # strategy paths
  P <- DT$root_to_leaf_paths()
  expect_equal(length(P),3)
  PS <- DT$paths_in_strategy(list(e1))
  expect_equal(length(PS),2)
  PS <- DT$paths_in_strategy(list(e4))
  expect_equal(length(PS),1)
  # strategies
  S <- DT$strategies()
  expect_equal(nrow(S),2)
  # evaluations
  RES <- DT$evaluate_strategy(list(e1))
})

# Evans et al, Pharmacoeconomics, 1997;12:565-577, Sumatriptan for migraine
# (base case)
test_that("rdecision replicates Evans et al, Sumatriptan base case", {
  # Time horizon
  th <- as.difftime(48, units="hours")
  # model variables
  c.sumatriptan <- 16.10
  c.caffeine <- 1.32
  c.ED <- 63.16
  c.admission <- 1093
  #
  # Sumatriptan branch
  #
  ta <- LeafNode$new("A", utility=1.0, interval=th)
  tb <- LeafNode$new("B", utility=0.9, interval=th)
  c3 <- ChanceNode$new("c3")
  e1 <- Reaction$new(c3, ta, p=0.594, label="No recurrence")
  e2 <- Reaction$new(c3, tb, p=0.406, cost=c.sumatriptan, label="Recurrence relieved with 2nd dose")
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
  e10 <- Reaction$new(c5, tg, p=0.297, cost=c.caffeine, label="Recurrence relieved with 2nd dose")
  #
  ti <- LeafNode$new("I", utility=0.1, interval=th)
  tj <- LeafNode$new("J", utility=-0.3, interval=th)
  c8 <- ChanceNode$new("c8")
  e11 <- Reaction$new(c8, ti, p=0.998, label="Relief")
  e12 <- Reaction$new(c8, tj, p=0.002, cost=c.admission, label="Hospitalization")
  # 
  th <- LeafNode$new("H", utility=-0.3, interval=th)
  c6 <- ChanceNode$new("c6")
  e13 <- Reaction$new(c6, th, p=0.920, label="Endures attack")
  e14 <- Reaction$new(c6, c8, p=0.080, cost=c.ED, label="Emergency Department")
  #
  c2 <- ChanceNode$new("c2")
  expect_equal(c2$label(), "c2")
  e15 <- Reaction$new(c2, c5, p=0.379, label="Relief")
  e16 <- Reaction$new(c2, c6, p=0.621, label="No relief")
  #
  # decision node
  d1 <- DecisionNode$new("d1")
  expect_equal(d1$label(), "d1")
  e17 <- Action$new(d1, c1, cost=c.sumatriptan, label="Sumatriptan")
  e18 <- Action$new(d1, c2, cost=c.caffeine, label="Caffeine/Ergotamine")
  # 
  # create lists of nodes and edges
  V <- list(
    d1, c1, c2, c3, c4, c5, c6, c7, c8,
    ta, tb, tc, td, te, tf, tg, th, ti, tj
  )
  E <- list(
    e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18
  )
  # tree
  expect_silent(dt <- DecisionTree$new(V,E))
  # evaluate
  RES <- dt$evaluate()
  # check
  expect_true(is.data.frame(RES))
  c.Sumatriptan <- round(RES[RES$Run==1 & RES$d1=="Sumatriptan", "Cost"],2)
  expect_equal(c.Sumatriptan, 22.06)
  c.Caffeine <- round(RES[RES$Run==1 & RES$d1=="Caffeine/Ergotamine", "Cost"],2)
  expect_equal(c.Caffeine, 4.71)
  u.Sumatriptan <- round(RES[RES$Run==1 & RES$d1=="Sumatriptan", "Utility"],2)
  expect_equal(u.Sumatriptan, 0.42)
  u.Caffeine <- round(RES[RES$Run==1 & RES$d1=="Caffeine/Ergotamine", "Utility"],2)
  expect_equal(u.Caffeine, 0.20)
})

# -----------------------------------------------------------------------------
# Kaminski et al CEJOR 2018;26:135-139, fig 7 (gas problem)
# -----------------------------------------------------------------------------
test_that("rdecision replacates Kaminski et al, fig 7", {
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
  S <- DT$strategies("label")
  expect_equal(nrow(S),6)
  # evaluate one strategy (test/sell/sell)
  RES <- DT$evaluate_strategy(list(E[[5]],E[[7]],E[[12]]))
  expect_true(is.data.frame(RES))
  expect_equal(sum(RES$Probability),1)
  expect_equal(sum(RES$Cost),50)
  expect_equal(sum(RES$Benefit),888)
  # find optimal strategies
  RES <- DT$evaluate()
  expect_equal(nrow(RES),6)
  imax <- which.max(RES$Benefit-RES$Cost)
  popt <- paste(RES$d1[imax], RES$d2[imax], RES$d3[imax], sep="/")
  expect_equal(popt, "test/sell/dig")
})

# ------------------------------------------------------------
# Jenks et al, App Health Econ & Health Policy 2016;14:135-149
# ------------------------------------------------------------
test_that("redecision replicates Jenks et al, 2016", {
  
  # clinical variables
  r.CRBSI <- NormModVar$new(
    'Baseline CRBSI rate', '/1000 catheter days', mu=1.48, sigma=0.074
  )
  hr.CRBSI <- LogNormModVar$new(
    'Tegaderm CRBSI HR', 'ratio', p1=-0.911, p2=0.393
  )
  r.LSI <- NormModVar$new(
    'Baseline LSI rate', '/patient', mu=0.1, sigma=0.01 
  )
  hr.LSI <- LogNormModVar$new(
    'Tegaderm LSI HR', 'ratio', p1=-0.911, p2=0.393
  )
  r.Dermatitis <- NormModVar$new(
    'Baseline dermatitis risk', '/catheter', mu=0.0026, sigma=0.00026
  )
  rr.Dermatitis <- LogNormModVar$new(
    'Tegaderm Dermatitis RR', 'ratio',  p1=1.482, p2=0.490
  )

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
  c.Tegaderm <- ConstModVar$new(
    'Tegaderm CHG cost', 'GBP', const=6.21
  )
  c.Standard <- ConstModVar$new(
    'Standard dressing cost', 'GBP', const=1.34
  )
  n.cathdays <- NormModVar$new(
    'No. days with catheter', 'days', mu=10, sigma=2
  )  
  n.dressings <- NormModVar$new(
    'No. dressings', 'dressings', mu=3, sigma=0.3 
  )

  # probabilities
  p.Dermatitis.S <- ExprModVar$new(
    'P(dermatitis|standard dressing)', 'P', 
    rlang::quo(n.dressings*r.Dermatitis)
  )
  p.Dermatitis.T <- ExprModVar$new(
    'P(dermatitis|Tegaderm)', 'P', 
    rlang::quo(n.dressings*r.Dermatitis*rr.Dermatitis)
  )
  r.LSI.T <- ExprModVar$new(
    'P(LSI|Tegaderm)', 'P', rlang::quo(r.LSI*hr.LSI)
  )
  p.CRBSI.S <- ExprModVar$new(
    'P(CRBSI|standard dressing)', 'P',  rlang::quo(r.CRBSI*n.cathdays/1000)
  )
  p.CRBSI.T <- ExprModVar$new(
    'P(CRBSI|Tegaderm)', 'P', rlang::quo(r.CRBSI*n.cathdays*hr.CRBSI/1000)
  )
  p.NoComp.S <- ExprModVar$new("P(No comp|standard dressing)", "P",
    rlang::quo(1-(p.Dermatitis.S+r.LSI+p.CRBSI.S))
  )
  p.NoComp.T <- ExprModVar$new("P(No comp|Tegaderm)", "P",
    rlang::quo(1-(p.Dermatitis.T+r.LSI.T+p.CRBSI.T))
  )

  # create decision tree
  th <- as.difftime(7, units="days")
  # standard dressing branch
  t1 <- LeafNode$new("Dermatitis", interval=th)
  t2 <- LeafNode$new("LSI", interval=th)
  t3 <- LeafNode$new("CRBSI", interval=th)
  t4 <- LeafNode$new("No comp", interval=th)
  c1 <- ChanceNode$new()
  e1 <- Reaction$new(c1,t1,p=p.Dermatitis.S,cost=c.Dermatitis)
  e2 <- Reaction$new(c1,t2,p=r.LSI,cost=c.LSI)
  e3 <- Reaction$new(c1,t3,p=p.CRBSI.S,cost=c.CRBSI)
  e4 <- Reaction$new(c1,t4,p=p.NoComp.S,cost=0)
  # Tegaderm dressing branch
  t5 <- LeafNode$new("Dermatitis", interval=th)
  t6 <- LeafNode$new("LSI", interval=th)
  t7 <- LeafNode$new("CRBSI", interval=th)
  t8 <- LeafNode$new("No comp", interval=th)
  c2 <- ChanceNode$new()
  e5 <- Reaction$new(c2,t5,p=p.Dermatitis.T,cost=c.Dermatitis)
  e6 <- Reaction$new(c2,t6,p=r.LSI.T,cost=c.LSI)
  e7 <- Reaction$new(c2,t7,p=p.CRBSI.T,cost=c.CRBSI)
  e8 <- Reaction$new(c2,t8,p=p.NoComp.T,cost=0)
  # decision node
  d1 <- DecisionNode$new("d1")
  e9 <- Action$new(d1,c1,label="Standard",cost=c.Standard)
  e10 <- Action$new(d1,c2,label="Tegaderm",cost=c.Tegaderm)
  # create decision tree
  V <- list(d1,c1,c2,t1,t2,t3,t4,t5,t6,t7,t8)
  E <- list(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10)
  expect_silent(DT <- DecisionTree$new(V,E))
  # evaluate the tree
  E <- DT$evaluate()
  str(E)
  print(E)
  
})
