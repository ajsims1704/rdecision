# Script to construct, in rdecision, the model described by Evans et al
# (Pharmacoeconomics 1997;12:565-77) for comparing two drugs used for treating
# migraine to check that its results agree with those presented in the paper, 
# and those reported by Briggs et al, Box 2.3, which replicated the model 
# reported by Evans.
#
# The checks are done as part of the testthat framework, which ensures that 
# any changes in the package code which unintentionally result in deviations
# from the reported results of the model are identified.
#
# Code to construct and run the model is contained within labelled knitr code
# chunks and does not contain test expectations, so can be used by a vignette.
# Unlabelled code chunks may contain testthat expectations and should be
# ignored by a vignette.


## @knitr modvars --------------------------------------------------------------

# Time horizon
th <- as.difftime(24L, units = "hours")

# model variables for cost
c_sumatriptan <- 16.10
c_caffeine <- 1.32
c_ed <- 63.16
c_admission <- 1093.0

# model variables for utility
u_relief_norecurrence <- 1.0
u_relief_recurrence <- 0.9
u_norelief_endures <- -0.30
u_norelief_er <- 0.1

# model variables for effect
p_sumatriptan_recurrence <- 0.594
p_caffeine_recurrence <- 0.703
p_sumatriptan_relief <- 0.558
p_caffeine_relief <- 0.379
p_er <- 0.08
p_admitted <- 0.002


## @knitr model ---------------------------------------------------------------

# Sumatriptan branch
ta <- LeafNode$new("A", utility = u_relief_norecurrence, interval = th)
tb <- LeafNode$new("B", utility = u_relief_recurrence, interval = th)
c3 <- ChanceNode$new()
e1 <- Reaction$new(
  c3, ta, p = p_sumatriptan_recurrence, label = "No recurrence"
)
e2 <- Reaction$new(
  c3, tb, p = 1.0 - p_sumatriptan_recurrence, cost=c_sumatriptan, 
  label = "Relieved 2nd dose"
)
td <- LeafNode$new("D", utility = u_norelief_er, interval = th)
te <- LeafNode$new("E", utility = u_norelief_endures, interval = th)
c7 <- ChanceNode$new()
e3 <- Reaction$new(c7, td, p = 1.0 - p_admitted, label = "Relief")
e4 <- Reaction$new(
  c7, te, p = p_admitted, cost = c_admission, label = "Hospitalization"
)

tc <- LeafNode$new("C", utility = u_norelief_endures, interval = th)
c4 <- ChanceNode$new()
e5 <- Reaction$new(c4, tc, p = 1.0 - p_er, label = "Endures attack")
e6 <- Reaction$new(c4, c7, p = p_er, cost=c_ed, label = "ER")

c1 <- ChanceNode$new()
e7 <- Reaction$new(c1, c3, p = p_sumatriptan_relief, label = "Relief")
e8 <- Reaction$new(c1, c4, p = 1.0 - p_sumatriptan_relief, label = "No relief")

# Caffeine/Ergotamine branch
tf <- LeafNode$new("F", utility = u_relief_norecurrence, interval = th)
tg <- LeafNode$new("G", utility = u_relief_recurrence, interval = th)
c5 <- ChanceNode$new()
e9 <- Reaction$new(c5, tf, p = p_caffeine_recurrence, label = "No recurrence")
e10 <- Reaction$new(
  c5, tg, p = 1.0 - p_caffeine_recurrence, cost=c_caffeine, 
  label = "Relieved 2nd dose"
)
ti <- LeafNode$new("I", utility = u_norelief_er, interval = th)
tj <- LeafNode$new("J", utility = u_norelief_endures, interval = th)
c8 <- ChanceNode$new()
e11 <- Reaction$new(c8, ti, p = 1.0 - p_admitted, label = "Relief")
e12 <- Reaction$new(
  c8, tj, p = p_admitted, cost = c_admission, label = "Hospitalization"
)

th <- LeafNode$new("H", utility = u_norelief_endures, interval = th)
c6 <- ChanceNode$new()
e13 <- Reaction$new(c6, th, p = 1.0 - p_er, label = "Endures attack")
e14 <- Reaction$new(c6, c8, p = p_er, cost = c_ed, label = "ER")

c2 <- ChanceNode$new()
e15 <- Reaction$new(c2, c5, p = p_caffeine_relief, label = "Relief")
e16 <- Reaction$new(c2, c6, p = 1.0 - p_caffeine_relief, label = "No relief")

# decision node
d1 <- DecisionNode$new("d1")
e17 <- Action$new(d1, c1, cost = c_sumatriptan, label = "Sumatriptan")
e18 <- Action$new(d1, c2, cost = c_caffeine, label = "Caffeine-Ergotamine")

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
DT <- DecisionTree$new(V,E)


## @knitr ---------------------------------------------------------------------
test_that("decision tree structure is as per Evans et al", {
  expect_identical(d1$label(), "d1")
})


## @knitr eval_by_path --------------------------------------------------------

ep <- DT$evaluate(by = "path")


## @knitr ---------------------------------------------------------------------

test_that("evaluation by path is as per Box 2.3 of Briggs", {
  expect_identical(nrow(ep), 10L)
  expect_setequal(
    colnames(ep),
    c("Leaf", "d1", "Probability", "Cost", "Benefit", "Utility", "QALY", "Run")
  )
  expect_setequal(ep[, "Leaf"], LETTERS[1L : 10L])
  expect_intol(sum(ep["Probability"]), 2.0, 0.01)
  
  ia <- which(ep[, "Leaf"] == "A")
  expect_identical(ep[[ia, "d1"]], "Sumatriptan")
  expect_intol(ep[[ia, "Probability"]], 0.331, 0.001)
  expect_intol(ep[[ia, "Cost"]], 5.34, 0.01)
  expect_intol(ep[[ia, "Utility"]], 0.33, 0.01)
  
  ih <- which(ep[, "Leaf"] == "H")
  expect_identical(ep[[ih, "d1"]], "Caffeine-Ergotamine")
  expect_intol(ep[[ih, "Probability"]], 0.571, 0.001)
  expect_intol(ep[[ih, "Cost"]], 0.75, 0.01)
  expect_intol(ep[[ih, "Utility"]], -0.17, 0.01)
})


## @knitr eval_by_strategy ---------------------------------------------------

es <- DT$evaluate()


## @knitr icer ---------------------------------------------------------------

is <- which(es[, "d1"] == "Sumatriptan")
cost_s <- es[[is, "Cost"]]
utility_s <- es[[is, "Utility"]]
qaly_s <- es[[is, "QALY"]]

ic <- which(es[, "d1"] == "Caffeine-Ergotamine")
cost_c <- es[[ic, "Cost"]]
utility_c <- es[[ic, "Utility"]]
qaly_c <- es[[ic, "QALY"]]

delta_c <- cost_s - cost_c
delta_u <- utility_s - utility_c
delta_q <- qaly_s - qaly_c
icer <- delta_c / delta_q


## @knitr -------------------------------------------------------------------

test_that("evaluation by strategy is as per Evans et al", {
  
  expect_identical(nrow(es), 2L)
  expect_setequal(
    colnames(es),
    c("d1", "Run", "Probability", "Cost", "Benefit", "Utility", "QALY")
  )
  expect_setequal(es[, "d1"], c("Sumatriptan", "Caffeine-Ergotamine"))
  expect_intol(sum(es["Probability"]), 2.0, 0.01)
  
  expect_intol(cost_s, 22.06, 0.01)
  expect_intol(utility_s, 0.41, 0.01)
  
  expect_intol(cost_c, 4.73, 0.02)
  expect_intol(utility_c, 0.20, 0.01)
  
  expect_between(icer / 29366.0, lower = 0.95, upper = 1.05)
})


## @knitr relief_threshold_upper -----------------------------------------------

p_sumatriptan_relief <- p_caffeine_relief + 0.268
e7$set_probability(p_sumatriptan_relief)
e8$set_probability(1.0 - p_sumatriptan_relief)
es <- DT$evaluate()


## @knitr ---------------------------------------------------------------------

test_that("upper relief threshold ICER agrees with Evans et al", {
  
  is <- which(es[, "d1"] == "Sumatriptan")
  cost_s <- es[[is, "Cost"]]
  utility_s <- es[[is, "Utility"]]
  qaly_s <- es[[is, "QALY"]]
  
  ic <- which(es[, "d1"] == "Caffeine-Ergotamine")
  cost_c <- es[[ic, "Cost"]]
  utility_c <- es[[ic, "Utility"]]
  qaly_c <- es[[ic, "QALY"]]
  
  delta_c <- cost_s - cost_c
  delta_u <- utility_s - utility_c
  delta_q <- qaly_s - qaly_c
  icer <- delta_c / delta_q
  
  expect_between(icer / 18950.0, lower = 0.95, upper = 1.05)
})


## @knitr relief_threshold_lower -----------------------------------------------

p_sumatriptan_relief <- p_caffeine_relief + 0.091
e7$set_probability(p_sumatriptan_relief)
e8$set_probability(1.0 - p_sumatriptan_relief)
es <- DT$evaluate()


## @knitr ---------------------------------------------------------------------

test_that("lower relief threshold ICER agrees with Evans et al", {
  
  is <- which(es[, "d1"] == "Sumatriptan")
  cost_s <- es[[is, "Cost"]]
  utility_s <- es[[is, "Utility"]]
  qaly_s <- es[[is, "QALY"]]
  
  ic <- which(es[, "d1"] == "Caffeine-Ergotamine")
  cost_c <- es[[ic, "Cost"]]
  utility_c <- es[[ic, "Utility"]]
  qaly_c <- es[[ic, "QALY"]]
  
  delta_c <- cost_s - cost_c
  delta_u <- utility_s - utility_c
  delta_q <- qaly_s - qaly_c
  icer <- delta_c / delta_q
  
  expect_between(icer / 60839.0, lower = 0.95, upper = 1.05)
})



## @knitr threshold_model -----------------------------------------------------------

# model variables with uncertainty
p_sumatriptan_relief <- ConstModVar$new(
  "P(relief|sumatriptan)", "P", 0.558
)
q_sumatriptan_relief <- ExprModVar$new(
  "Q(relief|sumatriptan)", "P", rlang::quo(1.0 - p_sumatriptan_relief)
)

# create edges associated with model variables
e7 <- Reaction$new(c1, c3, p = p_sumatriptan_relief, label="Relief")
e8 <- Reaction$new(c1, c4, p = q_sumatriptan_relief, label="No relief")

e15 <- Reaction$new(c2, c5, p = p_caffeine_relief, label="Relief")
e16 <- Reaction$new(c2, c6, p = 1.0 - p_caffeine_relief, label="No relief")

# rebuild the model
E <- list(
  e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, 
  e17, e18
)
dt <- DecisionTree$new(V,E)


## @knitr relief_thresholds ---------------------------------------------------

# upper 95% relief rate threshold for ICER (Table VIII)
p_relief_upper <- dt$threshold(
  index = list(e17), ref = list(e18), outcome = "ICER", 
  mvd = p_sumatriptan_relief$description(), 
  a = 0.6, b = 0.7,
  lambda = 18950.0, tol = 0.0001
)
# lower 95% relief rate threshold for ICER (Table VIII)
p_relief_lower <- dt$threshold(
  index = list(e17), ref = list(e18), outcome = "ICER", 
  mvd = p_sumatriptan_relief$description(), 
  a = 0.4, b = 0.5,
  lambda = 60839.0, tol = 0.0001
)


## @knitr ---------------------------------------------------------------------

test_that("ICER thresholds agree with Evans et al", {
  # check parameters of threshold function
  expect_error(
    dt$threshold(
      index=list(e17), ref=list(e18), outcome="ICER", 
      mvd = p_sumatriptan_relief$description(), 
      a=0.5, b=0.6,
      lambda=-1.0, tol=0.0001
    ),
    class = "invalid_lambda"
  )
  expect_error(
    dt$threshold(
      index=list(e17), ref=list(e18), outcome="ICER", 
      mvd = p_sumatriptan_relief$description(), 
      a=0.1, b=0.2,
      lambda=29366.0, tol=0.0001
    ),
    class = "invalid_brackets"
  )
  expect_error(
    dt$threshold(
      index=list(e17), ref=list(e18), outcome="ICER", 
      mvd = p_sumatriptan_relief$description(), 
      a=0.5, b=0.6,
      lambda=29366.0, tol=0.0001, nmax=5L
    ),
    class = "convergence_failure"
  )
  # mean relief rate threshold for ICER
  pt <- dt$threshold(
    index=list(e17), ref=list(e18), outcome="ICER", 
    mvd = p_sumatriptan_relief$description(), 
    a=0.5, b=0.6,
    lambda=29366.0, tol=0.0001
  )
  expect_intol(pt, p_caffeine_relief+0.179, tol=0.02)
  # check values against Table VIII
  expect_intol(p_relief_upper, p_caffeine_relief + 0.268, tol = 0.02)
  expect_intol(p_relief_lower, p_caffeine_relief + 0.091, tol = 0.02)
})


## @knitr psa_model ----------------------------------------------------------

# model variables with uncertainty
c_sumatriptan <- GammaModVar$new(
  "Sumatriptan","CAD", shape = 16.10, scale = 1.0
)
c_caffeine <- GammaModVar$new(
  "Caffeine", "CAD", shape = 1.32, scale = 1.0
)

# create edges with model variables
e2 <- Reaction$new(c3, tb, p=0.406, cost=c_sumatriptan, 
                   label="Relieved 2nd dose")
e10 <- Reaction$new(c5, tg, p=0.297, cost=c_caffeine, 
                    label="Relieved 2nd dose")
e17 <- Action$new(d1, c1, cost=c_sumatriptan, label="Sumatriptan")
e18 <- Action$new(d1, c2, cost=c_caffeine, label="Caffeine")

# rebuild the model
E <- list(
  e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, 
  e17, e18
)
dt <- DecisionTree$new(V,E)


## @knitr ---------------------------------------------------------------------

test_that("tornado diagram ICER ranges agree with Evans et al", {
  # check ICER ranges in tornado diagram (branches B and G get 2nd dose)
  TO <- dt$tornado(index=list(e17),ref=list(e18),outcome="ICER",draw=FALSE)
  #dq <- (q.Sumatriptan-q.Caffeine)
  c_sumatriptan$set("expected")
  c_caffeine$set("expected")
  p_sumatriptan_relief$set("expected")
  x <- qgamma(p=0.025,shape=16.10,rate=1.0)
  expect_intol(TO$LL[TO$Description=="Sumatriptan"], x,tol=0.01)
  deltac <- (x-c_sumatriptan$get())*1.227
  expect_intol(
    TO$outcome.min[TO$Description=="Sumatriptan"],
    (cost_s - cost_c + deltac) / delta_q,
    tol=100.0
  )
  x <- qgamma(p=0.975,shape=16.10,rate=1.0)
  expect_intol(TO$UL[TO$Description=="Sumatriptan"],x,tol=0.01)
  deltac <- (x-c_sumatriptan$get())*1.227
  expect_intol(
    TO$outcome.max[TO$Description=="Sumatriptan"],
    (cost_s - cost_c + deltac) / delta_q,
    tol=100.0
  )
  x <- qgamma(p=0.025,shape=1.32,rate=1.0)
  expect_intol(TO$LL[TO$Description=="Caffeine"], x,tol=0.01)
  deltac <- (c_caffeine$get()-x)*1.113
  expect_intol(
    TO$outcome.min[TO$Description=="Caffeine"],
    (cost_s - cost_c + deltac) / delta_q,
    tol=100.0
  )
  x <- qgamma(p=0.975,shape=1.32, rate=1.0)
  expect_intol(TO$UL[TO$Description=="Caffeine"],x,tol=0.01)
  deltac <- (c_caffeine$get()-x)*1.113
  expect_intol(
    TO$outcome.max[TO$Description=="Caffeine"],
    (cost_s - cost_c + deltac) / delta_q,
    tol=100.0
  )
})
