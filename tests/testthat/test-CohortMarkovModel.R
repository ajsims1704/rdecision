
# -----------------------------------------------------------------------------
# tests of creating a model
# -----------------------------------------------------------------------------
test_that("incorrect state types are rejected", {
  s.well <- MarkovState$new("WELL")
  expect_error(
    CohortMarkovModel$new(
      V=list(s.well, "DISABLED", "STROKE", "DEAD"), 
      E=list()
    ), 
    class="non-Node_vertex"
  )  
  n1 <- Node$new()
  e.ww <- MarkovTransition$new(s.well, s.well)
  expect_error(
    CohortMarkovModel$new(
      V=list(s.well, n1), 
      E=list(e.ww)
    ), 
    class="invalid_state"
  )  
})

test_that("incorrect transition types are rejected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.wd <- MarkovTransition$new(s.well, s.dead)
  e.dd <- Arrow$new(s.dead, s.dead)
  expect_error(
    CohortMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.wd, e.dd)
    ), 
    class="invalid_transition"
  )  
})

test_that("edge case graphs are rejected", {
  # empty graph
  expect_error(
    CohortMarkovModel$new(V=list(), E=list()),
    class = "invalid_graph"
  )
  # single node, no edges
  s.dead <- MarkovState$new("Dead")
  expect_error(
    CohortMarkovModel$new(V=list(s.dead), E=list()),
    class = "invalid_graph"
  )
  # minimal model
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  expect_silent(CohortMarkovModel$new(V=list(s.dead),E=list(e.dd)))
})

test_that("multiple digraph edges are rejected", {
  s.well <- MarkovState$new("Well")
  s.dead <- MarkovState$new("Dead")
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.ww.bleed <- MarkovTransition$new(s.well, s.well, r=0.2)
  e.wd <- MarkovTransition$new(s.well, s.dead, r=0.1)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  # two self loops from well to well
  expect_error(
    CohortMarkovModel$new(
      V = list(s.well, s.dead), 
      E = list(e.ww, e.ww.bleed, e.wd, e.dd)
    ),
    class = "multiple_edges"
  )
  # two loops from well to dead
  e.wd.bleed <- MarkovTransition$new(s.well, s.dead, r=NULL)
  expect_error(
    CohortMarkovModel$new(
      V = list(s.well, s.dead), 
      E = list(e.ww, e.wd.bleed, e.wd, e.dd)
    ),
    class = "multiple_edges"
  )
  # but multiple edges for graph but not digraph are allowed
  e.dw <- MarkovTransition$new(s.dead, s.well, r=0.1)
  expect_silent(
    CohortMarkovModel$new(
      V = list(s.well, s.dead), 
      E = list(e.ww, e.wd, e.dw, e.dd)
    )
  )
})

test_that("unconnected underlying graphs are detected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.wd <- MarkovTransition$new(s.well, s.dead)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  expect_error(
    CohortMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.wd, e.dd)
    ), 
    class="invalid_graph"
  )  
})

test_that("states without one NULL rate are detected", {
  # cycle time
  tcycle = as.difftime(365.25, units="days")
  # create states
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  # create transitions (each with NULL rates)
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.ss <- MarkovTransition$new(s.disabled, s.disabled)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  e.ws <- MarkovTransition$new(s.well, s.disabled)
  e.wd <- MarkovTransition$new(s.well, s.dead)
  e.sd <- MarkovTransition$new(s.disabled, s.dead)
  # create the Markov model
  MC <- CohortMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
  ) 
  # under-constrain (no rates specified for outgoing 'disabled' state)
  e.ws$set_rate(r=0.2)
  e.wd$set_rate(r=0.2)
  expect_error(
    MC$transition_probability(tcycle),
    class = "invalid_rate"
  )
  # over-constrain (all rates specified for 'dead' state)
  e.sd$set_rate(r=0.4)
  e.dd$set_rate(r=1)
  expect_error(
    MC$transition_probability(tcycle),
    class="invalid_rate"
  )
  # correctly specified 
  e.dd$set_rate(r=NULL)
  e.sd$set_rate(r=0.4)
  expect_silent(
    MC$transition_probability(tcycle)
  )
})

test_that("the transition matrix has the correct properties and values", {
  # create the model
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.ss <- MarkovTransition$new(s.disabled, s.disabled)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  r.ws <- -log(1-0.2)/1
  e.ws <- MarkovTransition$new(s.well, s.disabled, r=r.ws)
  r.wd <- -log(1-0.2)/1
  e.wd <- MarkovTransition$new(s.well, s.dead, r=r.wd)
  r.sd <- -log(1-0.4)/1
  e.sd <- MarkovTransition$new(s.disabled, s.dead, r=r.sd)
  M <- CohortMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
  )
  # check the transition matrix properties
  Ip <- M$transition_probability(as.difftime(365.25, units="days"))
  expect_equal(nrow(Ip),3)
  expect_equal(ncol(Ip),3)
  dn <- dimnames(Ip)
  expect_setequal(names(dn),list("source","target"))
  expect_setequal(dn[[1]],list("Well","Disabled","Dead"))
  expect_setequal(dn[[2]],list("Well","Disabled","Dead"))
  expect_true(is.matrix(Ip))
  # check the transition matrix values
  expect_equal(
    sum(Ip-matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)),0
  )
})

# -----------------------------------------------------------------------------
# tests of getting and setting state populations
# -----------------------------------------------------------------------------
test_that("invalid population vectors are rejected", {
  # create the model
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.ss <- MarkovTransition$new(s.disabled, s.disabled)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  e.ws <- MarkovTransition$new(s.well, s.disabled, r=0.2)
  e.wd <- MarkovTransition$new(s.well, s.dead, r=0.2)
  e.sd <- MarkovTransition$new(s.disabled, s.dead, r=0.4)
  M <- CohortMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
  )
  # check state names
  expect_setequal(M$get_statenames(), list("Well", "Disabled", "Dead"))
  # check default population
  rp <- M$get_populations()
  expect_equal(unname(rp[1]),1000)
  expect_equal(unname(rp[2]),0)
  expect_equal(unname(rp[3]),0)
  # number of elements
  pop <- c(Well=10000, Disabled=0)
  expect_error(M$set_populations(pop), class="incorrect_state_count")
  # state names
  pop <- c(Well=10000, Poorly=0, Disabled=0)
  expect_error(M$set_populations(pop), class="unmatched_states")
  pop <- c(10000, 0, 0)
  expect_error(M$set_populations(pop), class="unmatched_states")
  # type
  pop <- c(Well=10000, Disabled="0", Dead=0)
  expect_error(M$set_populations(pop), class="non-numeric_state_population")
  # correct
  pop <- c(Well=10000, Disabled=0, Dead=0)
  expect_silent(M$set_populations(pop))
  rp <- M$get_populations()
  expect_equal(unname(rp["Well"]), 10000)
  expect_equal(unname(rp["Disabled"]), 0)
  expect_equal(unname(rp["Dead"]), 0)
})

# -----------------------------------------------------------------------------
# tests of cycling
# -----------------------------------------------------------------------------
test_that("model is cyclable", {
  # create states
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new(name="Disabled")
  s.dead <- MarkovState$new(name="Dead")
  # create transitions
  r.ws <- -log(1-0.2)/1
  r.wd <- -log(1-0.2)/1
  r.sd <- -log(1-0.4)/1
  E <- list(
    MarkovTransition$new(s.well, s.well),
    MarkovTransition$new(s.dead, s.dead),
    MarkovTransition$new(s.disabled, s.disabled),
    MarkovTransition$new(s.well, s.disabled, r=r.ws),
    MarkovTransition$new(s.well, s.dead, r=r.wd),
    MarkovTransition$new(s.disabled, s.dead, r=r.sd)
  )
  # create the model
  M <- CohortMarkovModel$new(V = list(s.well, s.disabled, s.dead), E)
  # test cycles
  DF <- M$cycle()
  expect_true(is.data.frame(DF))
  expect_setequal(
    names(DF), 
    c("State", "Cycle", "Time", "Population", "EntryCost", "OccCost", "Cost", 
      "QALY")
  )
  expect_equal(nrow(DF),3)
})

# ---------------------------------------------------------------------------
# tests of rates
# ---------------------------------------------------------------------------
test_that("results are independent of cycle time", {
  # create states
  s.well <- MarkovState$new(name="Well")
  s.dead <- MarkovState$new(name="Dead")
  # create transitions
  r.wd <- -log(1-0.2)/1
  E <- list(
    MarkovTransition$new(s.well, s.well),
    MarkovTransition$new(s.dead, s.dead),
    MarkovTransition$new(s.well, s.dead, r=r.wd)
  )
  # create the model and cycle for 5 years
  M <- CohortMarkovModel$new(V = list(s.well, s.dead), E)
  MT <- M$cycles(5, hcc=FALSE)
  expect_equal(MT$Well[MT$Cycle==5], 327.68)
  # create the model and cycle for 5 years
  M <- CohortMarkovModel$new(V = list(s.well, s.dead), E)
  MT <- M$cycles(
    5*12, 
    hcc = FALSE, 
    tcycle = as.difftime(365.25/12, units="days")
  )
  expect_equal(MT$Well[MT$Cycle==60], 327.68)
})

# -----------------------------------------------------------------------------
# tests of model variables
# -----------------------------------------------------------------------------
test_that("Model variables are recognised, set and got", {
  # create modvars
  c.dis <- GammaModVar$new("Care cost", "GBP", shape=1000, scale=1/10)
  u.dis <- BetaModVar$new("u.dis", "U", alpha=7, beta=3)
  p.dis <- BetaModVar$new("p.disabled", "P", alpha=20, beta=80)
  p.ded <- BetaModVar$new("p.dead", "P", alpha=40, beta=60)
  # create states
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new(name="Disabled", cost=c.dis, utility=u.dis)
  s.dead <- MarkovState$new(name="Dead")
  # create transitions
  r.ws <- ExprModVar$new("r.ws", "HR", rlang::quo(-log(1-p.dis)/1))
  r.wd <- ExprModVar$new("r.wd", "HR", rlang::quo(-log(1-p.dis)/1))
  r.sd <- ExprModVar$new("r.sd", "HR", rlang::quo(-log(1-p.ded)/1))
  E <- list(
    MarkovTransition$new(s.well, s.well),
    MarkovTransition$new(s.dead, s.dead),
    MarkovTransition$new(s.disabled, s.disabled),
    e.ws <- MarkovTransition$new(s.well, s.disabled, r=r.ws),
    e.wd <- MarkovTransition$new(s.well, s.dead, r=r.wd),
    e.sd <- MarkovTransition$new(s.disabled, s.dead, r=r.sd)
  )
  # create the Markov model
  M <- CohortMarkovModel$new(V = list(s.well, s.disabled, s.dead), E)
  # extract the model variables
  MV <- M$modvars()
  expect_equal(length(MV), 7)
  # tabulate the input variables
  MVT <- M$modvar_table(expressions=FALSE)
  expect_equal(nrow(MVT), 4)
})

# -----------------------------------------------------------------------------
# Sonnenberg & Beck, Med Decis Making, 1993;13:322, Fig 3
# (prosthetic heart valve)
# -----------------------------------------------------------------------------
test_that("rdecision replicates Sonnenberg & Beck, Fig 3", {
  # cycle time
  tcycle = as.difftime(365.25, units="days")
  # create states
  s.well <- MarkovState$new(name="Well", utility=1)
  s.disabled <- MarkovState$new(name="Disabled",utility=0.7)
  s.dead <- MarkovState$new(name="Dead",utility=0)
  # create transitions
  r.ws <- -log(1-0.2)/1
  r.wd <- -log(1-0.2)/1
  r.sd <- -log(1-0.4)/1
  E <- list(
    MarkovTransition$new(s.well, s.well),
    MarkovTransition$new(s.dead, s.dead),
    MarkovTransition$new(s.disabled, s.disabled),
    e.ws <- MarkovTransition$new(s.well, s.disabled, r=r.ws),
    e.wd <- MarkovTransition$new(s.well, s.dead, r=r.wd),
    e.sd <- MarkovTransition$new(s.disabled, s.dead, r=r.sd)
  )
  # create the model
  M <- CohortMarkovModel$new(V = list(s.well, s.disabled, s.dead), E)
  # check the transition matrix values
  Ip <- M$transition_probability(tcycle)
  expect_equal(
    sum(Ip-matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)),0
  )
  # set the starting populations
  M$set_populations(c(Well=10000, Disabled=0, Dead=0)) 
  # cycle
  RC <- M$cycles(25, tcycle=tcycle, hcc=FALSE)
  expect_true(is.data.frame(RC))
  expect_equal(round(RC$Well[RC$Cycle==2]), 3600)
  expect_equal(round(RC$Disabled[RC$Cycle==2]), 2400)
  expect_equal(round(RC$Dead[RC$Cycle==2]), 4000)
  expect_true(TRUE)
})


# ---------------------------------------------------------------------------
# Chancellor, 1997 (HIV) and Briggs exercise 2.5
# ---------------------------------------------------------------------------
test_that("redecision replicates Briggs' example 2.5", {
  # transition rates calculated from annual transition probabilities
  trAB <- -log(1-0.202)/1 
  trAC <- -log(1-0.067)/1
  trAD <- -log(1-0.010)/1
  trBC <- -log(1-0.407)/1
  trBD <- -log(1-0.012)/1
  trCD <- -log(1-0.250)/1
  # Costs
  dmca <- 1701 # direct medical costs associated with state A
  dmcb <- 1774 # direct medical costs associated with state B
  dmcc <- 6948 # direct medical costs associated with state C
  ccca <- 1055 # Community care costs associated with state A
  cccb <- 1278 # Community care costs associated with state B
  cccc <- 2059 # Community care costs associated with state C
  # Drug costs
  cAZT <- 2278 # zidovudine drug cost
  cLam <- 2087 # lamivudine drug cost
  # Other parameters
  RR <- 0.509 # treatment effect
  cDR <- 6 # annual discount rate, costs (%)
  oDR <- 0 # annual discount rate, benefits (%)
  # create Markov states for monotherapy (zidovudine only)
  sA <- MarkovState$new("A", cost=dmca+ccca+cAZT)
  sB <- MarkovState$new("B", cost=dmcb+cccb+cAZT)
  sC <- MarkovState$new("C", cost=dmcc+cccc+cAZT)
  sD <- MarkovState$new("D", cost=0, utility=0)
  # create transitions
  tAA <- MarkovTransition$new(sA, sA, r=NULL)
  tAB <- MarkovTransition$new(sA, sB, r=trAB)
  tAC <- MarkovTransition$new(sA, sC, r=trAC)
  tAD <- MarkovTransition$new(sA, sD, r=trAD)
  tBB <- MarkovTransition$new(sB, sB, r=NULL)
  tBC <- MarkovTransition$new(sB, sC, r=trBC)
  tBD <- MarkovTransition$new(sB, sD, r=trBD)
  tCC <- MarkovTransition$new(sC, sC, r=NULL)
  tCD <- MarkovTransition$new(sC, sD, r=trCD)
  tDD <- MarkovTransition$new(sD, sD, r=NULL)
  # construct the model
  mhiv <- CohortMarkovModel$new(
    V = list(sA, sB, sC, sD),
    E = list(tAA, tAB, tAC, tAD, tBB, tBC, tBD, tCC, tCD, tDD),
    discount.cost = cDR/100,
    discount.utility = oDR/100
  )
  # tabulate states
  DF <- mhiv$tabulate_states()
  expect_equal(nrow(DF), 4)
  expect_equal(DF[1,"Name"], "A")
  expect_equal(DF[1,"Cost"], 2756+2278)
  expect_equal(DF[2,"Cost"], 3052+2278)
  expect_equal(DF[3,"Cost"], 9007+2278)
  expect_equal(DF[4,"Cost"], 0)
  # per-cycle transition probabilities
  tcycle = as.difftime(365.24, units="days")
  TM <- mhiv$transition_probability(tcycle)
  E <- matrix(
    c(0.721, 0.202, 0.067, 0.010,  
      0.000, 0.581, 0.407, 0.012,
      0.000, 0.000, 0.750, 0.250,
      0.000, 0.000, 0.000, 1.000),   # typo in book (D,D) = 1!
    byrow = TRUE,
    nrow = 4)
  expect_equal(sum(TM-E), 0)
  # create starting populations
  N <- 1000
  populations <- c(A = N, B = 0, C = 0, D = 0)
  mhiv$set_populations(populations)
  # run 20 cycles
  MT.mono <- mhiv$cycles(ncycles=20, tcycle=tcycle, hcc=FALSE)
  # monotherapy results
  el.mono <- sum(MT.mono$QALY)
  expect_intol(el.mono, 7.991, tolerance=0.03) # 7.991 from spreadsheet
  cost.mono <- sum(MT.mono$Cost)
  expect_intol(cost.mono, 44663, 100) # rounding errors in book
  #
  # set occupancy costs for combination therapy (zidovudine and lamivudine)
  sA$set_cost(dmca+ccca+cAZT+cLam)
  sB$set_cost(dmcb+cccb+cAZT+cLam)
  sC$set_cost(dmcc+cccc+cAZT+cLam)
  # apply treatment effect to annual probabilities
  trABm <- -log(1-0.202*RR)/1 
  trACm <- -log(1-0.067*RR)/1
  trADm <- -log(1-0.010*RR)/1
  trBCm <- -log(1-0.407*RR)/1
  trBDm <- -log(1-0.012*RR)/1
  trCDm <- -log(1-0.250*RR)/1
  # update transition rates
  tAB$set_rate(trABm)
  tAC$set_rate(trACm)
  tAD$set_rate(trADm)
  tBC$set_rate(trBCm)
  tBD$set_rate(trBDm)
  tCD$set_rate(trCDm)
  # check transition matrix
  TM <- mhiv$transition_probability(tcycle)
  E <- matrix(
    c(0.858, 0.103, 0.034, 0.005,  
      0.000, 0.787, 0.207, 0.006,
      0.000, 0.000, 0.873, 0.127,
      0.000, 0.000, 0.000, 1.000),   
    byrow = TRUE,
    nrow = 4)
  expect_equal(sum(TM-E), 0)
  # run combination therapy model for 2 years
  populations <- c('A'=N, 'B'=0, 'C'=0, 'D'=0)
  mhiv$set_populations(populations)
  # run 2 cycles
  MT.comb <- mhiv$cycles(2, tcycle=tcycle, hcc=FALSE)
  # revise costs and revert transition rates 
  sA$set_cost(dmca+ccca+cAZT)
  sB$set_cost(dmcb+cccb+cAZT)
  sC$set_cost(dmcc+cccc+cAZT)
  tAB$set_rate(trAB)
  tAC$set_rate(trAC)
  tAD$set_rate(trAD)
  tBC$set_rate(trBC)
  tBD$set_rate(trBD)
  tCD$set_rate(trCD)
  # and run model for next 18 years
  MT.comb <- rbind(MT.comb, mhiv$cycles(ncycles=18, tcycle=tcycle,hcc=FALSE)) 
  # combination therapy results
  el.comb <- sum(MT.comb$QALY)
  expect_intol(el.comb, 8.937, tolerance=0.02) # 8.937 from spreadsheet
  cost.comb <- sum(MT.comb$Cost)
  expect_intol(cost.comb, 50602, 100) # rounding errors in book
  # icer
  icer <- (cost.comb-cost.mono)/(el.comb-el.mono)
  expect_intol(icer, 6276, 10) # rounding errors in book
})

# ---------------------------------------------------------------------------
# Chancellor, 1997 (HIV) with PSA and Briggs exercise 4.7 
# ---------------------------------------------------------------------------
test_that("redecision replicates Briggs' example 4.7", {
  #
  # Discount rates
  # ==============
  cDR <- 6 # annual discount rate, costs (%)
  oDR <- 0 # annual discount rate, benefits (%)
  #
  # # Model structure
  # # ===============
  # # states
  # sA <- MarkovState$new("A")
  # sB <- MarkovState$new("B")
  # sC <- MarkovState$new("C")
  # sD <- MarkovState$new("D", cost=0, utility=0)
  # # transitions
  # tAA <- MarkovTransition$new(sA, sA, r=NULL)
  # tAB <- MarkovTransition$new(sA, sB)
  # tAC <- MarkovTransition$new(sA, sC)
  # tAD <- MarkovTransition$new(sA, sD)
  # tBB <- MarkovTransition$new(sB, sB, r=NULL)
  # tBC <- MarkovTransition$new(sB, sC)
  # tBD <- MarkovTransition$new(sB, sD)
  # tCC <- MarkovTransition$new(sC, sC, r=NULL)
  # tCD <- MarkovTransition$new(sC, sD)
  # tDD <- MarkovTransition$new(sD, sD, r=NULL)
  # # model
  # mhiv <- CohortMarkovModel$new(
  #   V = list(sA, sB, sC, sD),
  #   E = list(tAA, tAB, tAC, tAD, tBB, tBC, tBD, tCC, tCD, tDD),
  #   discount.cost = cDR/100,
  #   discount.utility = oDR/100
  # )
  # Cycle time
  # ==========
  tcycle = as.difftime(365.24, units="days")
  #
  # Transition rates
  # ================
  # Dirichlet distribution & model variables for transitions from A
  DA <- DirichletDistribution$new(c(1251, 350, 116, 17))
  pAB <- ModVar$new("pAB", "P", D=DA, k=as.integer(2))
  pAC <- ModVar$new("pCB", "P", D=DA, k=as.integer(3))
  pAD <- ModVar$new("pAD", "P", D=DA, k=as.integer(4))
  # Dirichlet distribution & model variables for transitions from B
  DB <- DirichletDistribution$new(c(731, 512, 15))
  pBC <- ModVar$new("pBC", "P", D=DB, k=as.integer(2)) 
  pBD <- ModVar$new("pBD", "P", D=DB, k=as.integer(3)) 
  # Dirichlet distribution & model variables for transitions from C
  DC <- DirichletDistribution$new(c(1312, 437))
  pCD <- ModVar$new("pCD", "P", D=DC, k=as.integer(2)) 
  # transition rates with monotherapy from annual transition probabilities
  trABm <- ExprModVar$new("trAB", "HR", rlang::quo(-log(1-pAB)/1))
  trACm <- ExprModVar$new("trAC", "HR", rlang::quo(-log(1-pAC)/1))
  trADm <- ExprModVar$new("trAD", "HR", rlang::quo(-log(1-pAD)/1))
  trBCm <- ExprModVar$new("trBC", "HR", rlang::quo(-log(1-pBC)/1))
  trBDm <- ExprModVar$new("trBD", "HR", rlang::quo(-log(1-pBD)/1))
  trCDm <- ExprModVar$new("trCD", "HR", rlang::quo(-log(1-pCD)/1))
  # Treatment effect (modelled as a log normal distribution)
  RR <- LogNormModVar$new(
    "Tx effect", "RR", p1=0.509, p2=(0.710-0.365)/(2*1.96), "LN7"
  )
  # transition rates with treatment effect
  trABc <- ExprModVar$new("trAB", "HR", rlang::quo(-log(1-pAB*RR)/1))
  trACc <- ExprModVar$new("trAC", "HR", rlang::quo(-log(1-pAC*RR)/1))
  trADc <- ExprModVar$new("trAD", "HR", rlang::quo(-log(1-pAD*RR)/1))
  trBCc <- ExprModVar$new("trBC", "HR", rlang::quo(-log(1-pBC*RR)/1))
  trBDc <- ExprModVar$new("trBD", "HR", rlang::quo(-log(1-pBD*RR)/1))
  trCDc <- ExprModVar$new("trCD", "HR", rlang::quo(-log(1-pCD*RR)/1))
  #
  # Costs
  # =====
  # drug costs
  cAZT <- 2278 # zidovudine drug cost
  cLam <- 2087 # lamivudine drug cost
  # direct medical and community costs (modelled as gamma distributions)
  dmca <- GammaModVar$new("dmca", "GBP", shape=1, scale=1701)
  dmcb <- GammaModVar$new("dmcb", "GBP", shape=1, scale=1774)
  dmcc <- GammaModVar$new("dmcc", "GBP", shape=1, scale=6948)
  ccca <- GammaModVar$new("ccca", "GBP", shape=1, scale=1055)
  cccb <- GammaModVar$new("cccb", "GBP", shape=1, scale=1278)
  cccc <- GammaModVar$new("cccc", "GBP", shape=1, scale=2059)
  # occupancy costs with monotherapy
  cAm <- ExprModVar$new("cA", "GBP", rlang::quo(dmca+ccca+cAZT))
  cBm <- ExprModVar$new("cB", "GBP", rlang::quo(dmcb+cccb+cAZT))
  cCm <- ExprModVar$new("cC", "GBP", rlang::quo(dmcc+cccc+cAZT))
  # occupancy costs with combination therapy
  cAc <- ExprModVar$new("cAc", "GBP", rlang::quo(dmca+ccca+cAZT+cLam))
  cBc <- ExprModVar$new("cBc", "GBP", rlang::quo(dmcb+cccb+cAZT+cLam))
  cCc <- ExprModVar$new("cCc", "GBP", rlang::quo(dmcc+cccc+cAZT+cLam))
  #
  # Monotherapy model
  # =================
  # states
  sAm <- MarkovState$new("A", cost=cAm)
  sBm <- MarkovState$new("B", cost=cBm)
  sCm <- MarkovState$new("C", cost=cCm)
  sDm <- MarkovState$new("D", cost=0, utility=0)
  # transitions
  tAAm <- MarkovTransition$new(sAm, sAm, r=NULL)
  tABm <- MarkovTransition$new(sAm, sBm, r=trABm)
  tACm <- MarkovTransition$new(sAm, sCm, r=trACm)
  tADm <- MarkovTransition$new(sAm, sDm, r=trADm)
  tBBm <- MarkovTransition$new(sBm, sBm, r=NULL)
  tBCm <- MarkovTransition$new(sBm, sCm, r=trBCm)
  tBDm <- MarkovTransition$new(sBm, sDm, r=trBDm)
  tCCm <- MarkovTransition$new(sCm, sCm, r=NULL)
  tCDm <- MarkovTransition$new(sCm, sDm, r=trCDm)
  tDDm <- MarkovTransition$new(sDm, sDm, r=NULL)
  # model
  m.mono <- CohortMarkovModel$new(
    V = list(sAm, sBm, sCm, sDm),
    E = list(tAAm, tABm, tACm, tADm, tBBm, tBCm, tBDm, tCCm, tCDm, tDDm),
    discount.cost = cDR/100,
    discount.utility = oDR/100
  )
  #
  # Combination therapy model
  # =========================
  # states
  sAc <- MarkovState$new("A", cost=cAc)
  sBc <- MarkovState$new("B", cost=cBc)
  sCc <- MarkovState$new("C", cost=cCc)
  sDc <- MarkovState$new("D", cost=0, utility=0)
  # transitions
  tAAc <- MarkovTransition$new(sAc, sAc, r=NULL)
  tABc <- MarkovTransition$new(sAc, sBc, r=trABc)
  tACc <- MarkovTransition$new(sAc, sCc, r=trACc)
  tADc <- MarkovTransition$new(sAc, sDc, r=trADc)
  tBBc <- MarkovTransition$new(sBc, sBc, r=NULL)
  tBCc <- MarkovTransition$new(sBc, sCc, r=trBCc)
  tBDc <- MarkovTransition$new(sBc, sDc, r=trBDc)
  tCCc <- MarkovTransition$new(sCc, sCc, r=NULL)
  tCDc <- MarkovTransition$new(sCc, sDc, r=trCDc)
  tDDc <- MarkovTransition$new(sDc, sDc, r=NULL)
  # model
  m.comb <- CohortMarkovModel$new(
    V = list(sAc, sBc, sCc, sDc),
    E = list(tAAc, tABc, tACc, tADc, tBBc, tBCc, tBDc, tCCc, tCDc, tDDc),
    discount.cost = cDR/100,
    discount.utility = oDR/100
  )
  # Function to estimate life years gained and costs
  # ================================================
  runmodel <- function() {
    # 
    # Monotherapy
    # ===========
    # set costs for monotherapy
    #sA$set_cost(cA)
    #sB$set_cost(cB)
    #sC$set_cost(cC)
    # set rates for monotherapy
    #tAB$set_rate(trAB)
    #tAC$set_rate(trAC)
    #tAD$set_rate(trAD)
    #tBC$set_rate(trBC)
    #tBD$set_rate(trBD)
    #tCD$set_rate(trCD)
    # create starting populations
    N <- 1000
    populations <- c(A = N, B = 0, C = 0, D = 0)
    m.mono$set_populations(populations)
    # run 20 cycles
    MT.mono <- m.mono$cycles(ncycles=20, tcycle=tcycle, hcc=FALSE)
    # expected life years and costs
    el.mono <- sum(MT.mono$QALY)
    cost.mono <- sum(MT.mono$Cost)
    #
    # Combination therapy
    # ===================
    # occupancy costs for combination therapy (zidovudine and lamivudine)
    #sA$set_cost(cAc)
    #sB$set_cost(cBc)
    #sC$set_cost(cCc)
    # transition rates for combination therapy
    #tAB$set_rate(trABm)
    #tAC$set_rate(trACm)
    #tAD$set_rate(trADm)
    #tBC$set_rate(trBCm)
    #tBD$set_rate(trBDm)
    #tCD$set_rate(trCDm)
    # run combination therapy model for 2 years
    populations <- c('A'=N, 'B'=0, 'C'=0, 'D'=0)
    m.comb$set_populations(populations)
    # run 2 cycles
    MT.comb <- m.comb$cycles(2, tcycle=tcycle, hcc=FALSE)
    # revise costs after 2 years
    #sA$set_cost(cA)
    #sB$set_cost(cB)
    #sC$set_cost(cC)
    # revert transition rates 
    #tAB$set_rate(trAB)
    #tAC$set_rate(trAC)
    #tAD$set_rate(trAD)
    #tBC$set_rate(trBC)
    #tBD$set_rate(trBD)
    #tCD$set_rate(trCD)
    # set populations in mono model
    populations <- m.comb$get_populations()
    m.mono$set_populations(populations, icycle=2)
    # run mono model for next 18 years
    MT.comb <- rbind(MT.comb, m.mono$cycles(ncycles=18, tcycle=tcycle,hcc=FALSE)) 
    print("")
    print(MT.comb)
    # expected life years and costs
    el.comb <- sum(MT.comb$QALY)
    cost.comb <- sum(MT.comb$Cost)
    #
    return(c("el.mono"=el.mono, "cost.mono"=cost.mono, 
             "el.comb"=el.comb, "cost.comb"=cost.comb))
  }
  #
  # Point estimate
  # ==============
  modvars <- m.mono$modvars()
  #print(modvars)
  sapply(modvars, FUN=function(mv){mv$set("expected")})
  M <- runmodel()
  expect_intol(M["el.mono"], 7.991, tolerance=0.03) # 7.991 from spreadsheet
  expect_intol(M["cost.mono"], 44663, 100) # rounding errors in book
  expect_intol(M["el.comb"], 8.937, tolerance=0.02) # 8.937 from spreadsheet
  expect_intol(M["cost.comb"], 50602, 100) # rounding errors in book
  icer <- (M["cost.comb"]-M["cost.mono"])/(M["el.comb"]-M["el.mono"])
  expect_intol(icer, 6276, 10) # rounding errors in book
  #
  # PSA
  # ===
  #n <- 10
  #DF <- sapply(1:n, FUN=function(i) {
  #  sapply(modvars, FUN=function(mv){mv$set("random")})
  #  M <- runmodel()
  #  return(M)
  #}) 
  #DF <- as.data.frame(t(DF))
  #print("")
  #print(DF)

  
  
  
})

