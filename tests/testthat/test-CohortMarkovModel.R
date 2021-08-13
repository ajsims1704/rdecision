
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

# ---------------------------------------------------------------------------
# tests of setting and getting rates
# ---------------------------------------------------------------------------
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

test_that("probabilities are calculated from rates correctly", {
  # create states
  sA <- MarkovState$new("A")
  sB <- MarkovState$new("B")
  sC <- MarkovState$new("C")
  sD <- MarkovState$new("D")
  V <- list(sA, sB, sC, sD)
  # create transitions
  E <- list(
    tAA <- MarkovTransition$new(sA, sA),
    tAB <- MarkovTransition$new(sA, sB, r=2),
    tBB <- MarkovTransition$new(sB, sB),
    tAC <- MarkovTransition$new(sA, sC, r=0.5),
    tCC <- MarkovTransition$new(sC, sC),
    tAD <- MarkovTransition$new(sA, sD, r=0.1),
    tDD <- MarkovTransition$new(sD, sD)
  )
  # create the model
  M <- CohortMarkovModel$new(V, E)
  # check the transition rate matrix properties
  Q <- M$transition_rate()
  expect_true(is.matrix(Q))
  expect_equal(nrow(Q),4)
  expect_equal(ncol(Q),4)
  dn <- dimnames(Q)
  expect_setequal(names(dn),list("source","target"))
  expect_setequal(dn[[1]],list("A","B","C", "D"))
  expect_setequal(dn[[2]],list("A","B","C", "D"))
  # check the transition rate matrix values
  EQ <- matrix(c(-2.6,2,0.5,0.1,0,0,0,0,0,0,0,0,0,0,0,0), nrow=4, byrow=TRUE)
  expect_true(all(abs(Q-EQ)<0.001))
  # test the per-year probability
  tcycle <- as.difftime(365.25, units="days")
  Pt <- M$transition_probability(tcycle)
  # check the transition probability matrix properties
  Pt <- M$transition_probability(as.difftime(365.25, units="days"))
  expect_true(is.matrix(Pt))
  expect_equal(nrow(Pt),4)
  expect_equal(ncol(Pt),4)
  dn <- dimnames(Pt)
  expect_setequal(names(dn),list("source","target"))
  expect_setequal(dn[[1]],list("A","B","C", "D"))
  expect_setequal(dn[[2]],list("A","B","C", "D"))
  # check the transition probability matrix values
  EPt <- matrix(
    c(0.074,0.712,0.178,0.036, 0,1,0,0, 0,0,1,0, 0,0,0,1), 
    nrow=4, byrow=TRUE
  )
  expect_true(all(round(Pt-EPt,3)<0.001))
  # test the per-month probability
  tcycle <- as.difftime(365.25/12, units="days")
  Pt <- M$transition_probability(tcycle)
  expect_equal(round(1-Pt[1,1],3),0.195)
})

test_that("rates are calculated from probabilities correctly", {
  # create the states
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  # create transitions, without rates
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.ss <- MarkovTransition$new(s.disabled, s.disabled)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  e.ws <- MarkovTransition$new(s.well, s.disabled)
  e.wd <- MarkovTransition$new(s.well, s.dead)
  e.sd <- MarkovTransition$new(s.disabled, s.dead)
  # create the model
  M <- CohortMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
  )
  # check that illegal arguments to set_rates are detected
  expect_error(M$set_rates(), class="invalid_Pt")
  EPt <- matrix(c(1,0,0,0),nrow=2,byrow=TRUE)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  EPt <- matrix(c(0.6,NA,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  EPt <- matrix(c(0.6,2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  EPt <- matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  state.names <- c("Well", "Disabled", "Dead")
  dimnames(EPt) <- list(source=state.names, target=NULL)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  dimnames(EPt) <- list(source=NULL, target=state.names)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  dimnames(EPt) <- list(source=state.names, target=state.names)
  expect_error(M$set_rates(EPt), class="invalid_tcycle")
  expect_error(M$set_rates(EPt,1), class="invalid_tcycle")
  # set and check the rates
  tcycle <- as.difftime(365.25, units="days")
  expect_silent(M$set_rates(EPt,tcycle))
  
  # Q <- expm::logm(EPt)/1
  # r.ws <- Q[1,2]
  # r.wd <- Q[1,3]
  # r.sd <- Q[2,3]
  # # check the transition matrix properties
  # Pt <- M$transition_probability(as.difftime(365.25, units="days"))
  # expect_equal(nrow(Pt),3)
  # expect_equal(ncol(Pt),3)
  # dn <- dimnames(Pt)
  # expect_setequal(names(dn),list("source","target"))
  # expect_setequal(dn[[1]],list("Well","Disabled","Dead"))
  # expect_setequal(dn[[2]],list("Well","Disabled","Dead"))
  # expect_true(is.matrix(Pt))
  # # check the transition matrix values
  # expect_true(all(abs(Pt-EPt)<0.010))
  
  expect_true(TRUE)
})

# -----------------------------------------------------------------------------
# tests of resetting the model
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
  expect_error(M$reset(pop), class="incorrect_state_count")
  # state names
  pop <- c(Well=10000, Poorly=0, Disabled=0)
  expect_error(M$reset(pop), class="unmatched_states")
  pop <- c(10000, 0, 0)
  expect_error(M$reset(pop), class="unmatched_states")
  # type
  pop <- c(Well=10000, Disabled="0", Dead=0)
  expect_error(M$reset(pop), class="non-numeric_state_population")
  # correct
  pop <- c(Well=10000, Disabled=0, Dead=0)
  expect_silent(M$reset(pop))
  rp <- M$get_populations()
  expect_equal(unname(rp["Well"]), 10000)
  expect_equal(unname(rp["Disabled"]), 0)
  expect_equal(unname(rp["Dead"]), 0)
})

test_that("invalid cycle numbers are rejected", {
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
  # attempt to reset with illegal cycle numbers
  expect_error(M$reset(icycle=2), class="invalid_icycle")
  expect_error(M$reset(icycle="2"), class="invalid_icycle")
  expect_error(M$reset(icycle=as.integer(-1)), class="invalid_icycle")
})

test_that("invalid elapsed times are rejected", {
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
  # attempt to reset with illegal elapsed times
  expect_error(M$reset(elapsed=2), class="invalid_elapsed")
  expect_error(M$reset(elapsed="2"), class="invalid_elapsed")
  expect_error(
    M$reset(icycle=as.difftime(-1, units="days")), 
    class="invalid_icycle"
  )
})

# -----------------------------------------------------------------------------
# tests of cycling
# -----------------------------------------------------------------------------
test_that("model is cyclable", {
  # create states
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new(name="Disabled")
  s.dead <- MarkovState$new(name="Dead")
  # use S&B per-cycle transition probabilities and calculate rates
  EPt <- matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  Q <- expm::logm(EPt)/1
  r.ws <- Q[1,2]
  r.wd <- Q[1,3]
  r.sd <- Q[2,3]
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

test_that("cycle time increments and resets correctly", {
  # create the model
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  # use S&B per-cycle transition probabilities to calculate rates
  EPt <- matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  Q <- expm::logm(EPt)/1
  r.ws <- Q[1,2]
  r.wd <- Q[1,3]
  r.sd <- Q[2,3]
  # create transitions  
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.ss <- MarkovTransition$new(s.disabled, s.disabled)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  e.ws <- MarkovTransition$new(s.well, s.disabled, r=r.ws)
  e.wd <- MarkovTransition$new(s.well, s.dead, r=r.wd)
  e.sd <- MarkovTransition$new(s.disabled, s.dead, r=r.sd)
  M <- CohortMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
  )
  # check that the elapsed time starts at zero
  t <- M$get_elapsed()
  expect_equal(as.numeric(t, units="days"), 0)
  # cycle for 2 years, including with hcc
  M$cycle()
  M$cycle(hcc.pop=FALSE,hcc.cost=FALSE)
  # check that the elapsed time is 2 years
  t <- M$get_elapsed()
  expect_equal(as.numeric(t, units="days"), 365.25*2)
  # run a further half year cycle
  M$cycle(tcycle=as.difftime(365.25/2, units="days"))
  # check that the elapsed time is 2.5 years
  t <- M$get_elapsed()
  expect_equal(as.numeric(t, units="days"), 365.25*2.5)
  # reset the time to 3 years and run a further cycle
  M$reset(elapsed=as.difftime(365.25*3, units="days"))
  M$cycle()
  # check that the elapsed time is 4 years
  t <- M$get_elapsed()
  expect_equal(as.numeric(t, units="days"), 365.25*4)
})

# ---------------------------------------------------------------------------
# tests of rates
# ---------------------------------------------------------------------------
test_that("results are independent of cycle time", {
  # create states
  s.well <- MarkovState$new(name="Well")
  s.dead <- MarkovState$new(name="Dead")
  # calculate rates from per-cycle probabilities
  Pt <- matrix(c(0.8,0.2,0,1),nrow=2,byrow=TRUE)
  Q <- expm::logm(Pt)/1
  r.wd <- Q[1,2]
  # create transitions
  E <- list(
    MarkovTransition$new(s.well, s.well),
    MarkovTransition$new(s.dead, s.dead),
    MarkovTransition$new(s.well, s.dead, r=r.wd)
  )
  # create the model and cycle for 5 years
  M <- CohortMarkovModel$new(V = list(s.well, s.dead), E)
  MT <- M$cycles(5, hcc.pop=FALSE, hcc.cost=FALSE)
  expect_equal(MT$Well[MT$Cycle==5], 327.68)
  # create the model and cycle for 5 years
  M <- CohortMarkovModel$new(V = list(s.well, s.dead), E)
  tcycle = as.difftime(365.25/12, units="days")
  TM <- M$transition_probability(tcycle=tcycle)
  MT <- M$cycles(
    5*12, 
    hcc.pop = FALSE, hcc.cost=FALSE, 
    tcycle = tcycle
  )
  expect_equal(MT$Well[MT$Cycle==60], 327.68)
})

test_that("rates can be proportioned correctly", {
  # create states
  s.A <- MarkovState$new(name="A")
  s.B <- MarkovState$new(name="B")
  # calculate rates from per-cycle probabilities
  Pt <- matrix(c(0.8,0.2,0,1),nrow=2,byrow=TRUE)
  Q <- expm::logm(Pt)/1
  r <- Q[1,2]
  # set the cycle time
  tcycle <- as.difftime(365.25, units="days")
  # create transitions
  E <- list(
    MarkovTransition$new(s.A, s.A),
    MarkovTransition$new(s.B, s.B),
    MarkovTransition$new(s.A, s.B, r=r)
  )
  # create the model and cycle for 5 years
  M <- CohortMarkovModel$new(V = list(s.A, s.B), E)
  MT <- M$cycles(5, hcc.pop=FALSE, hcc.cost=FALSE)
  expect_equal(MT$A[MT$Cycle==5], 327.68)
  expect_equal(MT$B[MT$Cycle==5], 1000-327.68)
  #
  # add extra absorbing state
  s.C <- MarkovState$new(name="C")
  # create transitions
  E <- list(
    MarkovTransition$new(s.A, s.A),
    MarkovTransition$new(s.B, s.B),
    MarkovTransition$new(s.C, s.C),
    MarkovTransition$new(s.A, s.B, r=r/2),
    MarkovTransition$new(s.A, s.C, r=r/2)
  )
  # create the model and cycle for 5 years
  M <- CohortMarkovModel$new(V = list(s.A, s.B, s.C), E)
  tm <- M$transition_probability(tcycle=tcycle)
  MT <- M$cycles(5, hcc.pop=FALSE, hcc.cost=FALSE)
  # expected proportions in each state
  expect_equal(MT$A[MT$Cycle==5], 327.68)
  expect_equal(MT$B[MT$Cycle==5], (1000-327.68)/2)
  expect_equal(MT$C[MT$Cycle==5], (1000-327.68)/2)
})

test_that("people waiting for a test are modelled correctly", {
  # population and test performance
  p.prev <- 0.4
  p.sens <- 0.75
  p.spec <- 0.75
  # create states
  s.A <- MarkovState$new(name="A") # people waiting for a test
  s.TP <- MarkovState$new(name="TP") # TP, absorbing
  s.TN <- MarkovState$new(name="TN") # TN, absorbing
  s.FP <- MarkovState$new(name="FP") # FP, absorbing
  s.FN <- MarkovState$new(name="FN") # FN, absorbing
  # calculate rates from per-cycle probabilities
  Pt <- matrix(c(0.8,0.2,0,1),nrow=2,byrow=TRUE)
  Q <- expm::logm(Pt)/1
  r <- Q[1,2]
  # test outcome proportions and rates
  pTP <- p.prev*p.sens
  pTN <- (1-p.prev)*p.spec
  pFP <- (1-p.prev)*(1-p.spec)
  pFN <- p.prev*(1-p.sens)
  rTP <- r*pTP
  rTN <- r*pTN
  rFP <- r*pFP
  rFN <- r*pFN
  #
  # create transitions
  E <- list(
    MarkovTransition$new(s.A, s.A, r=NULL),
    MarkovTransition$new(s.A, s.TP, r=rTP),
    MarkovTransition$new(s.TP, s.TP, r=NULL),
    MarkovTransition$new(s.A, s.TN, r=rTN),
    MarkovTransition$new(s.TN, s.TN, r=NULL),
    MarkovTransition$new(s.A, s.FP, r=rFP),
    MarkovTransition$new(s.FP, s.FP, r=NULL),
    MarkovTransition$new(s.A, s.FN, r=rFN),
    MarkovTransition$new(s.FN, s.FN, r=NULL)
  )
  # create the model and cycle for 5 years in months
  M <- CohortMarkovModel$new(V = list(s.A, s.TP, s.TN, s.FP, s.FN), E)
  tcycle <- as.difftime(365.25/12, units="days")
  MT <- M$cycles(5*12, tcycle=tcycle, hcc.pop=FALSE, hcc.cost=FALSE)
  # expected proportions in each state
  e.untested <- 327.68
  e.tested <- 1000-e.untested
  expect_equal(MT$A[MT$Cycle==5*12], e.untested)
  expect_equal(MT$TP[MT$Cycle==5*12], e.tested*pTP)
  expect_equal(MT$TN[MT$Cycle==5*12], e.tested*pTN)
  expect_equal(MT$FP[MT$Cycle==5*12], e.tested*pFP)
  expect_equal(MT$FN[MT$Cycle==5*12], e.tested*pFN)
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
  # transition probabilities and rates (complexity needed because S&B specify 
  # probabilities, not rates)
  #p.ws <- 0.2
  #p.wd <- 0.2
  #p.sd <- 0.4
  #r.ws <- (-log(1-(p.ws+p.wd))/1)*(p.ws/(p.ws+p.wd))
  #print(r.ws)
  #r.wd <- (-log(1-(p.ws+p.wd))/1)*(p.wd/(p.ws+p.wd))
  #print(r.wd)
  #r.sd <- -log(1-p.sd)/1
  #print(r.sd)
  ##
  Pt <- matrix(c(0.6, 0.2, 0.2, 0, 0.6, 0.4, 0, 0, 1), nrow=3, byrow=TRUE)
  Q <- expm::logm(Pt)/1
#  print(Q)
  r.ws <- Q[1,2]
  r.wd <- Q[1,3]
  r.sd <- Q[2,3]
  # create transitions
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
#  print(Ip)
  expect_equal(
    sum(Ip-matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)),0
  )
  # set the starting populations
  M$reset(c(Well=10000, Disabled=0, Dead=0)) 
  # cycle
  RC <- M$cycles(25, tcycle=tcycle, hcc.pop=FALSE, hcc.cost=FALSE)
  expect_true(is.data.frame(RC))
  expect_equal(round(RC$Well[RC$Cycle==2]), 3600)
  expect_equal(round(RC$Disabled[RC$Cycle==2]), 2400)
  expect_equal(round(RC$Dead[RC$Cycle==2]), 4000)
  expect_true(TRUE)
})

# ---------------------------------------------------------------------------
# Chancellor, 1997 (HIV) with PSA and Briggs exercises 2.5 and 4.7 
# ---------------------------------------------------------------------------
test_that("redecision replicates Briggs' example 4.7", {
  #
  # Discount rates
  # ==============
  cDR <- 6 # annual discount rate, costs (%)
  oDR <- 0 # annual discount rate, benefits (%)
  #
  # Cycle time
  # ==========
  tcycle = as.difftime(365.24, units="days")
  #
  # Transition rates
  # ================
  # Note the complexity with conditional probabilities being used here to derive
  # transition rates arises because Briggs et al used probabilities, rather
  # than rates, as input. 
  #
  # Dirichlet distribution & model variables for transitions from A
  nA <- c(1251, 350, 116, 17)       # transitions, people, per year
  DA <- DirichletDistribution$new(nA)
  pAB <- ModVar$new("pAB", "P", D=DA, k=as.integer(2))
  pAC <- ModVar$new("pAC", "P", D=DA, k=as.integer(3))
  pAD <- ModVar$new("pAD", "P", D=DA, k=as.integer(4))
  pA <- ExprModVar$new("pA", "P", rlang::quo(pAB+pAC+pAD))
  rA <- ExprModVar$new("rA", "HR", rlang::quo(-log(1-pA)))
  # Dirichlet distribution & model variables for transitions from B
  nB <- c(731,512,15)               # transitions, people per year
  DB <- DirichletDistribution$new(nB)
  pBC <- ModVar$new("pBC", "P", D=DB, k=as.integer(2)) 
  pBD <- ModVar$new("pBD", "P", D=DB, k=as.integer(3)) 
  pB <- ExprModVar$new("pB", "P", rlang::quo(pBC+pBD))
  rB <- ExprModVar$new("rB", "HR", rlang::quo(-log(1-pB)))
  # Dirichlet distribution & model variables for transitions from C
  nC <- c(1312,437)                 # transitions, people per year
  DC <- DirichletDistribution$new(nC)
  pCD <- ModVar$new("pCD", "P", D=DC, k=as.integer(2)) 
  pC <- ExprModVar$new("pC", "P", rlang::quo(pCD))
  rC <- ExprModVar$new("rC", "HR", rlang::quo(-log(1-pC)))
  # transition rates with monotherapy from annual transition probabilities
  trABm <- ExprModVar$new("trAB", "HR", rlang::quo(rA*pAB/pA))
  trACm <- ExprModVar$new("trAC", "HR", rlang::quo(rA*pAC/pA))
  trADm <- ExprModVar$new("trAD", "HR", rlang::quo(rA*pAD/pA))
  trBCm <- ExprModVar$new("trBC", "HR", rlang::quo(rB*pBC/pB))
  trBDm <- ExprModVar$new("trBD", "HR", rlang::quo(rB*pBD/pB))
  trCDm <- ExprModVar$new("trCD", "HR", rlang::quo(rC*pCD/pC))
  # Treatment effect (modelled as a log normal distribution)
  RR <- LogNormModVar$new(
    "Tx effect", "RR", p1=0.509, p2=(0.710-0.365)/(2*1.96), "LN7"
  )
  # transition rates, with treatment effect
  rAc <- ExprModVar$new("rAc", "HR", rlang::quo(-log(1-pA*RR)))
  rBc <- ExprModVar$new("rBc", "HR", rlang::quo(-log(1-pB*RR)))
  rCc <- ExprModVar$new("rCc", "HR", rlang::quo(-log(1-pC*RR)))
  trABc <- ExprModVar$new("trAB", "HR", rlang::quo(rAc*pAB/pA))
  trACc <- ExprModVar$new("trAC", "HR", rlang::quo(rAc*pAC/pA))
  trADc <- ExprModVar$new("trAD", "HR", rlang::quo(rAc*pAD/pA))
  trBCc <- ExprModVar$new("trBC", "HR", rlang::quo(rBc*pBC/pB))
  trBDc <- ExprModVar$new("trBD", "HR", rlang::quo(rBc*pBD/pB))
  trCDc <- ExprModVar$new("trCD", "HR", rlang::quo(rCc*pCD/pC))
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
  TM <- m.mono$transition_probability(tcycle)
  E <- matrix(
    c(0.721, 0.202, 0.067, 0.010,  
      0.000, 0.581, 0.407, 0.012,
      0.000, 0.000, 0.750, 0.250,
      0.000, 0.000, 0.000, 1.000),   # typo in book (D,D) = 1!
    byrow = TRUE,
    nrow = 4)
  expect_true(all(TM-E < 0.01))
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
  # check transition matrix
  TM <- m.comb$transition_probability(tcycle)
  E <- matrix(
    c(0.858, 0.103, 0.034, 0.005,  
      0.000, 0.787, 0.207, 0.006,
      0.000, 0.000, 0.873, 0.127,
      0.000, 0.000, 0.000, 1.000),   
    byrow = TRUE,
    nrow = 4)
  expect_true(all(TM-E < 0.01))
  #
  # Function to estimate life years gained and costs
  # ================================================
  runmodel <- function(hcc.pop=FALSE, hcc.cost=FALSE) {
    # 
    # Monotherapy
    # ===========
    # create starting populations
    N <- 1000
    populations <- c(A = N, B = 0, C = 0, D = 0)
    m.mono$reset(populations)
    # run 20 cycles
    MT.mono <- m.mono$cycles(
      ncycles=20, 
      tcycle=tcycle,
      hcc.pop = hcc.pop,
      hcc.cost = hcc.cost
    )
    # expected life years and costs
    el.mono <- sum(MT.mono$QALY)
    cost.mono <- sum(MT.mono$Cost)
    #
    # Combination therapy
    # ===================
    # run combination therapy model for 2 years
    populations <- c('A'=N, 'B'=0, 'C'=0, 'D'=0)
    m.comb$reset(populations)
    # run 2 cycles
    MT.comb <- m.comb$cycles(
      2, 
      tcycle = tcycle, 
      hcc.pop = hcc.pop,
      hcc.cost = hcc.cost
    )
    # set populations in mono model & reset cycle counter and time
    populations <- m.comb$get_populations()
    m.mono$reset(
      populations, 
      icycle=as.integer(2), 
      elapsed=as.difftime(365.25*2, units="days")
    )
    # run mono model for next 18 years
    MT.comb <- rbind(
      MT.comb, 
      m.mono$cycles(
        ncycles=18, 
        tcycle=tcycle,
        hcc.pop = hcc.pop,
        hcc.cost = hcc.cost
      )
    ) 
    # expected life years and costs
    el.comb <- sum(MT.comb$QALY)
    cost.comb <- sum(MT.comb$Cost)
    #
    return(c("el.mono"=el.mono, "cost.mono"=cost.mono, 
             "el.comb"=el.comb, "cost.comb"=cost.comb))
  }
  #
  # Point estimate (without half-cycle correction)
  # ==============================================
  # set all modvars to their expected values in both models
  modvars <- m.mono$modvars()
  sapply(modvars, FUN=function(mv){mv$set("expected")})
  modvars <- m.comb$modvars()
  sapply(modvars, FUN=function(mv){mv$set("expected")})
  # run the model
  M <- runmodel(hcc.pop=FALSE, hcc.cost=FALSE)
  # check results
  expect_intol(M["el.mono"], 7.991, tolerance=0.03) # 7.991 from spreadsheet
  expect_intol(M["cost.mono"], 44663, 100) # rounding errors in book
  expect_intol(M["el.comb"], 8.937, tolerance=0.02) # 8.937 from spreadsheet
  expect_intol(M["cost.comb"], 50602, 100) # rounding errors in book
  icer <- (M["cost.comb"]-M["cost.mono"])/(M["el.comb"]-M["el.mono"])
  expect_intol(icer, 6276, 10) # rounding errors in book
  #
  # Point estimate (with population half-cycle correction)
  # ======================================================
  # set all modvars to their expected values in both models
  modvars <- m.mono$modvars()
  sapply(modvars, FUN=function(mv){mv$set("expected")})
  modvars <- m.comb$modvars()
  sapply(modvars, FUN=function(mv){mv$set("expected")})
  # run the model
  M <- runmodel(hcc.pop=TRUE, hcc.cost=FALSE)
  # check results
  expect_intol(M["el.mono"], 8.48, tolerance=0.03) 
  expect_intol(M["cost.mono"], 44663, 100) # rounding errors in book
  expect_intol(M["el.comb"], 9.42, tolerance=0.02) 
  expect_intol(M["cost.comb"], 50602, 100) # rounding errors in book
  icer <- (M["cost.comb"]-M["cost.mono"])/(M["el.comb"]-M["el.mono"])
  expect_intol(icer, 6306, 10) # rounding errors in book
  #
  # PSA
  # ===
  skip_on_cran()
  n <- 100
  DF <- sapply(1:n, FUN=function(i) {
    # set all modvars to random values in both models
    modvars <- m.mono$modvars()
    sapply(modvars, FUN=function(mv){mv$set("random")})
    modvars <- m.comb$modvars()
    sapply(modvars, FUN=function(mv){mv$set("random")})
    # run the model
    M <- runmodel(hcc.pop=TRUE, hcc.cost=FALSE)
    return(M)
  })
  DF <- as.data.frame(t(DF))
  # calculate ICER and compare with 100 ICER samples from Briggs spreadsheet, 
  # example 4.7
  DF$icer <- (DF$cost.comb - DF$cost.mono)/(DF$el.comb - DF$el.mono)
  esamp <- scan(text=
    "5301.35
    9440.15
    5744.259409
    6389.097064
    6392.624986
    3994.719593
    7172.362209
    7911.346126
    4532.492365
    5624.510108
    7661.375273
    6994.758314
    6443.136695
    8415.490257
    5867.107527
    5233.06391
    6622.541882
    6437.612983
    2911.766414
    8913.801308
    6520.597363
    5578.340444
    4729.782693
    9504.301977
    6146.588061
    3765.76602  
    6489.778374
    8057.35546
    11811.25017
    5670.353086
    3849.923114
    4950.39429
    4122.370475  
    3821.724085
    8336.515228
    4198.215337
    5253.411175
    8011.172733
    5638.448818
    7453.423065
    6658.135907
    4499.134333
    5201.390732
    7122.874494
    4876.858193 
    4023.805442
    5129.919089
    4249.610465
    12462.7631
    7434.229976
    7200.266514
    7952.9198
    6556.010328
    3827.481732
    5443.137027
    6012.291712
    7139.430982
    6533.022468
    6739.577098
    6430.554141
    5167.152795  
    6904.27616
    5573.823787
    8202.449279
    6268.396547
    4602.761381
    2471.876679
    5973.696879
    7414.062825
    6541.740092
    1793.977058
    5224.207019
    3769.477755
    3660.149219
    16972.62138
    6229.749342
    2105.755358
    6150.607183
    6088.493293
    6090.582267
    4299.749596
    6894.101671
    8711.266189
    10316.17458
    7869.832914
    4949.493389
    4319.858777
    5084.068203
    7047.4081
    3261.818576
    7305.940637
    10060.60441
    5851.727982
    8271.551092
    5398.198376
    7150.231396
    8151.732244
    3949.116594
    7574.593941
    4027.405958",
    quiet = TRUE
  ) 
  ht <- ks.test(DF$icer, esamp)
  expect_true(ht$p.value > 0.001)
  
})


