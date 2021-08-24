
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
  e.wd.bleed <- MarkovTransition$new(s.well, s.dead)
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

test_that("non-unique state labels are detected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Well")
  s.dead <- MarkovState$new("Dead")
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.wd <- MarkovTransition$new(s.well, s.dead)
  e.ws <- MarkovTransition$new(s.well, s.disabled)
  e.sd <- MarkovTransition$new(s.disabled, s.dead)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  expect_error(
    CohortMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.wd, e.ws, e.sd, e.dd)
    ), 
    class="invalid_state_names"
  )  
})  

test_that("invalid discount rates are detected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.wd <- MarkovTransition$new(s.well, s.dead)
  e.ws <- MarkovTransition$new(s.well, s.disabled)
  e.sd <- MarkovTransition$new(s.disabled, s.dead)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  expect_error(
    CohortMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.wd, e.ws, e.sd, e.dd),
      discount.cost = "0"
    ), 
    class="invalid_discount"
  )  
  expect_error(
    CohortMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.wd, e.ws, e.sd, e.dd),
      discount.utility = "0"
    ), 
    class="invalid_discount"
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
  # under-specify (no rates specified for outgoing 'disabled' state)
  e.ws$set_rate(r=0.2)
  e.wd$set_rate(r=0.2)
  expect_error(
    MC$transition_probability(tcycle),
    class = "invalid_rate"
  )
  # non-zero rate sum (for 'dead' state)
  e.sd$set_rate(r=0.4)
  e.dd$set_rate(r=1)
  expect_error(
    MC$transition_probability(tcycle),
    class="invalid_rate"
  )
  # correctly specified (d->d has one state as NA)
  e.dd$set_rate(r=as.numeric(NA))
  e.sd$set_rate(r=0.4)
  expect_silent(
    MC$transition_probability(tcycle)
  )
  # correctly specified (d -> d is 0)
  e.sd$set_rate(r=0.4)
  e.dd$set_rate(r=0)
  expect_silent(
    MC$transition_probability(tcycle)
  )
  # correctly specified (d -> d is 0 and sum from s is zero)
  e.sd$set_rate(r=0.4)
  e.ss$set_rate(r=-0.4)
  e.dd$set_rate(r=0)
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
  # check that illegal cycle times are rejected
  expect_error(M$transition_probability(42))
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
  # save state names for convenience
  state.names <- c("Well", "Disabled", "Dead")
  # no Pt
  expect_error(M$set_rates(), class="invalid_Pt")
  # non matrix
  expect_error(M$set_rates(42), class="invalid_Pt")
  # Pt wrong size
  EPt <- matrix(c(1,0,0,0),nrow=2,byrow=TRUE)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  # Pt with missing value
  EPt <- matrix(c(0.6,NA,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  dimnames(EPt) <- list(source=state.names, target=state.names)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  # Pt has element > 1
  EPt <- matrix(c(0.6,2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  dimnames(EPt) <- list(source=state.names, target=state.names)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  # Pt has row sums exceeding 1
  EPt <- matrix(c(0.6,0.2,0.3,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  dimnames(EPt) <- list(source=state.names, target=state.names)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  # Pt has non-zero probabilities for disallowed transitions
  EPt <- matrix(c(0.6,0.2,0.2,0.2,0.4,0.4,0,0,1),nrow=3,byrow=TRUE)
  dimnames(EPt) <- list(source=state.names, target=state.names)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  # Pt has no labels
  EPt <- matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  # Pt has partly missing labels
  dimnames(EPt) <- list(source=state.names, target=NULL)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  dimnames(EPt) <- list(source=NULL, target=state.names)
  expect_error(M$set_rates(EPt), class="invalid_Pt")
  # check that illegal cycle times are detected
  EPt <- matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  dimnames(EPt) <- list(source=state.names, target=state.names)
  expect_error(M$set_rates(EPt), class="invalid_tcycle")
  expect_error(M$set_rates(EPt,1), class="invalid_tcycle")
  # set and check the rates
  EPt <- matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)
  dimnames(EPt) <- list(source=state.names, target=state.names)
  tcycle <- as.difftime(365.25, units="days")
  expect_silent(M$set_rates(EPt,tcycle))
  M$set_rates(EPt,tcycle)
  # check the transition matrix properties
  Pt <- M$transition_probability(as.difftime(365.25, units="days"))
  expect_equal(nrow(Pt),3)
  expect_equal(ncol(Pt),3)
  dn <- dimnames(Pt)
  expect_setequal(names(dn),list("source","target"))
  expect_setequal(dn[[1]],list("Well","Disabled","Dead"))
  expect_setequal(dn[[2]],list("Well","Disabled","Dead"))
  expect_true(is.matrix(Pt))
  # check the transition matrix values
  expect_true(all(abs(Pt-EPt)<0.010))
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
  snames <- c("Well","Disabled","Dead")
  Pt <- matrix(
    data = c(0.6, 0.2, 0.2, 0, 0.6, 0.4, 0, 0, 1),
    nrow = 3, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  # create transitions
  E <- list(
    MarkovTransition$new(s.well, s.well),
    MarkovTransition$new(s.dead, s.dead),
    MarkovTransition$new(s.disabled, s.disabled),
    MarkovTransition$new(s.well, s.disabled),
    MarkovTransition$new(s.well, s.dead),
    MarkovTransition$new(s.disabled, s.dead)
  )
  # create the model
  M <- CohortMarkovModel$new(V = list(s.well, s.disabled, s.dead), E)
  # set rates
  tcycle <- as.difftime(365.25, units="days")
  M$set_rates(Pt, tcycle)
  # test cycles
  DF <- M$cycle()
  expect_true(is.data.frame(DF))
  expect_setequal(
    names(DF), 
    c("State", "Cycle", "Time", "Population", "EntryCost", "OccCost", "Cost", 
      "QALY")
  )
  expect_equal(nrow(DF),3)
  # detect illegal parameters to cycle()
  expect_error(M$cycle(tcycle=42), class="invalid_cycle_length")
  expect_error(M$cycle(hcc.pop=3), class="invalid_hcc")
  expect_error(M$cycle(hcc.cost=3), class="invalid_hcc")
  expect_error(M$cycle(hcc.pop=FALSE,hcc.cost=TRUE), class="invalid_hcc")
  
})

test_that("cycle time increments and resets correctly", {
  # create the model
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  # use S&B per-cycle transition probabilities to calculate rates
  snames <- c("Well","Disabled","Dead")
  Pt <- matrix(
    data = c(0.6, 0.2, 0.2, 0, 0.6, 0.4, 0, 0, 1),
    nrow = 3, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  # create transitions  
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
  # set rates
  tcycle <- as.difftime(365.25, units="days")
  M$set_rates(Pt, tcycle)
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
  # set annual per-cycle probabilities
  Pt <- matrix(
    c(0.8,0.2,0,1), nrow=2, byrow=TRUE, 
    dimnames=list(source=c("Well", "Dead"), target=c("Well", "Dead"))
  )
  # create transitions
  E <- list(
    MarkovTransition$new(s.well, s.well),
    MarkovTransition$new(s.dead, s.dead),
    MarkovTransition$new(s.well, s.dead)
  )
  # create the model
  M <- CohortMarkovModel$new(V = list(s.well, s.dead), E)
  # set rates
  M$set_rates(Pt, tcycle=as.difftime(365.25, units="days"))
  # cycle for 5 years
  MT <- M$cycles(5, hcc.pop=FALSE, hcc.cost=FALSE)
  expect_equal(MT$Well[MT$Cycle==5], 327.68)
  # reset the population
  M$reset()
  # cycle for 5 years in months
  tcycle = as.difftime(365.25/12, units="days")
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
  # set annual per-cycle probabilities
  Pt <- matrix(
    c(0.8,0.2,0,1), nrow=2, byrow=TRUE, 
    dimnames=list(source=c("A", "B"), target=c("A", "B"))
  )
  # set the cycle time
  tcycle <- as.difftime(365.25, units="days")
  # create transitions with no rates set initially
  E <- list(
    MarkovTransition$new(s.A, s.A),
    MarkovTransition$new(s.B, s.B),
    MarkovTransition$new(s.A, s.B)
  )
  # create the model
  M <- CohortMarkovModel$new(V = list(s.A, s.B), E)
  # set rates from probabilities
  M$set_rates(Pt, tcycle=as.difftime(365.25, units="days"))
  # get the transition rate from A->B
  Q <- M$transition_rate()
  r <- Q["A","B"]
  # cycle for 5 years
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
  # proportions of outcomes
  pTP <- p.prev*p.sens
  pTN <- (1-p.prev)*p.spec
  pFP <- (1-p.prev)*(1-p.spec)
  pFN <- p.prev*(1-p.sens)
  # create states
  s.A <- MarkovState$new(name="A") # people waiting for a test
  s.TP <- MarkovState$new(name="TP") # TP, absorbing
  s.TN <- MarkovState$new(name="TN") # TN, absorbing
  s.FP <- MarkovState$new(name="FP") # FP, absorbing
  s.FN <- MarkovState$new(name="FN") # FN, absorbing
  # set the rate of testing based on a waiting time of 3 months
  twait <- as.difftime(365.25/4, units="days")
  r <- 1/(as.numeric(twait, units="days")/365.25)
  # set the rates for each outcome
  rTP <- r*pTP
  rTN <- r*pTN
  rFP <- r*pFP
  rFN <- r*pFN
  # create transitions
  E <- list(
    MarkovTransition$new(s.A, s.A),
    MarkovTransition$new(s.A, s.TP, r=rTP),
    MarkovTransition$new(s.TP, s.TP),
    MarkovTransition$new(s.A, s.TN, r=rTN),
    MarkovTransition$new(s.TN, s.TN),
    MarkovTransition$new(s.A, s.FP, r=rFP),
    MarkovTransition$new(s.FP, s.FP),
    MarkovTransition$new(s.A, s.FN, r=rFN),
    MarkovTransition$new(s.FN, s.FN)
  )
  # create the model and cycle for 6 months, in months
  M <- CohortMarkovModel$new(V = list(s.A, s.TP, s.TN, s.FP, s.FN), E)
  tcycle <- as.difftime(365.25/12, units="days")
  MT <- M$cycles(6, tcycle=tcycle, hcc.pop=FALSE, hcc.cost=FALSE)
  # expected proportions in each state
  e.tested <- 1000*(1-exp(-r*6/12))
  e.untested <- 1000-e.tested
  expect_equal(MT$A[MT$Cycle==6], e.untested)
  expect_equal(MT$TP[MT$Cycle==6], e.tested*pTP)
  expect_equal(MT$TN[MT$Cycle==6], e.tested*pTN)
  expect_equal(MT$FP[MT$Cycle==6], e.tested*pFP)
  expect_equal(MT$FN[MT$Cycle==6], e.tested*pFN)
})

test_that("Welton and Ades (2005) 3-state model is replicated", {
  # model (fig 3)
  s1 <- MarkovState$new("State 1")
  s2 <- MarkovState$new("State 2")
  s3 <- MarkovState$new("State 3")
  # transition rates (events per person year, table 1)
  g12 <- 3/51
  g13 <- 7/51
  g23 <- 5/14
  # transitions
  t11 <- MarkovTransition$new(s1,s1)
  t12 <- MarkovTransition$new(s1,s2,r=g12)
  t13 <- MarkovTransition$new(s1,s3,r=g13)
  t22 <- MarkovTransition$new(s2,s2)
  t23 <- MarkovTransition$new(s2,s3,r=g23)
  # model
  M <- CohortMarkovModel$new(V=list(s1,s2,s3),E=list(t11,t12,t13,t22,t23))
  # check Q
  Q <- M$transition_rate()
  EQ <- matrix(c(-(g12+g13),g12,g13,0,-g23,g23,0,0,0),nrow=3,byrow=TRUE)
  expect_true(all(Q-EQ<sqrt(.Machine$double.eps)))
  # check Pt
  t <- 1
  p11 <- exp(-(g12+g13)*t)
  p12 <-g12*exp(-g23*t)*(1-exp(-(g12+g13-g23)*t))/(g12+g13-g23)
  p13 <- 1-p11-p12
  p22 <- exp(-g23*t)
  p23 <- 1-exp(-g23*t)
  EPt <- matrix(c(p11,p12,p13,0,p22,p23,0,0,1), nrow=3, byrow=TRUE)
  Pt <- M$transition_probability(tcycle=as.difftime(365.25,units="days"))
  expect_true(all(Pt-EPt<sqrt(.Machine$double.eps)))
})

# -----------------------------------------------------------------------------
# tests of model variables
# -----------------------------------------------------------------------------
test_that("the rate uncertainty method of Welton & Ades (2005) is supported", {
  # Exposure, E, and events, r (Table 1)
  E1 <- 51
  E2 <- 14
  r12 <- 3
  r13 <- 7
  r23 <- 5
  # cost and utility
  c2 <- GammaModVar$new("C2", "GBP", shape=1000, scale=1/10)
  u2 <- BetaModVar$new("U2", "U", alpha=7, beta=3)
  # model (fig 3)
  s1 <- MarkovState$new("State 1")
  s2 <- MarkovState$new("State 2", cost=c2, utility=u2)
  s3 <- MarkovState$new("State 3")
  # model variables for rates and conditional probabilities
  lambda1 <- GammaModVar$new("lambda1", "rate", shape=(r12+r13), scale=1/E1)
  lambda2 <- GammaModVar$new("lambda2", "rate", shape=r23, scale=1/E2)
  D1 <- DirichletDistribution$new(alpha=c(r12, r13))
  rho12 <- ModVar$new("rho12", "p", D=D1, k=as.integer(1))
  rho13 <- ModVar$new("rho13", "p", D=D1, k=as.integer(2))
  g12 <- ExprModVar$new("g12", "rate", rlang::quo(lambda1*rho12))
  g13 <- ExprModVar$new("g13", "rate", rlang::quo(lambda1*rho13))
  g23 <- ExprModVar$new("g23", "rate", rlang::quo(lambda2))
  # transitions
  t11 <- MarkovTransition$new(s1,s1)#,r=g11)
  t12 <- MarkovTransition$new(s1,s2,r=g12)
  t13 <- MarkovTransition$new(s1,s3,r=g13)
  t22 <- MarkovTransition$new(s2,s2)#,r=g22)
  t23 <- MarkovTransition$new(s2,s3,r=g23)
  # model
  M <- CohortMarkovModel$new(V=list(s1,s2,s3),E=list(t11,t12,t13,t22,t23))
  # check modvar count
  mv <- M$modvars()
  expect_equal(length(mv),7+2)
  # tabulate the input variables
  MVT <- M$modvar_table(expressions=FALSE)
  expect_equal(nrow(MVT), 4+2)
  # spot check values
  expect_intol(lambda1$mean(), (r12+r13)/E1, tol=0.0001)
  expect_equal(rho12$mean(), r12/(r12+r13))
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
  # create transitions leaving rates undefined
  E <- list(
    MarkovTransition$new(s.well, s.well),
    MarkovTransition$new(s.dead, s.dead),
    MarkovTransition$new(s.disabled, s.disabled),
    MarkovTransition$new(s.well, s.disabled),
    MarkovTransition$new(s.well, s.dead),
    MarkovTransition$new(s.disabled, s.dead)
  )
  # create the model
  M <- CohortMarkovModel$new(V = list(s.well, s.disabled, s.dead), E)
  # check the state tabulation
  ST <- M$tabulate_states()
  expect_setequal(names(ST), c("Name", "Cost", "Utility"))
  expect_equal(nrow(ST),3)
  # create transition probability matrix
  snames <- c("Well","Disabled","Dead")
  EPt <- matrix(
    data = c(0.6, 0.2, 0.2, 0, 0.6, 0.4, 0, 0, 1),
    nrow = 3, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  # set and check the transition rates
  M$set_rates(EPt, tcycle)
  Pt <- M$transition_probability(tcycle)
  expect_true(all(EPt-Pt < sqrt(.Machine$double.eps)))
  # set the starting populations
  M$reset(c(Well=10000, Disabled=0, Dead=0)) 
  # cycle
  RC <- M$cycles(25, tcycle=tcycle, hcc.pop=FALSE, hcc.cost=FALSE)
  expect_true(is.data.frame(RC))
  expect_equal(round(RC$Well[RC$Cycle==2]), 3600)
  expect_equal(round(RC$Disabled[RC$Cycle==2]), 2400)
  expect_equal(round(RC$Dead[RC$Cycle==2]), 4000)
})

# ---------------------------------------------------------------------------
# Chancellor, 1997 (HIV) with PSA and Briggs exercises 2.5 and 4.7 
# ** This is a bad example because Briggs et al have converted a table of
# ** observed transitions (Table 2.5, p38) into probabilities. They should
# ** have followed the method of Welton and Ades (2005) which describes how to
# ** convert fully observed transitions into rates. It has the consequence of
# ** an unusual usage of the Dirichlet distributions, which should have been
# ** used only for the conditionals. For future releases, consider leaving
# ** the validation case as a point estimate only and choosing a different
# ** validation example for PSA.
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
  # Markov model
  # ============
  # states (leave all costs as zero initially)
  sA <- MarkovState$new("A")
  sB <- MarkovState$new("B")
  sC <- MarkovState$new("C")
  sD <- MarkovState$new("D", cost=0, utility=0)
  # transitions (leave all rates as NA initially)
  tAA <- MarkovTransition$new(sA, sA)
  tAB <- MarkovTransition$new(sA, sB)
  tAC <- MarkovTransition$new(sA, sC)
  tAD <- MarkovTransition$new(sA, sD)
  tBB <- MarkovTransition$new(sB, sB)
  tBC <- MarkovTransition$new(sB, sC)
  tBD <- MarkovTransition$new(sB, sD)
  tCC <- MarkovTransition$new(sC, sC)
  tCD <- MarkovTransition$new(sC, sD)
  tDD <- MarkovTransition$new(sD, sD)
  # model
  m <- CohortMarkovModel$new(
    V = list(sA, sB, sC, sD),
    E = list(tAA, tAB, tAC, tAD, tBB, tBC, tBD, tCC, tCD, tDD),
    discount.cost = cDR/100,
    discount.utility = oDR/100
  )
  #
  # Treatment effect (modelled as a log normal distribution)
  # ========================================================
  RR <- LogNormModVar$new(
    "Tx effect", "RR", p1=0.509, p2=(0.710-0.365)/(2*1.96), "LN7"
  )
  #
  # Dirichlet distributions for conditional probabilities
  # =====================================================
  DA <- DirichletDistribution$new(c(1251, 350, 116, 17)) # from A
  DB <- DirichletDistribution$new(c(731,512,15))  # from B
  DC <- DirichletDistribution$new(c(1312,437)) # from C
  
  #
  # Function to estimate life years gained and costs
  # ================================================
  runmodel <- function(expected=TRUE, hcc.pop=FALSE, hcc.cost=FALSE) {
    # set variables
    modvars <- m$modvars()
    if (expected) {
      sapply(modvars, FUN=function(mv){mv$set("expected")})
      RR$set("expected")
      Pt <- matrix(
        c(DA$mean(), c(0, DB$mean()), c(0, 0, DC$mean()), c(0, 0, 0, 1)), 
        byrow = TRUE,
        nrow = 4,
        dimnames = list(source=c("A","B","C","D"), target=c("A","B","C","D"))
      )
    } else {
      sapply(modvars, FUN=function(mv){mv$set("random")})
      RR$set("random")
      Pt <- matrix(
        c(DA$r(), c(0, DB$r()), c(0, 0, DC$r()), c(0, 0, 0, 1)), 
        byrow = TRUE,
        nrow = 4,
        dimnames = list(source=c("A","B","C","D"), target=c("A","B","C","D"))
      )
    }
    # 
    # Monotherapy
    # ===========
    # set costs
    sA$set_cost(cAm)
    sB$set_cost(cBm)
    sC$set_cost(cCm)
    sD$set_cost(0)
    # set transition rates from probabilities
    m$set_rates(Pt, tcycle)
    # check them
    TM <- m$transition_probability(tcycle)
    if (expected) {
      E <- matrix(
        c(0.721, 0.202, 0.067, 0.010,  
          0.000, 0.581, 0.407, 0.012,
          0.000, 0.000, 0.750, 0.250,
          0.000, 0.000, 0.000, 1.000),   # typo in book (D,D) = 1!
        byrow = TRUE,
        nrow = 4
      )
      expect_true(all(TM-E < 0.01))
    }
    # create starting populations
    N <- 1000
    populations <- c(A = N, B = 0, C = 0, D = 0)
    m$reset(populations)
    # run 20 cycles
    MT.mono <- m$cycles(
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
    # set costs
    sA$set_cost(cAc)
    sB$set_cost(cBc)
    sC$set_cost(cCc)
    sD$set_cost(0)
    # create Pt for combination therapy (Briggs applied the RR to the transition 
    # probabilities - not recommended, but done here for reproducibility).
    Ptc <- Pt
    for (i in 1:4) {
      for (j in 1:4) {
        Ptc[i,j] <- ifelse(i==j, NA, RR$get()*Ptc[i,j])
      }
      Ptc[i,which(is.na(Ptc[i,]))] <- 1-sum(Ptc[i,],na.rm=TRUE) 
    }
    # set transition rates from probabilities
    m$set_rates(Ptc, tcycle)
    # check them
    TC <- m$transition_probability(tcycle)
    if (expected) {
      E <- matrix(
        c(0.858, 0.103, 0.034, 0.005,  
          0.000, 0.787, 0.207, 0.006,
          0.000, 0.000, 0.873, 0.127,
          0.000, 0.000, 0.000, 1.000),   
        byrow = TRUE,
        nrow = 4)
      expect_true(all(TC-E < 0.01))
    }
    # run combination therapy model for 2 years
    populations <- c('A'=N, 'B'=0, 'C'=0, 'D'=0)
    m$reset(populations)
    # run 2 cycles
    MT.comb <- m$cycles(
      2, 
      tcycle = tcycle, 
      hcc.pop = hcc.pop,
      hcc.cost = hcc.cost
    )
    # set costs
    sA$set_cost(cAm)
    sB$set_cost(cBm)
    sC$set_cost(cCm)
    sD$set_cost(0)
    # set transition rates from probabilities
    m$set_rates(Pt, tcycle)
    # set populations in mono model & reset cycle counter and time
    populations <- m$get_populations()
    m$reset(
      populations, 
      icycle=as.integer(2), 
      elapsed=as.difftime(365.25*2, units="days")
    )
    # run mono model for next 18 years
    MT.comb <- rbind(
      MT.comb, 
      m$cycles(
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
             "el.comb"=el.comb, "cost.comb"=cost.comb
             ))
  }
  #
  # Point estimate (without half-cycle correction)
  # ==============================================
  # run the model
  M <- runmodel(expected=TRUE, hcc.pop=FALSE, hcc.cost=FALSE)
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
  # run the model
  M <- runmodel(expected=TRUE, hcc.pop=TRUE, hcc.cost=FALSE)
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
    # run the model
    M <- runmodel(expected=FALSE, hcc.pop=TRUE, hcc.cost=FALSE)
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


