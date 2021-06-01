
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
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  # no outgoing transitions from one state
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.ws <- MarkovTransition$new(s.well, s.disabled, r=0.2)
  expect_error(
    CohortMarkovModel$new(
      V = list(s.well, s.disabled),
      E = list(e.ww, e.ws)
    ), 
    class="invalid_rate"
  ) 
  # under-constrain (no rates specified for outgoing 'disabled' state)
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.ss <- MarkovTransition$new(s.disabled, s.disabled)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  e.ws <- MarkovTransition$new(s.well, s.disabled, r=0.2)
  e.wd <- MarkovTransition$new(s.well, s.dead, r=0.2)
  e.sd <- MarkovTransition$new(s.disabled, s.dead)
  expect_error(
    CohortMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
    ), 
    class="invalid_rate"
  ) 
  # over-constrain (all rates specified for 'dead' state)
  e.sd <- MarkovTransition$new(s.disabled, s.dead, r=0.4)
  e.dd <- MarkovTransition$new(s.dead, s.dead, r=1)
  expect_error(
    CohortMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
    ), 
    class="invalid_rate"
  )
  # correctly specified 
  e.ww <- MarkovTransition$new(s.well, s.well)
  e.ss <- MarkovTransition$new(s.disabled, s.disabled)
  e.dd <- MarkovTransition$new(s.dead, s.dead)
  e.ws <- MarkovTransition$new(s.well, s.disabled, r=0.2)
  e.wd <- MarkovTransition$new(s.well, s.dead, r=0.2)
  e.sd <- MarkovTransition$new(s.disabled, s.dead, r=0.4)
  expect_silent(
    CohortMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
    )
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
  Ip <- M$transition_probability()
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
  M <- CohortMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E
  )
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
  M <- CohortMarkovModel$new(
    V = list(s.well, s.dead), 
    E, 
    hcc = FALSE
  )
  MT <- M$cycles(5)
  expect_equal(MT$Well[MT$Cycle==5], 327.68)
  # create the model and cycle for 5 years
  M <- CohortMarkovModel$new(
    V = list(s.well, s.dead), 
    E, 
    hcc = FALSE,
    tcycle = as.difftime(365.25/12, units="days")
  )
  MT <- M$cycles(5*12)
  expect_equal(MT$Well[MT$Cycle==60], 327.68)
})

# -----------------------------------------------------------------------------
# Sonnenberg & Beck, Med Decis Making, 1993;13:322, Fig 3
# (prosthetic heart valve)
# -----------------------------------------------------------------------------
test_that("rdecision replicates Sonnenberg & Beck, Fig 3", {
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
  M <- CohortMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E,
    hcc = FALSE
  )
  # check the transition matrix values
  Ip <- M$transition_probability()
  expect_equal(
    sum(Ip-matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)),0
  )
  # set the starting populations
  M$set_populations(c(Well=10000, Disabled=0, Dead=0)) 
  # cycle
  RC <- M$cycles(25)
  expect_true(is.data.frame(RC))
  expect_equal(round(RC$Well[RC$Cycle==2]), 3600)
  expect_equal(round(RC$Disabled[RC$Cycle==2]), 2400)
  expect_equal(round(RC$Dead[RC$Cycle==2]), 4000)
  expect_true(TRUE)
})

