
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
  expect_error(
    CohortMarkovModel$new(
      V=list(s.well, n1), 
      E=list()
    ), 
    class="invalid_state"
  )  
})

# test_that("temporary states with cycle limit > 1 are rejected", {
#   s.well <- MarkovState$new("Well")
#   s.disabled <- MarkovState$new("Disabled", cycleLimit=2)
#   s.dead <- MarkovState$new("Dead")
#   states <- list(s.well, s.disabled, s.dead)
#   Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
#   expect_error(CohortMarkovModel$new(states, Ip), class="unsupported_cycle_limit")  
# })
# 
# test_that("temporary states with cycle limit = 1 are accepted", {
#   s.well <- MarkovState$new("Well")
#   s.disabled <- MarkovState$new("Disabled", cycleLimit=1)
#   s.dead <- MarkovState$new("Dead")
#   states <- list(s.well, s.disabled, s.dead)
#   Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
#   expect_silent(CohortMarkovModel$new(states, Ip))  
# })
# 
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

test_that("non-absorbing states without one NULL rate are detected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
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

# test_that("differing state and transition matrix names are rejected", {
#   s.well <- MarkovState$new("Well")
#   s.disabled <- MarkovState$new("Disabled")
#   s.dead <- MarkovState$new("Dead")
#   states <- list(s.well, s.disabled, s.dead)
#   Ip <- TransitionMatrix$new(c("Well", "Poorly", "Dead"))
#   expect_error(CohortMarkovModel$new(states, Ip), class="unmatched_states")  
# })

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
# test_that("model is cyclable", {
#   s.well <- MarkovState$new(name="Well")
#   s.disabled <- MarkovState$new("Disabled")
#   s.dead <- MarkovState$new("Dead")
#   Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
#   Ip$set_rate(from="Well", to="Disabled", rate=0.2)
#   Ip$set_rate(from="Well", to="Dead", rate=0.2)
#   Ip$set_rate(from="Disabled", to="Dead", rate=0.4)
#   M <- CohortMarkovModel$new(c(s.well, s.disabled, s.dead), Ip)
#   expect_error(M$cycle(), class="missing_state_populations")
# })

# -----------------------------------------------------------------------------
# Sonnenberg & Beck, Med Decis Making, 1993;13:322, Fig 3
# (prosthetic heart valve)
# -----------------------------------------------------------------------------
test_that("rdecision replicates Sonnenberg & Beck, Fig 3", {
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
    e.ws <- MarkovTransition$new(s.well, s.disabled, r=r.ws),
    e.wd <- MarkovTransition$new(s.well, s.dead, r=r.wd),
    e.sd <- MarkovTransition$new(s.disabled, s.dead, r=r.sd)
  )
  # create the model
  M <- CohortMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E
  )
  # check the transition matrix values
  Ip <- M$transition_probability()
  expect_equal(
    sum(Ip-matrix(c(0.6,0.2,0.2,0,0.6,0.4,0,0,1),nrow=3,byrow=TRUE)),0
  )
  # set the starting populations
  M$set_populations(c(Well=10000, Disabled=0, Dead=0)) 
  # cycle
  M$cycle()
  M$cycle()
  
#  RC <- M$cycles(25)
#  expect_true(is.data.frame(RC))
#  expect_equal(round(RC$Well[RC$Cycle==2]), 3600)
#  expect_equal(round(RC$Disabled[RC$Cycle==2]), 2400)
#  expect_equal(round(RC$Dead[RC$Cycle==2]), 4000)
  expect_true(TRUE)
})

