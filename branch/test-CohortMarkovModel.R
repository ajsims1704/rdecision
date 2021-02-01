
# -----------------------------------------------------------------------------
# tests of creating a model
# -----------------------------------------------------------------------------
test_that("incorrect state types are rejected", {
  s.well <- MarkovState$new("Well")
  states <- list(s.well, "Disabled", "Dead")
  Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
  expect_error(CohortMarkovModel$new(states, Ip), class="not_markov_state")  
})

test_that("temporary states with cycle limit > 1 are rejected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled", cycleLimit=2)
  s.dead <- MarkovState$new("Dead")
  states <- list(s.well, s.disabled, s.dead)
  Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
  expect_error(CohortMarkovModel$new(states, Ip), class="unsupported_cycle_limit")  
})

test_that("temporary states with cycle limit = 1 are accepted", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled", cycleLimit=1)
  s.dead <- MarkovState$new("Dead")
  states <- list(s.well, s.disabled, s.dead)
  Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
  expect_silent(CohortMarkovModel$new(states, Ip))  
})

test_that("an incorrect transition matrix type is rejected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  states <- list(s.well, s.disabled, s.dead)
  Ip <- list("Well", "Disabled", "Dead")
  expect_error(CohortMarkovModel$new(states, Ip), class="not_transition_matrix")  
})

test_that("differing state and transition matrix names are rejected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  states <- list(s.well, s.disabled, s.dead)
  Ip <- TransitionMatrix$new(c("Well", "Poorly", "Dead"))
  expect_error(CohortMarkovModel$new(states, Ip), class="unmatched_states")  
})

# -----------------------------------------------------------------------------
# tests of setting state populations
# -----------------------------------------------------------------------------
test_that("invalid population vectors are rejected", {
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
  M <- CohortMarkovModel$new(list(s.well, s.disabled, s.dead), Ip)
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
})

# -----------------------------------------------------------------------------
# tests of cycling
# -----------------------------------------------------------------------------
test_that("model is cyclable", {
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
  Ip$set_rate(from="Well", to="Disabled", rate=0.2)
  Ip$set_rate(from="Well", to="Dead", rate=0.2)
  Ip$set_rate(from="Disabled", to="Dead", rate=0.4)
  M <- CohortMarkovModel$new(c(s.well, s.disabled, s.dead), Ip)
  expect_error(M$cycle(), class="missing_state_populations")
})

# -----------------------------------------------------------------------------
# Sonnenberg & Beck, Med Decis Making, 1993;13:322, Fig 3
# (prosthetic heart valve)
# -----------------------------------------------------------------------------
test_that("rdecision replicates Sonnenberg & Beck, Fig 3", {
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
  Ip$set_rate(from="Well", to="Disabled", rate=0.2)
  Ip$set_rate(from="Well", to="Dead", rate=0.2)
  Ip$set_rate(from="Disabled", to="Dead", rate=0.4)
  M <- CohortMarkovModel$new(c(s.well, s.disabled, s.dead), Ip)
  M$set_populations(c(Well=10000, Disabled=0, Dead=0))  
  RC <- M$cycles(25)
  expect_true(is.data.frame(RC))
  expect_equal(round(RC$Well[RC$Cycle==2]), 3600)
  expect_equal(round(RC$Disabled[RC$Cycle==2]), 2400)
  expect_equal(round(RC$Dead[RC$Cycle==2]), 4000)
})

