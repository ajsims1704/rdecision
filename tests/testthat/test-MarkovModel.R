
# -----------------------------------------------------------------------------
# tests of creating a model
# -----------------------------------------------------------------------------
test_that("incorrect state types are rejected", {
  s.well <- MarkovState$new("Well")
  states <- list(s.well, "Disabled", "Dead")
  Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
  expect_error(MarkovModel$new(states, Ip), class="not_markov_state")  
})

test_that("an incorrect transition matrix type is rejected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  states <- list(s.well, s.disabled, s.dead)
  Ip <- list("Well", "Disabled", "Dead")
  expect_error(MarkovModel$new(states, Ip), class="not_transition_matrix")  
})

test_that("differing state and transition matrix names are rejected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  states <- list(s.well, s.disabled, s.dead)
  Ip <- TransitionMatrix$new(c("Well", "Poorly", "Dead"))
  expect_error(MarkovModel$new(states, Ip), class="unmatched_states")  
})

# -----------------------------------------------------------------------------
# tests of setting state populations
# -----------------------------------------------------------------------------
test_that("invalid population vectors are rejected", {
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
  M <- MarkovModel$new(list(s.well, s.disabled, s.dead), Ip)
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
# Sonnenberg & Beck, Med Decis Making, 1993;13:322, Fig 3
# (prosthetic heart valve)
# -----------------------------------------------------------------------------
test_that("rdecision replicates Sonnenberg & Beck, Fig 3", {
  # states
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  # transition matrix
  Ip <- TransitionMatrix$new(c("Well", "Disabled", "Dead"))
  Ip$set_rate(from="Well", to="Disabled", rate=0.2)
  Ip$set_rate(from="Well", to="Dead", rate=0.2)
  Ip$set_rate(from="Disabled", to="Dead", rate=0.4)
  # model
  M <- MarkovModel$new(c(s.well, s.disabled, s.dead), Ip)
  # starting populations
  M$set_populations(c(Well=10000, Disabled=0, Dead=0))  
  # first cycle
  RC <- M$cycle()
  # check results
  expect_true(is.data.frame(RC))
#  expect_equal(RC$Population)
})

# -----------------------------------------------------------------------------
# transition matrix tests
# -----------------------------------------------------------------------------

# test_that("a transition matrix with no NAs is rejected", {
#   s.well <- MarkovState$new(name="Well")
#   s.disabled <- MarkovState$new("Disabled")
#   s.dead <- MarkovState$new("Dead")
#   Ip <- matrix(
#     data = c( 0.6, 0.2, 0.2,
#               0.0, 0.6, 0.4,
#               0.0, 0.0, 1.0),
#     nrow = 3,
#     ncol = 3, 
#     byrow = TRUE,
#     dimnames = list(c("Well", "Disabled", "Dead"), c("Well", "Disabled", "Dead"))
#   )
#   expect_error(model <- MarkovModel$new(c(s.well, s.disabled, s.dead), Ip))
# })
# 
# test_that("a transition matrix with too many NAs is rejected", {
#   s.well <- MarkovState$new(name="Well")
#   s.disabled <- MarkovState$new("Disabled")
#   s.dead <- MarkovState$new("Dead")
#   Ip <- matrix(
#     data = c(  NA, 0.2, 0.2,
#               0.0,  NA, 0.4,
#                NA, 0.0,  NA),
#     nrow = 3,
#     ncol = 3, 
#     byrow = TRUE,
#     dimnames = list(c("Well", "Disabled", "Dead"), c("Well", "Disabled", "Dead"))
#   )
#   expect_error(model <- MarkovModel$new(c(s.well, s.disabled, s.dead), Ip))
# })
# 
# 
