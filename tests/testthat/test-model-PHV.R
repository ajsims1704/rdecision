# Script to construct, in rdecision, a model of patients with a prosthetic
# heart valve, given by Sonnenberg and Beck (Med Decis Making 1983; 13: 322-338)
# in their figure 3, with transition probabilities in their table 2 and results
# their table 3.
#
# The checks are done as part of the testthat framework, ensuring that
# changes in the package code which unintentionally result in deviations
# from the expected results of the model are identified.
#
# Code to construct and run the model is contained within labelled knitr code
# chunks and do not contain test expectations, so can be used by a vignette.
# Unlabelled code chunks may contain testthat expectations and should be
# ignored by a vignette.

## @knitr create-states -------------------------------------------------------
# create states
s.well <- MarkovState$new(name = "Well", utility = 1.0)
s.disabled <- MarkovState$new(name = "Disabled", utility = 0.7)
s.dead <- MarkovState$new(name = "Dead", utility = 0.0)

## @knitr create-transitions --------------------------------------------------
# create transitions leaving rates undefined
E <- list(
  Transition$new(s.well, s.well),
  Transition$new(s.dead, s.dead),
  Transition$new(s.disabled, s.disabled),
  Transition$new(s.well, s.disabled),
  Transition$new(s.well, s.dead),
  Transition$new(s.disabled, s.dead)
)

## @knitr create-model --------------------------------------------------------
# create the model
M <- SemiMarkovModel$new(V = list(s.well, s.disabled, s.dead), E)

## @knitr ---------------------------------------------------------------------
test_that("state tabulation is as expected", {
  # check the state tabulation
  ST <- M$tabulate_states()
  expect_setequal(names(ST), c("Name", "Cost", "Utility"))
  expect_identical(nrow(ST), 3L)
})

## @knitr set-pt -------------------------------------------------------------
# create transition probability matrix
snames <- c("Well", "Disabled", "Dead")
Pt <- matrix(
  data = c(0.6, 0.2, 0.2, 0.0, 0.6, 0.4, 0.0, 0.0, 1.0),
  nrow = 3L, byrow = TRUE,
  dimnames = list(source = snames, target = snames)
)
# set the transition rates from per-cycle probabilities
M$set_probabilities(Pt)

## @knitr --------------------------------------------------------------------
test_that("transition probabilities are as expected", {
  OPt <- M$transition_probabilities()
  OPt <- OPt[snames, snames]
  expect_true(all(Pt - OPt < sqrt(.Machine$double.eps)))
})

## @knitr set-pop ------------------------------------------------------------
# set the starting populations
M$reset(c(Well = 10000.0, Disabled = 0.0, Dead = 0.0))

## @knitr cycle --------------------------------------------------------------
# cycle
MT <- M$cycles(25L, hcc.pop = FALSE, hcc.cost = FALSE)

## @knitr --------------------------------------------------------------------
test_that("cycle results match S&B table 2", {
  # check structure of data frame
  expect_identical(M$get_elapsed(), as.difftime(25.0 * 365.25, units = "days"))
  expect_s3_class(MT, "data.frame")
  expect_identical(nrow(MT), 26L)
  # check cycle numbers and times
  expect_identical(MT[, "Cycle"], seq(from = 0L, to = 25L))
  expect_identical(MT[, "Years"], as.numeric(seq(from = 0L, to = 25L)))
  # check costs
  expect_identical(MT[, "Cost"], rep(0.0, times = 26L))
  # spot check one row
  expect_identical(round(MT[which(MT[, "Cycle"] == 2L), "Well"]), 3600.0)
  expect_identical(round(MT[which(MT[, "Cycle"] == 2L), "Disabled"]), 2400.0)
  expect_identical(round(MT[which(MT[, "Cycle"] == 2L), "Dead"]), 4000.0)
})

## @knitr trace-to-t2 ---------------------------------------------------------
t2 <- data.frame(
  Cycle = MT[, "Cycle"],
  Well = round(MT[, "Well"], 0L),
  Disabled = round(MT[, "Disabled"], 0L),
  Dead = round(MT[, "Dead"], 0L),
  CycleSum = round(MT[, "QALY"] * 10000.0, 0L),
  CumulativeUtility = round(10000.0 * cumsum(MT[, "QALY"]), 0L)
)

## @knitr ---------------------------------------------------------------------
test_that("reformatted cycle results match S&B table 2", {
  # cycle 0
  r0 <- which(t2[, "Cycle"] == 0L)
  expect_identical(t2[r0, "Well"], 10000.0)
  expect_identical(t2[r0, "Disabled"], 0.0)
  expect_identical(t2[r0, "Dead"], 0.0)
  expect_identical(t2[r0, "CycleSum"], 0.0)
  expect_identical(t2[r0, "CumulativeUtility"], 0.0)
  # cycle 1
  r <- which(t2[, "Cycle"] == 1L)
  expect_identical(t2[r, "Well"], 6000.0)
  expect_identical(t2[r, "Disabled"], 2000.0)
  expect_identical(t2[r, "Dead"], 2000.0)
  expect_identical(t2[r, "CycleSum"], 7400.0)
  expect_identical(t2[r, "CumulativeUtility"], 7400.0)
  # cycle 2
  r <- which(t2[, "Cycle"] == 2L)
  expect_identical(t2[r, "Well"], 3600.0)
  expect_identical(t2[r, "Disabled"], 2400.0)
  expect_identical(t2[r, "Dead"], 4000.0)
  expect_identical(t2[r, "CycleSum"], 5280.0)
  expect_identical(t2[r, "CumulativeUtility"], 12680.0)
  # cycle 23
  r <- which(t2[, "Cycle"] == 23L)
  expect_identical(t2[r, "Well"], 0.0)
  expect_identical(t2[r, "Disabled"], 1.0)
  expect_identical(t2[r, "Dead"], 9999.0)
  expect_identical(t2[r, "CycleSum"], 1.0)  # typo in paper?
  expect_intol(t2[r, "CumulativeUtility"], 23752.0, tol = 5.0)
  # cycle 24
  r <- which(t2[, "Cycle"] == 24L)
  expect_identical(t2[r, "Well"], 0.0)
  expect_identical(t2[r, "Disabled"], 0.0)
  expect_identical(t2[r, "Dead"], 10000.0)
  expect_identical(t2[r, "CycleSum"], 0.0)
  expect_intol(t2[r, "CumulativeUtility"], 23752.0, tol = 5.0)
})
