# Script to construct, in rdecision, the model described by Chancellor et al
# (Pharmacoeconomics 1997;12:54-66) for comparing two drugs used for HIV, and
# to check that its results agree with those presented in the paper, and those
# reported by Briggs et al, exercises 2.5 and 4.7, which replicated the model
# reported by Chancellor.
#
# The checks are done as part of the testthat framework, which ensures that
# any changes in the package code which unintentionally result in deviations
# from the reported results of the model are identified.
#
# Code to construct and run the model is contained within labelled knitr code
# chunks and do not contain test expectations, so can be used by a vignette.
# Unlabelled code chunks may contain testthat expectations and should be
# ignored by a vignette.

## @knitr model ---------------------------------------------------------------
# create Markov states
sA <- MarkovState$new("A")
sB <- MarkovState$new("B")
sC <- MarkovState$new("C")
sD <- MarkovState$new("D", utility = 0.0)
# create transitions
tAA <- Transition$new(sA, sA)
tAB <- Transition$new(sA, sB)
tAC <- Transition$new(sA, sC)
tAD <- Transition$new(sA, sD)
tBB <- Transition$new(sB, sB)
tBC <- Transition$new(sB, sC)
tBD <- Transition$new(sB, sD)
tCC <- Transition$new(sC, sC)
tCD <- Transition$new(sC, sD)
tDD <- Transition$new(sD, sD)
# set discount rates
cDR <- 6.0 # annual discount rate, costs (%)
oDR <- 0.0 # annual discount rate, benefits (%)
# construct the model
m <- SemiMarkovModel$new(
  V = list(sA, sB, sC, sD),
  E = list(tAA, tAB, tAC, tAD, tBB, tBC, tBD, tCC, tCD, tDD),
  discount.cost = cDR / 100.0,
  discount.utility = oDR / 100.0
)

## @knitr ---------------------------------------------------------------------
gml <- m$as_gml()
gmlfile <- "gml.gml" #tempfile(fileext = ".gml")
writeLines(gml, con = gmlfile)
gv <- m$as_DOT()
gvfile <- "gv.dot" #tempfile(fileext = ".gml")
writeLines(gv, con = gvfile)

## @knitr costs-det -----------------------------------------------------------
# drug costs
cAZT <- 2278.0 # zidovudine drug cost
cLam <- 2087.0 # lamivudine drug cost

# direct medical and community costs
dmca <- 1701.0 # direct medical costs associated with state A
dmcb <- 1774.0 # direct medical costs associated with state B
dmcc <- 6948.0 # direct medical costs associated with state C
ccca <- 1055.0 # Community care costs associated with state A
cccb <- 1278.0 # Community care costs associated with state B
cccc <- 2059.0 # Community care costs associated with state C

# occupancy costs with monotherapy
cAm <- dmca + ccca + cAZT
cBm <- dmcb + cccb + cAZT
cCm <- dmcc + cccc + cAZT

# occupancy costs with combination therapy
cAc <- dmca + ccca + cAZT + cLam
cBc <- dmcb + cccb + cAZT + cLam
cCc <- dmcc + cccc + cAZT + cLam

## @knitr txeffect-det --------------------------------------------------------
RR <- 0.509

## @knitr pt-mono-det ----------------------------------------------------------
# transition counts
nAA <- 1251L
nAB <- 350L
nAC <- 116L
nAD <- 17L
nBB <- 731L
nBC <- 512L
nBD <- 15L
nCC <- 1312L
nCD <- 437L
# create transition matrix
nA <- nAA + nAB + nAC + nAD
nB <- nBB + nBC + nBD
nC <- nCC + nCD
Ptm <- matrix(
  c(nAA / nA, nAB / nA, nAC / nA, nAD / nA,
    0.0, nBB / nB, nBC / nB, nBD / nB,
    0.0,      0.0, nCC / nC, nCD / nC,
    0.0,      0.0,      0.0,      1.0),
  nrow = 4L, byrow = TRUE,
  dimnames = list(
    source = c("A", "B", "C", "D"), target = c("A", "B", "C", "D")
  )
)

## @knitr ---------------------------------------------------------------------
test_that("monotherapy transition matrix agrees with Briggs Table 2.2", {
  E <- matrix(
    c(0.721, 0.202, 0.067, 0.010,
      0.000, 0.581, 0.407, 0.012,
      0.000, 0.000, 0.750, 0.250,
      0.000, 0.000, 0.000, 1.000),   # typo in book (D,D) = 1!
    byrow = TRUE,
    nrow = 4L
  )
  expect_true(all(Ptm - E < 0.01))
})

## @knitr run-mono -------------------------------------------------------------
# function to run model for 20 years of monotherapy
run_mono <- function(Ptm, cAm, cBm, cCm, hcc = FALSE) {
  # create starting populations
  N <- 1000L
  populations <- c(A = N, B = 0L, C = 0L, D = 0L)
  m$reset(populations)
  # set costs
  sA$set_cost(cAm)
  sB$set_cost(cBm)
  sC$set_cost(cCm)
  # set transition probabilities
  m$set_probabilities(Ptm)
  # run 20 cycles
  tr <- m$cycles(
    ncycles = 20L, hcc.pop = hcc, hcc.cost = FALSE, hcc.QALY = hcc
  )
  return(tr)
}

## @knitr mono-det-results -----------------------------------------------------
MT.mono <- run_mono(Ptm, cAm, cBm, cCm)
el.mono <- sum(MT.mono$QALY)
cost.mono <- sum(MT.mono$Cost)

## @knitr ---------------------------------------------------------------------
test_that("monotherapy QALY and cost agrees with Briggs tables 2.3, 2.4", {
  expect_intol(el.mono, 7.996, tolerance = 0.03) # 7.991 from spreadsheet
  expect_intol(cost.mono, 44663.0, tolerance = 100.0) # rounding errors in book
})

## @knitr pt-comb-det ----------------------------------------------------------
# annual probabilities modified by treatment effect
pAB <- RR * nAB / nA
pAC <- RR * nAC / nC
pAD <- RR * nAD / nA
pBC <- RR * nBC / nB
pBD <- RR * nBD / nB
pCD <- RR * nCD / nC
# annual transition probability matrix
Ptc <- matrix(
  c(1.0 - pAB - pAC - pAD,               pAB,         pAC, pAD,
    0.0, (1.0 - pBC - pBD),         pBC, pBD,
    0.0,               0.0, (1.0 - pCD), pCD,
    0.0,               0.0,         0.0, 1.0),
  nrow = 4L, byrow = TRUE,
  dimnames = list(
    source = c("A", "B", "C", "D"), target = c("A", "B", "C", "D")
  )
)

## @knitr ---------------------------------------------------------------------
test_that("combo therapy transition matrix agrees with Briggs Table 2.2", {
  E <- matrix(
    c(0.858, 0.103, 0.034, 0.005,
      0.000, 0.787, 0.207, 0.006,
      0.000, 0.000, 0.873, 0.127,
      0.000, 0.000, 0.000, 1.000),
    byrow = TRUE,
    nrow = 4L
  )
  expect_true(all(Ptc - E < 0.01))
})

## @knitr run-combo -----------------------------------------------------------
# function to run model for 2 years of combination therapy and 18 of monotherapy
run_comb <- function(Ptm, cAm, cBm, cCm, Ptc, cAc, cBc, cCc, hcc = FALSE) {
  # set populations
  N <- 1000L
  populations <- c(A = N, B = 0L, C = 0L, D = 0L)
  m$reset(populations)
  # set the transition probabilities accounting for treatment effect
  m$set_probabilities(Ptc)
  # set the costs including those for the additional drug
  sA$set_cost(cAc)
  sB$set_cost(cBc)
  sC$set_cost(cCc)
  # run first 2 yearly cycles with additional drug costs and tx effect
  tr <- m$cycles(2L, hcc.pop = hcc, hcc.cost = FALSE, hcc.QALY = hcc)
  # save the state populations after 2 years
  populations <- m$get_populations()
  # revert probabilities to those without treatment effect
  m$set_probabilities(Ptm)
  # revert costs to those without the extra drug
  sA$set_cost(cAm)
  sB$set_cost(cBm)
  sC$set_cost(cCm)
  # restart the model with populations from first 2 years with extra drug
  m$reset(
    populations,
    icycle = 2L,
    elapsed = as.difftime(365.25 * 2.0, units = "days")
  )
  # run for next 18 years, combining the traces
  tr <- rbind(
    tr,
    m$cycles(ncycles = 18L, hcc.pop = hcc, hcc.cost = FALSE, hcc.QALY = hcc)
  )
  # return the trace
  return(tr)
}

## @knitr comb-det-results -----------------------------------------------------
MT.comb <- run_comb(Ptm, cAm, cBm, cCm, Ptc, cAc, cBc, cCc)
el.comb <- sum(MT.comb$QALY)
cost.comb <- sum(MT.comb$Cost)
icer <- (cost.comb - cost.mono) / (el.comb - el.mono)

## @knitr ---------------------------------------------------------------------
test_that("combo therapy QALY, cost and ICER agree with Briggs ex 2.5", {
  expect_intol(el.comb, 8.937, tolerance = 0.02) # 8.937 from spreadsheet
  expect_intol(cost.comb, 50602.0, 100.0) # rounding errors in book
  expect_intol(icer, 6276.0, tolerance = 20.0)
})

## @knitr hcc-det -------------------------------------------------------------
MT.mono.hcc <- run_mono(Ptm, cAm, cBm, cCm, hcc = TRUE)
el.mono.hcc <- sum(MT.mono.hcc$QALY)
cost.mono.hcc <- sum(MT.mono.hcc$Cost)
MT.comb.hcc <- run_comb(Ptm, cAm, cBm, cCm, Ptc, cAc, cBc, cCc, hcc = TRUE)
el.comb.hcc <- sum(MT.comb.hcc$QALY)
cost.comb.hcc <- sum(MT.comb.hcc$Cost)
icer.hcc <- (cost.comb.hcc - cost.mono.hcc) / (el.comb.hcc - el.mono.hcc)

## @knitr ---------------------------------------------------------------------
test_that("model with HCC agrees with Briggs ex 2.5", {
  expect_intol(el.mono.hcc, 8.475, tolerance = 0.03)
  expect_intol(cost.mono.hcc, 44663.0, tolerance = 100.0)
  expect_intol(el.comb.hcc, 9.42, tolerance = 0.02)
  expect_intol(cost.comb.hcc, 50602.0, 100.0) # rounding errors in book
  expect_intol(icer.hcc, (50602.0 - 44663.0) / (9.42 - 8.475), tolerance = 20.0)
})

## @knitr costs-psa -----------------------------------------------------------
# direct medical and community costs (modelled as gamma distributions)
dmca <- GammaModVar$new("dmca", "GBP", shape = 1.0, scale = 1701.0)
dmcb <- GammaModVar$new("dmcb", "GBP", shape = 1.0, scale = 1774.0)
dmcc <- GammaModVar$new("dmcc", "GBP", shape = 1.0, scale = 6948.0)
ccca <- GammaModVar$new("ccca", "GBP", shape = 1.0, scale = 1055.0)
cccb <- GammaModVar$new("cccb", "GBP", shape = 1.0, scale = 1278.0)
cccc <- GammaModVar$new("cccc", "GBP", shape = 1.0, scale = 2059.0)

# occupancy costs with monotherapy
cAm <- ExprModVar$new("cA", "GBP", rlang::quo(dmca + ccca + cAZT))
cBm <- ExprModVar$new("cB", "GBP", rlang::quo(dmcb + cccb + cAZT))
cCm <- ExprModVar$new("cC", "GBP", rlang::quo(dmcc + cccc + cAZT))

# occupancy costs with combination therapy
cAc <- ExprModVar$new("cAc", "GBP", rlang::quo(dmca + ccca + cAZT + cLam))
cBc <- ExprModVar$new("cBc", "GBP", rlang::quo(dmcb + cccb + cAZT + cLam))
cCc <- ExprModVar$new("cCc", "GBP", rlang::quo(dmcc + cccc + cAZT + cLam))

## @knitr txeffect-psa -------------------------------------------------------
RR <- LogNormModVar$new(
  "Tx effect", "RR", p1 = 0.509, p2 = (0.710 - 0.365) / (2.0 * 1.96), "LN7"
)

## @knitr pt-psa --------------------------------------------------------------
# function to generate a probabilistic transition matrix
pt_prob <- function() {
  # create Dirichlet distributions for conditional probabilities
  DA <- DirichletDistribution$new(c(1251L, 350L, 116L, 17L)) # from A # nolint
  DB <- DirichletDistribution$new(c(731L, 512L, 15L))  # from B # nolint
  DC <- DirichletDistribution$new(c(1312L, 437L)) # from C # nolint
  # sample from the Dirichlet distributions
  DA$sample()
  DB$sample()
  DC$sample()
  # create the transition matrix
  Pt <- matrix(
    c(DA$r(), c(0.0, DB$r()), c(0.0, 0.0, DC$r()), c(0.0, 0.0, 0.0, 1.0)),
    byrow = TRUE,
    nrow = 4L,
    dimnames = list(
      source = c("A", "B", "C", "D"), target = c("A", "B", "C", "D")
    )
  )
  return(Pt)
}

## @knitr run-psa -------------------------------------------------------------
# create matrix to hold the incremental costs and life years for each run
psa <- matrix(
  data = NA_real_, nrow = 1000L, ncol = 5L,
  dimnames = list(
    NULL, c("el.mono", "cost.mono", "el.comb", "cost.comb", "icer")
  )
)

# run the model repeatedly
for (irun in seq_len(nrow(psa))) {

  # sample variables from their uncertainty distributions
  cAm$set("random")
  cBm$set("random")
  cCm$set("random")
  cAc$set("random")
  cBc$set("random")
  cCc$set("random")
  RR$set("random")

  # sample the probability transition matrix from observed counts
  Ptm <- pt_prob()

  # run monotherapy model
  MT.mono <- run_mono(Ptm, cAm, cBm, cCm, hcc = TRUE)
  el.mono <- sum(MT.mono$QALY)
  cost.mono <- sum(MT.mono$Cost)
  psa[[irun, "el.mono"]] <- el.mono
  psa[[irun, "cost.mono"]] <- cost.mono

  # create Pt for combination therapy (Briggs applied the RR to the transition
  # probabilities - not recommended, but done here for reproducibility).
  Ptc <- Ptm
  for (i in 1L:4L) {
    for (j in 1L:4L) {
      Ptc[[i, j]] <- ifelse(i == j, NA, RR$get() * Ptc[[i, j]])
    }
    Ptc[i, which(is.na(Ptc[i, ]))] <- 1.0 - sum(Ptc[i, ], na.rm = TRUE)
  }

  # run combination therapy model
  MT.comb <- run_comb(Ptm, cAm, cBm, cCm, Ptc, cAc, cBc, cCc, hcc = TRUE)
  el.comb <- sum(MT.comb$QALY)
  cost.comb <- sum(MT.comb$Cost)
  psa[[irun, "el.comb"]] <- el.comb
  psa[[irun, "cost.comb"]] <- cost.comb

  # calculate the icer
  psa[[irun, "icer"]] <- (cost.comb - cost.mono) / (el.comb - el.mono)
}

## @knitr ---------------------------------------------------------------------
test_that("PSA matches Briggs ex4.7", {

  # skip PSA tests on CRAN
  skip_on_cran()
  # retrieve data set with individual run results from Briggs
  data(BriggsEx47, package = "rdecision")
  # compare observed with expected
  suppressWarnings({
    ht <- ks.test(psa[, "el.mono"], BriggsEx47$Mono.LYs)
    expect_gt(ht$p.value, 0.001)
  })
  suppressWarnings({
    ht <- ks.test(psa[, "cost.mono"], BriggsEx47$Mono.Cost)
    expect_gt(ht$p.value, 0.001)
  })
  suppressWarnings({
    ht <- ks.test(psa[, "el.comb"], BriggsEx47$Comb.LYs)
    expect_gt(ht$p.value, 0.001)
  })
  suppressWarnings({
    ht <- ks.test(psa[, "cost.comb"], BriggsEx47$Comb.Cost)
    expect_gt(ht$p.value, 0.001)
  })
  suppressWarnings({
    ht <- ks.test(psa[, "icer"], BriggsEx47$ICER)
    expect_gt(ht$p.value, 0.001)
  })
})
