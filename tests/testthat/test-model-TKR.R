# Script to construct, in rdecision, the model described by Dong and Buxton (Int
# J Health Tech Assessment 2006;22:191-202) for computer-assisted total knee
# replacement, and to check that its results agree with those presented in the
# paper.
#
# The checks are done as part of the testthat framework, ensuring that
# changes in the package code which unintentionally result in deviations
# from the expected results of the model are identified.
#
# Code to construct and run the model is contained within labelled knitr code
# chunks and do not contain test expectations, so can be used by a vignette.
# Unlabelled code chunks may contain testthat expectations and should be
# ignored by a vignette.

## @knitr state-names ---------------------------------------------------------
states <- c(
  A = "TKR operation for knee problems",
  B = "TKR with serious complications",
  C = "TKR with minor complications",
  D = "Normal health after primary TKR",
  E = "Complex revision",
  F = "Simple revision",
  G = "Other treatments",
  H = "Normal health after TKR revision",
  I = "Death"
)

## @knitr utils-point ----------------------------------------------------------
utility_A <- 0.72
utility_B <- 0.35
utility_C <- 0.66
utility_D <- 0.78
utility_E <- 0.51
utility_F <- 0.66
utility_G <- 0.72
utility_H <- 0.68
utility_I <- 0.00

## @knitr costs-point ----------------------------------------------------------
cost_A <- 5197.0
cost_B <- 0.0
cost_C <- 0.0
cost_D <- 0.0
cost_E <- 7326.0
cost_F <- 6234.0
cost_G <- 2844.0
cost_H <- 0.0
cost_I <- 0.0
cost_CAS <- 235.0


## @knitr SMM-point ------------------------------------------------------------
# Markov states
sA <- MarkovState$new(states["A"], utility = utility_A)
sB <- MarkovState$new(states["B"], utility = utility_B)
sC <- MarkovState$new(states["C"], utility = utility_C)
sD <- MarkovState$new(states["D"], utility = utility_D)
sE <- MarkovState$new(states["E"], utility = utility_E)
sF <- MarkovState$new(states["F"], utility = utility_F)
sG <- MarkovState$new(states["G"], utility = utility_G)
sH <- MarkovState$new(states["H"], utility = utility_H)
sI <- MarkovState$new(states["I"], utility = utility_I)
States <- list(sA, sB, sC, sD, sE, sF, sG, sH, sI)

# Transitions
tAD <- Transition$new(sA, sD, cost = cost_A)
tAC <- Transition$new(sA, sC, cost = cost_A)
tAB <- Transition$new(sA, sB, cost = cost_A)
tBC <- Transition$new(sB, sC)
tBE <- Transition$new(sB, sE, cost = cost_E)
tBF <- Transition$new(sB, sF, cost = cost_F)
tBG <- Transition$new(sB, sG, cost = cost_G)
tCB <- Transition$new(sC, sB)
tCD <- Transition$new(sC, sD)
tCF <- Transition$new(sC, sF, cost = cost_F)
tCG <- Transition$new(sC, sG, cost = cost_G)
tCC <- Transition$new(sC, sC)
tDC <- Transition$new(sD, sC)
tDB <- Transition$new(sD, sB)
tDD <- Transition$new(sD, sD)
tEB <- Transition$new(sE, sB)
tEH <- Transition$new(sE, sH)
tFB <- Transition$new(sF, sB)
tFC <- Transition$new(sF, sC)
tFG <- Transition$new(sF, sG, cost = cost_G)
tFH <- Transition$new(sF, sH)
tGB <- Transition$new(sG, sB)
tGC <- Transition$new(sG, sC)
tGF <- Transition$new(sG, sF, cost = cost_F)
tGD <- Transition$new(sG, sD)
tHE <- Transition$new(sH, sE, cost = cost_E)
tHF <- Transition$new(sH, sF, cost = cost_F)
tHH <- Transition$new(sH, sH)
tBI <- Transition$new(sB, sI)
tCI <- Transition$new(sC, sI)
tDI <- Transition$new(sD, sI)
tEI <- Transition$new(sE, sI)
tFI <- Transition$new(sF, sI)
tGI <- Transition$new(sG, sI)
tHI <- Transition$new(sH, sI)
tII <- Transition$new(sI, sI)

# Transitions incorporating CAS cost
tAD_CAS <- Transition$new(sA, sD, cost = cost_A + cost_CAS)
tAC_CAS <- Transition$new(sA, sC, cost = cost_A + cost_CAS)
tAB_CAS <- Transition$new(sA, sB, cost = cost_A + cost_CAS)
tBE_CAS <- Transition$new(sB, sE, cost = cost_E + cost_CAS)
tBF_CAS <- Transition$new(sB, sF, cost = cost_F + cost_CAS)
tCF_CAS <- Transition$new(sC, sF, cost = cost_F + cost_CAS)
tGF_CAS <- Transition$new(sG, sF, cost = cost_F + cost_CAS)
tHE_CAS <- Transition$new(sH, sE, cost = cost_E + cost_CAS)
tHF_CAS <- Transition$new(sH, sF, cost = cost_F + cost_CAS)

Transitions_base <- list(
  tAD, tAC, tAB, tBC, tBE, tBF, tBG, tCB, tCD, tCF, tCG, tCC,
  tDC, tDB, tDD, tEB, tEH, tFB, tFC, tFG, tFH, tGB, tGC, tGF,
  tGD, tHE, tHF, tHH, tBI, tCI, tDI, tEI, tFI, tGI, tHI, tII
)

Transitions_CAS <- list(
  tAD_CAS, tAC_CAS, tAB_CAS, tBC, tBE_CAS, tBF_CAS, tBG, tCB, tCD, tCF_CAS,
  tCG, tCC, tDC, tDB, tDD, tEB, tEH, tFB, tFC, tFG, tFH, tGB, tGC, tGF_CAS,
  tGD, tHE_CAS, tHF_CAS, tHH, tBI, tCI, tDI, tEI, tFI, tGI, tHI, tII
)

## @knitr SMM-def-point --------------------------------------------------------
SMM_base <- SemiMarkovModel$new(
  V = States, E = Transitions_base,
  tcycle = as.difftime(365.25 / 12L, units = "days"), # cycles in months
  discount.cost = 0.035,
  discount.utility = 0.035
)

SMM_CAS <- SemiMarkovModel$new(
  V = States, E = Transitions_CAS,
  tcycle = as.difftime(365.25 / 12L, units = "days"), # cycles in months
  discount.cost = 0.035,
  discount.utility = 0.035
)

## @knitr transitions-point ---------------------------------------------------
# Death
p_death_all <- 0.00341
p_death_primary <- 0.00046
p_death_revision <- 0.00151

# Transitions
p_AtoB <- 0.01495
p_AtoC <- 0.04285
p_AtoD <- NA # alternatively: 1 - p_AtoB - p_AtoC
p_BtoC <- 0.01385
p_BtoE <- 0.02469
p_BtoF <- 0.00523
p_BtoI <- p_death_all + p_death_primary
p_BtoG <- NA # alternatively: 1 - p_BtoC - p_BtoE - p_BtoF - p_BtoI
p_CtoB <- 0.00921
p_CtoC <- 0.02505
p_CtoF <- 0.00250
p_CtoG <- 0.01701
p_CtoI <- p_death_all + p_death_primary
p_CtoD <- NA # alternatively: 1 - p_CtoB - p_CtoC - p_CtoF - p_CtoG - p_CtoI
p_DtoB <- 0.00921
p_DtoC <- 0.01385
p_DtoI <- p_death_all + p_death_primary
p_DtoD <- NA # alternatively: 1 - p_DtoB - p_DtoC - p_DtoI
p_EtoB <- 0.02545
p_EtoI <- p_death_all + p_death_revision
p_EtoH <- NA # alternatively: 1 - p_EtoB - p_EtoI
p_FtoB <- 0.01590
p_FtoC <- 0.00816
p_FtoG <- 0.01701
p_FtoI <- p_death_all + p_death_revision
p_FtoH <- NA # alternatively: 1 - p_FtoB - p_FtoC - p_FtoG - p_FtoI
p_GtoB <- 0.00921
p_GtoC <- 0.01385
p_GtoF <- 0.00250
p_GtoI <- p_death_all + p_death_primary
p_GtoD <- NA # alternatively: 1 - p_GtoB - p_GtoC - p_GtoF - p_GtoI
p_HtoE <- 0.02003
p_HtoF <- 0.01038
p_HtoI <- p_death_all + p_death_revision
p_HtoH <- NA # alternatively: 1 - p_HtoE - p_HtoF - p_HtoI
p_ItoI <- 1.0

# Set transition probabilities
Pt <- matrix(c(
  0L, p_AtoB, p_AtoC, p_AtoD,      0L,      0L,      0L,      0L,     0L,
  0L,     0L, p_BtoC,     0L,  p_BtoE,  p_BtoF,  p_BtoG,      0L, p_BtoI,
  0L, p_CtoB, p_CtoC, p_CtoD,      0L,  p_CtoF,  p_CtoG,      0L, p_CtoI,
  0L, p_DtoB, p_DtoC, p_DtoD,      0L,      0L,      0L,      0L, p_DtoI,
  0L, p_EtoB,     0L,     0L,      0L,      0L,      0L,  p_EtoH, p_EtoI,
  0L, p_FtoB, p_FtoC,     0L,      0L,      0L,  p_FtoG,  p_FtoH, p_FtoI,
  0L, p_GtoB, p_GtoC, p_GtoD,      0L, p_GtoF,       0L,      0L, p_GtoI,
  0L,     0L,     0L,     0L,  p_HtoE,  p_HtoF,      0L,  p_HtoH, p_HtoI,
  0L,     0L,     0L,     0L,      0L,      0L,      0L,      0L, p_ItoI
), nrow = 9L, byrow = TRUE, dimnames = list(
  source = states[LETTERS[1L:9L]], target = states[LETTERS[1L:9L]]
))
SMM_base$set_probabilities(Pt)


## @knitr tx-effect -----------------------------------------------------------
txeffect <- function(Pt, rr) {
  # copy transition matrix
  pr <- Pt
  # calculate reduced probability of transition to state B
  derisk <- states[["B"]]
  dpb <- pr[, derisk] * rr
  # reduce the probability of making a transition to state B and increase the
  # probability of making a transition elsewhere by the same amount
  uprisks <- c("D", "G", "D", "D", "H", "H", "D", "H", "I")
  for (i in 1L:9L) {
    s <- states[[i]]
    pr[[s, derisk]] <- pr[[s, derisk]] - dpb[[i]]
    uprisk <- states[[uprisks[[i]]]]
    pr[[s, uprisk]] <- pr[[s, uprisk]] + dpb[[i]]
  }
  return(pr)
}

## @knitr CAS-transitions-point ------------------------------------------------
# apply CAS_effect to the transition matrix
CAS_effect <- 0.34
Pt_CAS <- txeffect(Pt, CAS_effect)
SMM_CAS$set_probabilities(Pt_CAS)

## @knitr cycle-point ----------------------------------------------------------
# create starting populations
N <- 1000L
populations <- c(N, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
names(populations) <- states

# run 120 one-month cycles for both models
SMM_base$reset(populations)
SMM_base_10years <- SMM_base$cycles(
  ncycles = 120L, hcc.pop = FALSE, hcc.cost = FALSE
)
SMM_CAS$reset(populations)
SMM_CAS_10years <- SMM_CAS$cycles(
  ncycles = 120L, hcc.pop = FALSE, hcc.cost = FALSE
)

## @knitr as-table4 ----------------------------------------------------------
# convert a Markov trace (matrix) with monthly cycles to an annual summary
# matrix with cumulative values, as per Dong and Buxton, Table 4
as_table4 <- function(m_trace) {
  # calculate cumulative event counts
  m_cum <- apply(m_trace, 2L, cumsum)
  # create annual summary table
  m_t4 <- matrix(
    data = c(
      m_trace[, "Years"],
      m_cum[, "TKR with serious complications"] / 10L,
      m_cum[, "TKR with minor complications"] / 10L,
      m_cum[, "Complex revision"] / 10L,
      m_cum[, "Simple revision"] / 10L,
      m_trace[, "Death"] / 10L,
      m_cum[, "Cost"] * 1000L,
      m_cum[, "QALY"] * 1000L
    ),
    dimnames = list(
      NULL,
      c(
        "Year",
        "Cumulative serious complication (%)",
        "Cumulative minor complication (%)",
        "Cumulative complex revision (%)",
        "Cumulative simple revision (%)",
        "Cumulative death (%)",
        "Discounted costs (£)",
        "Discounted QALYs"
      )
    ),
    nrow = 121L, ncol = 8L
  )
  # return in table 4 format
  yearly <- (1L:10L) * 12L + 1L
  return(m_t4[yearly, ])
}

## @knitr cs-table-4 ----------------------------------------------------------
t4_CS <- as_table4(SMM_base_10years)

## @knitr ---------------------------------------------------------------------
test_that("Table 4 for conventional surgery is replicated", {
  expect_identical(nrow(t4_CS), 10L)
  expect_intol(
    t4_CS[[10L, "Cumulative serious complication (%)"]], 87.32, tolerance = 0.2
  )
  expect_intol(
    t4_CS[[10L, "Cumulative minor complication (%)"]], 135.87, tolerance = 0.2
  )
  expect_intol(
    t4_CS[[10L, "Cumulative complex revision (%)"]], 5.13, tolerance = 0.2
  )
  expect_intol(
    t4_CS[[10L, "Cumulative simple revision (%)"]], 2.55, tolerance = 0.2
  )
  expect_intol(
    t4_CS[[10L, "Cumulative death (%)"]], 37.10, tolerance = 0.2
  )
  expect_intol(
    t4_CS[[10L, "Discounted costs (£)"]], 7791288.9, tolerance = 100000.0
  )
  expect_intol(
    t4_CS[[10L, "Discounted QALYs"]], 5526.4, tolerance = 250.0
  )
})

## @knitr cas-table-4 ----------------------------------------------------------
t4_CAS <- as_table4(SMM_CAS_10years)

## @knitr ---------------------------------------------------------------------
test_that("Table 4 for computer-assisted surgery is replicated", {
  expect_identical(nrow(t4_CAS), 10L)
  expect_intol(
    t4_CAS[[10L, "Cumulative serious complication (%)"]], 58.14, tolerance = 0.2
  )
  expect_intol(
    t4_CAS[[10L, "Cumulative minor complication (%)"]], 136.52, tolerance = 0.2
  )
  expect_intol(
    t4_CAS[[10L, "Cumulative complex revision (%)"]], 3.56, tolerance = 0.2
  )
  expect_intol(
    t4_CAS[[10L, "Cumulative simple revision (%)"]], 1.89, tolerance = 0.2
  )
  expect_intol(
    t4_CAS[[10L, "Cumulative death (%)"]], 37.06, tolerance = 0.2
  )
  expect_intol(
    t4_CAS[[10L, "Discounted costs (£)"]], 7208671.4, tolerance = 100000.0
  )
  expect_intol(
    t4_CAS[10L, "Discounted QALYs"], 5541.2, tolerance = 250.0
  )
})

## @knitr cea -----------------------------------------------------------------
dcost <- t4_CAS[[10L, "Discounted costs (£)"]] / 1000L -
  t4_CS[[10L, "Discounted costs (£)"]] / 1000L
dutil <- t4_CAS[[10L, "Discounted QALYs"]] / 1000L -
  t4_CS[[10L, "Discounted QALYs"]] / 1000L

## @knitr ---------------------------------------------------------------------
test_that("Deterministic CEA matches published value", {
  expect_intol(dcost, -583.0, tolerance = 50.0)
  expect_intol(dutil, 0.0164, tolerance = 0.005)
})

## @knitr utilities-var ------------------------------------------------------
utility_A_nu <- 0.42
utility_B_nu <- 0.80
utility_C_nu <- 0.32
utility_D_nu <- 0.34
utility_E_nu <- 0.55
utility_F_nu <- 0.57
utility_G_nu <- 0.34
utility_H_nu <- 0.38

## @knitr utilities-beta -----------------------------------------------------
utility_A_beta <- BetaModVar$new(
  "Utility of state A", "",
  utility_A * utility_A_nu, (1.0 - utility_A) * utility_A_nu
)
utility_B_beta <- BetaModVar$new(
  "Utility of state B", "",
  utility_B * utility_B_nu, (1.0 - utility_B) * utility_B_nu
)
utility_C_beta <- BetaModVar$new(
  "Utility of state C", "",
  utility_C * utility_C_nu, (1.0 - utility_C) * utility_C_nu
)
utility_D_beta <- BetaModVar$new(
  "Utility of state D", "",
  utility_D * utility_D_nu, (1.0 - utility_D) * utility_D_nu
)
utility_E_beta <- BetaModVar$new(
  "Utility of state E", "",
  utility_E * utility_E_nu, (1.0 - utility_E) * utility_E_nu
)
utility_F_beta <- BetaModVar$new(
  "Utility of state F", "",
  utility_F * utility_F_nu, (1.0 - utility_F) * utility_F_nu
)
utility_G_beta <- BetaModVar$new(
  "Utility of state G", "",
  utility_G * utility_G_nu, (1.0 - utility_G) * utility_G_nu
)
utility_H_beta <- BetaModVar$new(
  "Utility of state H", "",
  utility_H * utility_H_nu, (1.0 - utility_H) * utility_H_nu
)

## @knitr costs-gamma --------------------------------------------------------
cost_A_stdev <- (6217L - 4218L) / (2L * 1.96)
cost_E_stdev <- (11307L - 5086L) / (2L * 1.96)
cost_F_stdev <- (7972L - 5043L) / (2L * 1.96)
cost_G_stdev <- (5579L - 1428L) / (2L * 1.96)

cost_A_gamma <- GammaModVar$new(
  "Cost of state A", "", cost_A ^ 2L / cost_A_stdev ^ 2L,
  cost_A_stdev ^ 2L / cost_A
)
cost_E_gamma <- GammaModVar$new(
  "Cost of state E", "", cost_E ^ 2L / cost_E_stdev ^ 2L,
  cost_E_stdev ^ 2L / cost_E
)
cost_F_gamma <- GammaModVar$new(
  "Cost of state F", "", cost_F ^ 2L / cost_F_stdev ^ 2L,
  cost_F_stdev ^ 2L / cost_F
)
cost_G_gamma <- GammaModVar$new(
  "Cost of state G", "", cost_G ^ 2L / cost_G_stdev ^ 2L,
  cost_G_stdev ^ 2L / cost_G
)

## @knitr cas-cost-gamma ------------------------------------------------------
cost_CAS_stdev <- 4L * cost_A_stdev / cost_A * cost_CAS
cost_CAS_gamma <- GammaModVar$new(
  "Cost of CAS", "", cost_CAS ^ 2L / cost_CAS_stdev ^ 2L,
  cost_CAS_stdev ^ 2L / cost_CAS
)
cost_A_CAS <- ExprModVar$new(
  "Cost of state A with CAS", "", rlang::quo(cost_A_gamma + cost_CAS_gamma)
)
cost_E_CAS <- ExprModVar$new(
  "Cost of state E with CAS", "", rlang::quo(cost_E_gamma + cost_CAS_gamma)
)
cost_F_CAS <- ExprModVar$new(
  "Cost of state F with CAS", "", rlang::quo(cost_F_gamma + cost_CAS_gamma)
)

## @knitr cas-lognorm ---------------------------------------------------------
CAS_effect_mean <- 0.34
CAS_effect_sd <- 1.25
CAS_effect_lognorm <- LogNormModVar$new(
  "Effect of CAS", "", log(CAS_effect_mean), log(CAS_effect_sd)
)

## @knitr SMM-PSA -------------------------------------------------------------
# Markov states as Beta distributions
sA_PSA <- MarkovState$new(states["A"], utility = utility_A_beta)
sB_PSA <- MarkovState$new(states["B"], utility = utility_B_beta)
sC_PSA <- MarkovState$new(states["C"], utility = utility_C_beta)
sD_PSA <- MarkovState$new(states["D"], utility = utility_D_beta)
sE_PSA <- MarkovState$new(states["E"], utility = utility_E_beta)
sF_PSA <- MarkovState$new(states["F"], utility = utility_F_beta)
sG_PSA <- MarkovState$new(states["G"], utility = utility_G_beta)
sH_PSA <- MarkovState$new(states["H"], utility = utility_H_beta)
# state I has no uncertainty associated with it, so a probabilistic
# representation is not required

States_PSA <- list(
  sA_PSA, sB_PSA, sC_PSA, sD_PSA, sE_PSA, sF_PSA, sG_PSA, sH_PSA, sI
)

# Transition costs as Gamma distributions
tAD_PSA <- Transition$new(sA_PSA, sD_PSA, cost = cost_A_gamma)
tAC_PSA <- Transition$new(sA_PSA, sC_PSA, cost = cost_A_gamma)
tAB_PSA <- Transition$new(sA_PSA, sB_PSA, cost = cost_A_gamma)
tBC_PSA <- Transition$new(sB_PSA, sC_PSA)
tBE_PSA <- Transition$new(sB_PSA, sE_PSA, cost = cost_E_gamma)
tBF_PSA <- Transition$new(sB_PSA, sF_PSA, cost = cost_F_gamma)
tBG_PSA <- Transition$new(sB_PSA, sG_PSA, cost = cost_G_gamma)
tCB_PSA <- Transition$new(sC_PSA, sB_PSA)
tCD_PSA <- Transition$new(sC_PSA, sD_PSA)
tCF_PSA <- Transition$new(sC_PSA, sF_PSA, cost = cost_F_gamma)
tCG_PSA <- Transition$new(sC_PSA, sG_PSA, cost = cost_G_gamma)
tCC_PSA <- Transition$new(sC_PSA, sC_PSA)
tDC_PSA <- Transition$new(sD_PSA, sC_PSA)
tDB_PSA <- Transition$new(sD_PSA, sB_PSA)
tDD_PSA <- Transition$new(sD_PSA, sD_PSA)
tEB_PSA <- Transition$new(sE_PSA, sB_PSA)
tEH_PSA <- Transition$new(sE_PSA, sH_PSA)
tFB_PSA <- Transition$new(sF_PSA, sB_PSA)
tFC_PSA <- Transition$new(sF_PSA, sC_PSA)
tFG_PSA <- Transition$new(sF_PSA, sG_PSA, cost = cost_G_gamma)
tFH_PSA <- Transition$new(sF_PSA, sH_PSA)
tGB_PSA <- Transition$new(sG_PSA, sB_PSA)
tGC_PSA <- Transition$new(sG_PSA, sC_PSA)
tGF_PSA <- Transition$new(sG_PSA, sF_PSA, cost = cost_F_gamma)
tGD_PSA <- Transition$new(sG_PSA, sD_PSA)
tHE_PSA <- Transition$new(sH_PSA, sE_PSA, cost = cost_E_gamma)
tHF_PSA <- Transition$new(sH_PSA, sF_PSA, cost = cost_F_gamma)
tHH_PSA <- Transition$new(sH_PSA, sH_PSA)
tBI_PSA <- Transition$new(sB_PSA, sI)
tCI_PSA <- Transition$new(sC_PSA, sI)
tDI_PSA <- Transition$new(sD_PSA, sI)
tEI_PSA <- Transition$new(sE_PSA, sI)
tFI_PSA <- Transition$new(sF_PSA, sI)
tGI_PSA <- Transition$new(sG_PSA, sI)
tHI_PSA <- Transition$new(sH_PSA, sI)
# transition I to I also has no probabilistic elements

# Transitions incorporating CAS cost
tAD_CAS_PSA <- Transition$new(sA_PSA, sD_PSA, cost = cost_A_CAS)
tAC_CAS_PSA <- Transition$new(sA_PSA, sC_PSA, cost = cost_A_CAS)
tAB_CAS_PSA <- Transition$new(sA_PSA, sB_PSA, cost = cost_A_CAS)
tBE_CAS_PSA <- Transition$new(sB_PSA, sE_PSA, cost = cost_E_CAS)
tBF_CAS_PSA <- Transition$new(sB_PSA, sF_PSA, cost = cost_F_CAS)
tCF_CAS_PSA <- Transition$new(sC_PSA, sF_PSA, cost = cost_F_CAS)
tGF_CAS_PSA <- Transition$new(sG_PSA, sF_PSA, cost = cost_F_CAS)
tHE_CAS_PSA <- Transition$new(sH_PSA, sE_PSA, cost = cost_E_CAS)
tHF_CAS_PSA <- Transition$new(sH_PSA, sF_PSA, cost = cost_F_CAS)

Transitions_base_PSA <- list(
  tAD_PSA, tAC_PSA, tAB_PSA,
  tBC_PSA, tBE_PSA, tBF_PSA, tBG_PSA, tBI_PSA,
  tCB_PSA, tCD_PSA, tCF_PSA, tCG_PSA, tCC_PSA, tCI_PSA,
  tDC_PSA, tDB_PSA, tDD_PSA, tDI_PSA,
  tEB_PSA, tEH_PSA, tEI_PSA,
  tFB_PSA, tFC_PSA, tFG_PSA, tFH_PSA, tFI_PSA,
  tGB_PSA, tGC_PSA, tGF_PSA, tGD_PSA, tGI_PSA,
  tHE_PSA, tHF_PSA, tHH_PSA, tHI_PSA,
  tII
)

Transitions_CAS_PSA <- list(
  tAD_CAS_PSA, tAC_CAS_PSA, tAB_CAS_PSA,
  tBC_PSA, tBE_CAS_PSA, tBF_CAS_PSA, tBG_PSA, tBI_PSA,
  tCB_PSA, tCD_PSA, tCF_CAS_PSA, tCG_PSA, tCC_PSA, tCI_PSA,
  tDC_PSA, tDB_PSA, tDD_PSA, tDI_PSA,
  tEB_PSA, tEH_PSA, tEI_PSA,
  tFB_PSA, tFC_PSA, tFG_PSA, tFH_PSA, tFI_PSA,
  tGB_PSA, tGC_PSA, tGF_CAS_PSA, tGD_PSA, tGI_PSA,
  tHE_CAS_PSA, tHF_CAS_PSA, tHH_PSA, tHI_PSA,
  tII
)

SMM_base_PSA <- SemiMarkovModel$new(
  V = States_PSA, E = Transitions_base_PSA,
  tcycle = as.difftime(365.25 / 12L, units = "days"), # cycles in months
  discount.cost = 0.035,
  discount.utility = 0.035
)

SMM_CAS_PSA <- SemiMarkovModel$new(
  V = States_PSA, E = Transitions_CAS_PSA,
  tcycle = as.difftime(365.25 / 12L, units = "days"), # cycles in months
  discount.cost = 0.035,
  discount.utility = 0.035
)

## @knitr fun-dirichlet -------------------------------------------------------
dirichletify <- function(Pt, population = 1L) {
  # check argument
  stopifnot(
    is.matrix(Pt),
    is.numeric(Pt),
    nrow(Pt) == ncol(Pt),
    all(dimnames(Pt)[[2L]] == dimnames(Pt)[[1L]])
  )
  nNA <- rowSums(is.na(Pt))
  sumP <- rowSums(Pt, na.rm = TRUE)
  # check if a count or proportion representation
  is_count <- any(Pt > 1.0, na.rm = TRUE)
  if (is_count) {
    # counts cannot have NAs
    stopifnot(all(nNA == 0L))
    # normalise into proportions
    Pt <- Pt / rowSums(Pt)
  } else {
    # proportions can have NA values, but only 1/row
    stopifnot(all(nNA <= 1L))
    # store information about which variable was set as NA
    whichNA <- apply(Pt, 1L, function(row) which(is.na(row)))
    # populate missing values to a total of 1
    for (row in names(nNA)[nNA > 0L]) {
      Pt[row, whichNA[[row]]] <- 1.0 - sumP[row]
    }
  }
  # build state-wise Dirichlet distributions
  for (r in seq_len(nrow(Pt))) {
    non0 <- which(Pt[r, ] != 0.0)
    # if multiple outgoing transitions are possible, model as Dirichlet
    if (length(non0) > 1L) {
      dist <- DirichletDistribution$new(Pt[r, non0] * population)
      dist$sample() # randomise
      Pt[r, non0] <- dist$r()
    }
    # if only 1 transition is possible, leave as given originally
  }
  return(Pt)
}

## @knitr cycle-PSA -----------------------------------------------------------
nruns <- 250L
t4_CS_PSA <- array(
  dim = c(10L, ncol(t4_CS), nruns),
  dimnames = list(NULL, colnames(t4_CS), NULL)
)
t4_CAS_PSA <- array(
  dim = c(10L, ncol(t4_CS), nruns),
  dimnames = list(NULL, colnames(t4_CAS), NULL)
)
for (run in seq_len(nruns)) {
  # reset populations
  SMM_base_PSA$reset(populations)
  SMM_CAS_PSA$reset(populations)
  # find unique modvars and randomise them
  mv <- unique(c(
    SMM_base_PSA$modvars(),
    SMM_CAS_PSA$modvars(),
    CAS_effect_lognorm
  ))
  for (m in mv) {
    m$set("random")
  }
  # set the transition matrix, applying the CAS effect for CAS model
  pt_cs <- dirichletify(Pt, population = 1000L)
  SMM_base_PSA$set_probabilities(pt_cs)
  pt_cas <- txeffect(pt_cs, CAS_effect_lognorm$get())
  SMM_CAS_PSA$set_probabilities(pt_cas)
  # cycle the CS model
  mtrace <- SMM_base_PSA$cycles(
    ncycles = 120L, hcc.pop = FALSE, hcc.cost = FALSE
  )
  t4 <- as_table4(as.matrix(mtrace))
  t4_CS_PSA[, , run] <- t4
  # cycle the CAS model
  mtrace <- SMM_CAS_PSA$cycles(
    ncycles = 120L, hcc.pop = FALSE, hcc.cost = FALSE
  )
  t4 <- as_table4(as.matrix(mtrace))
  t4_CAS_PSA[, , run] <- t4
}

## @knitr ---------------------------------------------------------------------
test_that("Table 4 for conventional surgery is replicated by mean of PSA", {
  # skip on CRAN
  skip_on_cran()
  # Test that the range of the PSA estimates includes the expected value for
  # seven outcomes. This is a crude test of functionality. Statistical tests,
  # such as a one sample t-test, are not much help here because the shape of
  # the distribution of model results may be non-normal, and the type 1 error
  # rate is likely to be higher than suggested by the p-value.
  fields <- c(
    "Cumulative serious complication (%)",
    "Cumulative minor complication (%)",
    "Cumulative complex revision (%)",
    "Cumulative simple revision (%)",
    "Cumulative death (%)",
    "Discounted costs (£)",
    "Discounted QALYs"
  )
  for (f in fields) {
    r <- range(t4_CS_PSA[10L, f, ])
    e <- t4_CS[[10L, f]]
    expect_between(e, r[[1L]], r[[2L]])
  }
})

## @knitr ---------------------------------------------------------------------
test_that("Table 4 for computer-assisted surgery is replicated by mean PSA", {
  # skip on CRAN and set wide tolerance
  skip_on_cran()
  # Test that the range of the PSA estimates includes the expected value for
  # seven outcomes. This is a crude test of functionality. Statistical tests,
  # such as a one sample t-test, are not much help here because the shape of
  # the distribution of model results may be non-normal, and the type 1 error
  # rate is likely to be higher than suggested by the p-value.
  fields <- c(
    "Cumulative serious complication (%)",
    "Cumulative minor complication (%)",
    "Cumulative complex revision (%)",
    "Cumulative simple revision (%)",
    "Cumulative death (%)",
    "Discounted costs (£)",
    "Discounted QALYs"
  )
  for (f in fields) {
    r <- range(t4_CAS_PSA[10L, f, ])
    e <- t4_CAS[[10L, f]]
    expect_between(e, r[[1L]], r[[2L]])
  }
})

## @knitr t5-CS-PSA ----------------------------------------------------------
fields <- c(
  "Cumulative serious complication (%)",
  "Cumulative complex revision (%)",
  "Cumulative simple revision (%)"
)
t5_CS <- matrix(
  data = NA_real_, nrow = length(fields), ncol = 4L,
  dimnames = list(fields, c("Mean", "SD", "Q2.5", "Q97.5"))
)
for (f in fields) {
  t5_CS[[f, "Mean"]] <- mean(t4_CS_PSA[10L, f, ])
  t5_CS[[f, "SD"]] <- sd(t4_CS_PSA[10L, f, ])
  t5_CS[[f, "Q2.5"]] <- quantile(t4_CS_PSA[10L, f, ], probs = 0.025)
  t5_CS[[f, "Q97.5"]] <- quantile(t4_CS_PSA[10L, f, ], probs = 0.975)
}

## @knitr t5-CAS-PSA ----------------------------------------------------------
fields <- c(
  "Cumulative serious complication (%)",
  "Cumulative complex revision (%)",
  "Cumulative simple revision (%)"
)
t5_CAS <- matrix(
  data = NA_real_, nrow = length(fields), ncol = 4L,
  dimnames = list(fields, c("Mean", "SD", "Q2.5", "Q97.5"))
)
for (f in fields) {
  t5_CAS[[f, "Mean"]] <- mean(t4_CAS_PSA[10L, f, ])
  t5_CAS[[f, "SD"]] <- sd(t4_CAS_PSA[10L, f, ])
  t5_CAS[[f, "Q2.5"]] <- quantile(t4_CAS_PSA[10L, f, ], probs = 0.025)
  t5_CAS[[f, "Q97.5"]] <- quantile(t4_CAS_PSA[10L, f, ], probs = 0.975)
}

## @knitr cea-psa --------------------------------------------------------------
dcost_psa <- t4_CAS_PSA[10L, "Discounted costs (£)", ] / 1000L -
  t4_CS_PSA[10L, "Discounted costs (£)", ] / 1000L
dutil_psa <- t4_CAS_PSA[10L, "Discounted QALYs", ] / 1000L -
  t4_CS_PSA[10L, "Discounted QALYs", ] / 1000L
icer_psa <- dcost_psa / dutil_psa

## @knitr ---------------------------------------------------------------------
test_that("PSA CEA matches deterministic calculation", {
  r <- range(dcost_psa)
  expect_between(dcost, r[[1L]], r[[2L]])
  r <- range(dutil_psa)
  expect_between(dutil, r[[1L]], r[[2L]])
})
