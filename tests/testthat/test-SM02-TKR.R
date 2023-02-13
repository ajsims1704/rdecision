## @knitr state-names ---------------------------------------------------------
states <- c(
  "A" = "TKR operation for knee problems",
  "B" = "TKR with serious complications",
  "C" = "TKR with minor complications",
  "D" = "Normal health after primary TKR",
  "E" = "Complex revision",
  "F" = "Simple revision",
  "G" = "Other treatments",
  "H" = "Normal health after TKR revision",
  "I" = "Death"
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
  tGD, tHE, tHF, tHH, tBI, tCI, tDI, tEI, tFI, tGI, tHI, tII)

Transitions_CAS <- list(
  tAD_CAS, tAC_CAS, tAB_CAS, tBC, tBE_CAS, tBF_CAS, tBG, tCB, tCD, tCF_CAS, 
  tCG, tCC, tDC, tDB, tDD, tEB, tEH, tFB, tFC, tFG, tFH, tGB, tGC, tGF_CAS, 
  tGD, tHE_CAS, tHF_CAS, tHH, tBI, tCI, tDI, tEI, tFI, tGI, tHI, tII)


## @knitr SMM-def-point --------------------------------------------------------
SMM_base <- SemiMarkovModel$new(
  V = States, E = Transitions_base,
  tcycle = as.difftime(365.25/12L, units = "days"), # cycles expressed in months
  discount.cost = 0.035,
  discount.utility = 0.035
)

SMM_CAS <- SemiMarkovModel$new(
  V = States, E = Transitions_CAS,
  tcycle = as.difftime(365.25/12L, units = "days"), # cycles expressed in months
  discount.cost = 0.035,
  discount.utility = 0.035
)






