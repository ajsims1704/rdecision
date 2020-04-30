## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F-------------------------------------------------------
library(rdecision)

## ----echo=T--------------------------------------------------------------
# model parameters
nStates <- 3
state <- data.frame(
  name = c("Well", "Disabled", "Dead"),
  hasCycleLimit = c(FALSE, FALSE, FALSE),
  cycleLimit = c(NA, NA, NA),
  annualCost = c(0, 0, 0),
  entryCost = c(0, 0, 0),
  utility = c(1.0, 0.7, 0.0),
  group = c(0,0,0)
)
prevalence <- c(1.0, 0.0, 0.0)
incidence <- matrix(c(c(NA,  0.2, 0.2),
                      c(0.0,  NA, 0.4),
                      c(0.0, 0.0,  NA)),
                    nrow=nStates, byrow=TRUE
)
Tp <- matrix(rep(0, nStates*nStates), nrow=nStates, byrow=TRUE)
discount <- 0.0

## ----echo=T--------------------------------------------------------------
# simulation parameters
nPatients <- 10000
nCyclesPerYear <- 1
nYears <- 24
nCycles <- nYears*nCyclesPerYear

## ----echo=T--------------------------------------------------------------
# solve the model using Monte-Carlo method...
ms <- des(nStates=nStates, 
          nGroups = 0,
          nPatients = nPatients,
          nCyclesPerYear = nCyclesPerYear, 
          nCycles = nCycles,
          state = state, 
          group = NA,
          prevalence = prevalence, 
          Ip = incidence, 
          Tp = Tp, 
          Gp = NA,
          discount = discount,
          stub=NA)

## ----echo=F--------------------------------------------------------------
# convert cycle matrices to a data frame for display
POP <- as.data.frame(ms[[1]])
CST <- as.data.frame(ms[[2]])
UTL <- as.data.frame(ms[[4]])
DF <- cbind(
  POP,
  'Utility' = UTL$Total
)
knitr::kable(DF)

