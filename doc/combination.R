## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F-------------------------------------------------------
library(rdecision)

## ----echo=T--------------------------------------------------------------
# create Markov states for monotherapy (zidovudine only)
state.mono.A <- MarkovState$new("A", 1701+1055+2278)
state.mono.B <- MarkovState$new("B", 1774+1278+2278)
state.mono.C <- MarkovState$new("C", 6948+2059+2278)
state.mono.D <- MarkovState$new("D", 0)
# transition matrix for monotherapy
I.mono <- matrix(
  data = c(0.721, 0.202, 0.067, 0.010,
           0.000, 0.581, 0.407, 0.012,
           0.000, 0.000, 0.750, 0.250,
           0.000, 0.000, 0.000, 1.000),
  nrow = 4,
  ncol = 4, 
  byrow = T,
  dimnames = list(c('A', 'B', 'C', 'D'), c('A', 'B', 'C', 'D'))
)
# construct the model
m.mono <- MarkovModel$new(
  states=list(state.mono.A, state.mono.B, state.mono.C, state.mono.D),
  Ip=I.mono,
  discount=6.0
)

## ----echo=T--------------------------------------------------------------
model.states <- m.mono$stateSummary()

## ----echo=F--------------------------------------------------------------
knitr::kable(model.states)
rm(model.states)

## ----echo=T--------------------------------------------------------------
transition.matrix <- m.mono$transitionSummary()

## ----echo=F--------------------------------------------------------------
knitr::kable(transition.matrix)
rm(transition.matrix)

## ----echo=T--------------------------------------------------------------
# create starting populations
populations <- c('A'=1000, 'B'=0, 'C'=0, 'D'=0)
m.mono$setPopulations(populations)
# run the model
DF <- m.mono$cycle()

## ----echo=F--------------------------------------------------------------
knitr::kable(DF)
rm(DF)

## ----echo=T--------------------------------------------------------------
# create starting populations
N <- 1000
populations <- c('A'=N, 'B'=0, 'C'=0, 'D'=0)
m.mono$setPopulations(populations)
# run 20 cycles
DF.mono <- m.mono$cycles(nCycles=20+1)
# calculate the proportion alive at each cycle
DF.mono$Alive <- (DF.mono$A + DF.mono$B + DF.mono$C)/N

## ----echo=F--------------------------------------------------------------
knitr::kable(DF.mono)

## ----echo=T--------------------------------------------------------------
# create Markov states for combination therapy (zidovudine and lamivudine)
state.comb.A <- MarkovState$new("A", 1701+1055+2278+2086)
state.comb.B <- MarkovState$new("B", 1774+1278+2278+2086)
state.comb.C <- MarkovState$new("C", 6948+2059+2278+2086)
state.comb.D <- MarkovState$new("D", 0)
# transition matrix for combination therapy
I.comb <- matrix(
  data = c(0.858, 0.103, 0.034, 0.005,
           0.000, 0.787, 0.207, 0.006,
           0.000, 0.000, 0.873, 0.127,
           0.000, 0.000, 0.000, 1.000),
  nrow = 4,
  ncol = 4, 
  byrow = T,
  dimnames = list(c('A', 'B', 'C', 'D'), c('A', 'B', 'C', 'D'))
)
# construct the model
m.comb <- MarkovModel$new(
  states = list(state.comb.A, state.comb.B, state.comb.C, state.comb.D),
  Ip = I.comb,
  discount = 6.0
)

## ----echo=T--------------------------------------------------------------
# run combination therapy model for 2 years
N <- 1000
populations <- c('A'=N, 'B'=0, 'C'=0, 'D'=0)
m.comb$setPopulations(populations)
DF.comb <- m.comb$cycles(nCycles=2+1)
# revise costs and transitions, and run model for next 18 years
state.comb.A$setAnnualCost(1701+1055+2278)
state.comb.B$setAnnualCost(1774+1278+2278)
state.comb.C$setAnnualCost(6948+2059+2278)
m.comb$setTransitions(I.mono)
DF.comb <- rbind(DF.comb, m.comb$cycles(nCycles=18))
# calculate the proportion alive at end of each cycle
DF.comb$Alive <- (DF.comb$A + DF.comb$B + DF.comb$C)/N

## ----echo=F--------------------------------------------------------------
knitr::kable(DF.comb)

## ----echo=F--------------------------------------------------------------
el.mono <- sum(DF.mono$Alive)
el.comb <- sum(DF.comb$Alive)
cost.mono <- sum(DF.mono$Cost)
cost.comb <- sum(DF.comb$Cost)
icer <- (cost.comb-cost.mono)/(el.comb-el.mono)

