rm(list=ls())

source("rdecision-dev.R")

N1 <- NormalModelVariable$new('N1', 'Norm N1', 'GBP', mu=0, sigma=1)
N2 <- NormalModelVariable$new('N2', 'Norm N2', 'GBP', mu=0, sigma=1)

E1 <- ExpressionModelVariable$new('E1', 'Exp 1', 'GBP', quote(4+N1))
E2 <- ExpressionModelVariable$new('E2', 'Exp 2', 'GBP', quote(N1+N2))
ED <- ExpressionModelVariable$new('ED', 'Diff', 'GBP', quote(E1-E2))

leaf.A <- LeafNode$new('Leaf A')
leaf.B <- LeafNode$new('Leaf B')
d <- DecisionNode$new(
  c(leaf.A, leaf.B),
  c('A', 'B'),
  c(E1, E2)
)


N <- 1000

sE1 <- E1$r(N)
sE2 <- E2$r(N)
sC <- ED$r(N)
sU <- sE1 - sE2

sD <- vector(mode='numeric', length=N)
for (i in 1:N) {
  RES <- d$evaluateChoices(expected=F)
  D <- RES$Cost[RES$Choice=='A'] - RES$Cost[RES$Choice=='B']
  sD[i] <- D
}

print(paste('mean sC=', mean(sC)))
print(paste('sd sC=', sd(sC)))
print(paste('mean sU=', mean(sU)))
print(paste('sd sU=', sd(sU)))
print(paste('mean sD=', mean(sD)))
print(paste('sd sD=', sd(sD)))





