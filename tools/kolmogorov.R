rm(list=ls())
#library("expm")


# Sonneberg and Beck, 3 state, forward only
# =========================================
# matrix order
n <- 3
# given Pt, calculate Q
IPt <- matrix(c(0.6, 0.2, 0.2, 0, 0.6, 0.4, 0, 0, 1), nrow=n, byrow=TRUE)
t <- 1
Q <- expm::logm(IPt)/1
print(Q)

# calculate Pt using method of Jones et al
OPt <- matrix(NA, nrow=n, ncol=n)
OPt[1,1] <- exp(Q[1,1]*t)
OPt[1,2] <- Q[1,2]*(exp(Q[1,1]*t - exp(Q[2,3]*t)))/(Q[1,1]+Q[2,3])
OPt[1,3] <- 1 - OPt[1,1] - OPt[1,2]
OPt[2,1] <- 0
OPt[2,2] <- exp(-Q[2,3]*t)
OPt[2,3] <- 1 - exp(-Q[2,3]*t)
OPt[3,1] <- 0
OPt[3,2] <- 0
OPt[3,3] <- 1
print(OPt)

