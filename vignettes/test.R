rm(list=ls())

source("rdecision-dev.R")


f <- function() {
  x <- 42
  y <- 94
  exp1 <- quote(x+y)
  z <- eval(exp1)
  print(paste('z =',z))
  
  mva <- NormalModelVariable$new('mva', 'A', '', mu=0, sigma=1) 
#  print(mva$getOperands())
  mvb <- NormalModelVariable$new('mvb', 'B', '', mu=0, sigma=1) 
  mv1 <- ExpressionModelVariable$new('mv1', '1', '', quote(mva*mvb), envir=environment()) 
#  print(mv1$getOperands())
  mv2 <- NormalModelVariable$new('mv2', '2', '' , mu=0, sigma=1)
  mv3 <- ExpressionModelVariable$new('mv3', '3', '', quote(mv1+mv2), envir=environment())
  print(mv3$getOperands())
  
  
}

f()



