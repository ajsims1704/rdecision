## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F-------------------------------------------------------
library(rdecision)

## ----echo=T--------------------------------------------------------------
# Sumatriptan branch
leaf.a <- LeafNode$new("A", utility=1.0)
leaf.b <- LeafNode$new("B", utility=0.9)
leaf.c <- LeafNode$new("C", utility=-0.3)
leaf.d <- LeafNode$new("D", utility=0.1)
leaf.e <- LeafNode$new("E", utility=-0.3)

c.8 <- ChanceNode$new(
  p = list(0.998, 0.002),
  children = list(leaf.d, leaf.e),
  edgelabels = list("Relief", "Hospitalization"),
  costs = list(0, 1093.0)
)

c.4 <- ChanceNode$new(
  p = list(0.594, 0.406),
  children = list(leaf.a, leaf.b),
  edgelabels = list("No recurrence", "Recurrence relieved with 2nd dose"),
  costs = list(0, 16.10)
)

c.5 <- ChanceNode$new(
  p = list(0.920, 0.080),
  children = list(leaf.c, c.8),
  edgelabels = list("Endures attack", "ER"),
  costs = list(0, 63.16)
)

c.2 <- ChanceNode$new(
  p = list(0.558, 0.442),
  children = list(c.4, c.5),
  edgelabels = list("Relief", "No relief"),
  costs = list(0, 0)
)

# Caffeine/Ergotamine branch
leaf.f <- LeafNode$new("F", utility=1.0)
leaf.g <- LeafNode$new("G", utility=0.9)
leaf.h <- LeafNode$new("H", utility=-0.3)
leaf.i <- LeafNode$new("I", utility=0.1)
leaf.j <- LeafNode$new("J", utility=-0.3)

c.9 <- ChanceNode$new(
  p = list(0.998, 0.002),
  children = list(leaf.i, leaf.j),
  edgelabels = list("Relief", "Hospitalization"),
  costs = list(0, 1093.0)
)

c.6 <- ChanceNode$new(
  p = list(0.703, 0.297),
  children = list(leaf.f, leaf.g),
  edgelabels = list("No recurrence", "Recurrence relieved with 2nd dose"),
  costs = list(0, 1.32)
)

c.7 <- ChanceNode$new(
  p = list(0.920, 0.080),
  children = list(leaf.h, c.9),
  edgelabels = list("Endures attack", "ER"),
  costs = list(0, 63.13)
)

c.3 <- ChanceNode$new(
  p = list(0.379, 0.621),
  children = list(c.6, c.7),
  edgelabels = list("Relief", "No relief"),
  costs = list(0, 0)
)

# decision node
d.1 <- DecisionNode$new(
  children = list(c.2, c.3),
  edgelabels = list("Sumatriptan", "Caffeine/Ergotamine"),
  costs = list(16.10, 1.32)
)

## ----echo=FALSE----------------------------------------------------------
local({
  RES <- d.1$evaluatePathways()
  names(RES) <- c('Choice', 'Pathway', 'Probability', 'Cost', 'Utility',
                  'Expected Cost', 'Expected Utility')
  keep <- c('Choice', 'Pathway', 'Probability', 'Cost', 'Expected Cost',
            'Utility', 'Expected Utility')
  knitr::kable(RES[,keep], row.names=F, digits=c(NA,NA,3,2,2,2,5))
})

## ----echo=F--------------------------------------------------------------
local({
  SUM <- d.1$evaluateChoices()
  names(SUM) <- c('Run', 'Choice', 'Expected Cost', 'Expected Utility')
  keep <- c('Choice', 'Expected Cost', 'Expected Utility')
  knitr::kable(SUM[,keep], row.names=F, digits=c(NA,2,5))
})

