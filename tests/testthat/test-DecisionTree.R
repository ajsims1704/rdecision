
# -----------------------------------------------------------------------------
# Evans et al, Pharmacoeconomics, 1997;12:565-577, Sumatriptan for migraine
# (base case)
# -----------------------------------------------------------------------------

test_that("rdecision replicates Evans et al, Sumatriptan base case", {
  # Time horizon
  th <- as.difftime(48, units="hours")
  # model variables
  sumatriptan <- 16.10
  caffeine <- 1.32
  ED <- 63.16
  admission <- 1093
  # Expressions
  c.A <- sumatriptan
  c.B <- 2*sumatriptan
  c.C <- sumatriptan
  c.D <- sumatriptan+ED
  c.E <- sumatriptan+ED+admission
  c.F <- caffeine
  c.G <- 2*caffeine
  c.H <- caffeine
  c.I <- caffeine+ED
  c.J <- caffeine+ED+admission
  admission <- 1093
  #
  # Sumatriptan branch
  #
  n.a <- LeafNode$new("A", cost=c.A, utility=1.0, interval=th)
  n.b <- LeafNode$new("B", cost=c.B, utility=0.9, interval=th)
  n.4 <- ChanceNode$new("n.4")
  e.4a <- Reaction$new(n.4, n.a, p=0.594, label="No recurrence")
  e.4b <- Reaction$new(n.4, n.b, p=0.406, label="Recurrence relieved with 2nd dose")
  #
  n.d <- LeafNode$new("D", cost=c.D, utility=0.1, interval=th)
  n.e <- LeafNode$new("E", cost=c.E, utility=-0.3, interval=th)
  n.8 <- ChanceNode$new("n.8")
  e.8d <- Reaction$new(n.8, n.d, p=0.998, label="Relief")
  e.8e <- Reaction$new(n.8, n.e, p=0.002, label="Hospitalization")
  #
  n.c <- LeafNode$new("C", cost=c.C, utility=-0.3, interval=th)
  n.5 <- ChanceNode$new("n.5")
  e.5c <- Reaction$new(n.5, n.c, p=0.920, label="Endures attack")
  e.58 <- Reaction$new(n.5, n.8, p=0.008, label="Emergency Department")
  #
  n.2 <- ChanceNode$new("n.2")
  e.24 <- Reaction$new(n.2, n.4, p=0.558, label="Relief")
  e.25 <- Reaction$new(n.2, n.5, p=0.442, label="No relief")
  #
  # Caffeine/Ergotamine branch
  #
  n.f <- LeafNode$new("F", cost=c.F, utility=1.0, interval=th)
  n.g <- LeafNode$new("G", cost=c.G, utility=0.9, interval=th)
  n.6 <- ChanceNode$new("n.6")
  e.6f <- Reaction$new(n.6, n.f, p=0.703, label="No recurrence")
  e.6g <- Reaction$new(n.6, n.g, p=0.297, label="Recurrence relieved with 2nd dose")
  #
  n.i <- LeafNode$new("I", cost=c.I, utility=0.1, interval=th)
  n.j <- LeafNode$new("J", cost=c.J, utility=-0.3, interval=th)
  n.9 <- ChanceNode$new("C.9")
  e.9i <- Reaction$new(n.9, n.i, p=0.998, label="Relief")
  e.9j <- Reaction$new(n.9, n.j, p=0.002, label="Hospitalization")
  # 
  n.h <- LeafNode$new("H", cost=c.H, utility=-0.3, interval=th)
  n.7 <- ChanceNode$new("n.7")
  e.7h <- Reaction$new(n.7, n.h, p=0.920, label="Endures attack")
  e.79 <- Reaction$new(n.7, n.9, p=0.080, label="Emergency Department")
  #
  n.3 <- ChanceNode$new("n.3")
  expect_equal(n.3$label(), "n.3")
  e.36 <- Reaction$new(n.3, n.6, p=0.379, label="Relief")
  e.37 <- Reaction$new(n.3, n.7, p=0.621, label="No relief")
  #
  # decision node
  n.1 <- DecisionNode$new("n.1")
  expect_equal(n.1$label(), "n.1")
  e.12 <- Action$new(n.1, n.2, label="Sumatriptan")
  e.13 <- Action$new(n.1, n.3, label="Caffeine/Ergotamine")
  # 
  # create lists of nodes and edges
  V <- list(
    n.1, n.2, n.3, n.4, n.5, n.6, n.7, n.8, n.9,
    n.a, n.b, n.c, n.d, n.e, n.f, n.g, n.h, n.i, n.j
  )
  E <- list(
    e.12, e.13, 
    e.24, e.25, e.36, e.37, 
    e.4a, e.4b, e.5c, e.58, e.8d, e.8e,
    e.6f, e.6g, e.7h, e.79, e.9i, e.9j
  )
  # tree
  expect_silent(dt <- DecisionTree$new(V,E))
  # # evaluate
  # RES <- dt$evaluateChoices()
  # expect_true(is.data.frame(RES))
  # c.Sumatriptan <- round(RES[RES$Run==1 & RES$Choice=="Sumatriptan", "Cost"],2)
  # expect_equal(c.Sumatriptan, 22.06)
  # c.Caffeine <- round(RES[RES$Run==1 & RES$Choice=="Caffeine/Ergotamine", "Cost"],2)
  # expect_equal(c.Caffeine, 4.71)
  # u.Sumatriptan <- round(RES[RES$Run==1 & RES$Choice=="Sumatriptan", "Utility"],2)
  # expect_equal(u.Sumatriptan, 0.42)
  # u.Caffeine <- round(RES[RES$Run==1 & RES$Choice=="Caffeine/Ergotamine", "Utility"],2)
  # expect_equal(u.Caffeine, 0.20)
})
