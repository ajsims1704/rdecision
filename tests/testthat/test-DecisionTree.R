
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
  # Sumatriptan branch
  leaf.a <- LeafNode$new("A", cost=c.A, utility=1.0, interval=th)
  leaf.b <- LeafNode$new("B", cost=c.B, utility=0.9, interval=th)
  c.4 <- ChanceNode$new("c.4")

  #   p = list(0.594, 0.406),
  #   children = list(state.a, state.b),
  #   edgelabels = list("No recurrence", "Recurrence relieved with 2nd dose")
  # )
  leaf.c <- LeafNode$new("C", cost=c.C, utility=-0.3, interval=th)
  leaf.d <- LeafNode$new("D", cost=c.D, utility=0.1, interval=th)
  leaf.e <- LeafNode$new("E", cost=c.E, utility=-0.3, interval=th)
  expect_equal(leaf.e$label(), "E")
  c.8 <- ChanceNode$new("c.8")
  #   p = list(0.998, 0.002),
  #   children = list(state.d, state.e),
  #   edgelabels = list("Relief", "Hospitalization")
  # )
  c.5 <- ChanceNode$new("c.5")
  #   p = list(0.920, 0.080),
  #   children = list(state.c, c.8),
  #   edgelabels = list("Endures attack", "ER")
  # )
  c.2 <- ChanceNode$new("c.2")
  #   p = list(0.558, 0.442),
  #   children = list(c.4, c.5),
  #   edgelabels = list("Relief", "No relief")
  # )
  # Caffeine/Ergotamine branch
  leaf.f <- LeafNode$new("F", cost=c.F, utility=1.0, interval=th)
  leaf.g <- LeafNode$new("G", cost=c.G, utility=0.9, interval=th)
  leaf.h <- LeafNode$new("H", cost=c.H, utility=-0.3, interval=th)
  leaf.i <- LeafNode$new("I", cost=c.I, utility=0.1, interval=th)
  leaf.j <- LeafNode$new("J", cost=c.J, utility=-0.3, interval=th)
  c.9 <- ChanceNode$new("C.9")
  #   p = list(0.998, 0.002),
  #   children = list(state.i, state.j),
  #   edgelabels = list("Relief", "Hospitalization")
  # )
  c.6 <- ChanceNode$new("c.6")
  #   p = list(0.703, 0.297),
  #   children = list(state.f, state.g),
  #   edgelabels = list("No recurrence", "Recurrence relieved with 2nd dose")
  # )
  c.7 <- ChanceNode$new("c.7")
  #   p = list(0.920, 0.080),
  #   children = list(state.h, c.9),
  #   edgelabels = list("Endures attack", "ER")
  # )
  c.3 <- ChanceNode$new("c.3")
  expect_equal(c.3$label(), "c.3")
  #   p = list(0.379, 0.621),
  #   children = list(c.6, c.7),
  #   edgelabels = list("Relief", "No relief")
  # )
  # decision node
  d <- DecisionNode$new("d")
  expect_equal(d$label(), "d")
  e.17 <- Action$new(d,c.2)
  e.18 <- Action$new(d,c.3)
  
  #   children = list(c.2, c.3),
  #   choices = list("Sumatriptan", "Caffeine/Ergotamine")
  # )
  # create lists of nodes and edges
  V <- list(
    d,
#    leaf.a, leaf.b, leaf.c, leaf.d, leaf.e,
    c.2,  #c.8, c.4, c.5, c.2,
#    leaf.f, leaf.g, leaf.h, leaf.i, leaf.j,
    c.3 #c.9, c.6, c.7, c.3
  )
  E <- list(
    e.17, e.18
  )
  
  # tree
  dt <- DecisionTree$new(V,E)
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
