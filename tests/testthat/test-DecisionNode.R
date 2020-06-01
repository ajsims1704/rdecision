
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
  state.a <- State$new("A", cost=c.A, utility=1.0, interval=th)
  state.b <- State$new("B", cost=c.B, utility=0.9, interval=th)
  state.c <- State$new("C", cost=c.C, utility=-0.3, interval=th)
  state.d <- State$new("D", cost=c.D, utility=0.1, interval=th)
  state.e <- State$new("E", cost=c.E, utility=-0.3, interval=th)
  c.8 <- ChanceNode$new(
    p = list(0.998, 0.002),
    children = list(state.d, state.e),
    edgelabels = list("Relief", "Hospitalization")
  )
  c.4 <- ChanceNode$new(
    p = list(0.594, 0.406),
    children = list(state.a, state.b),
    edgelabels = list("No recurrence", "Recurrence relieved with 2nd dose")
  )
  c.5 <- ChanceNode$new(
    p = list(0.920, 0.080),
    children = list(state.c, c.8),
    edgelabels = list("Endures attack", "ER")
  )
  c.2 <- ChanceNode$new(
    p = list(0.558, 0.442),
    children = list(c.4, c.5),
    edgelabels = list("Relief", "No relief")
  )
  # Caffeine/Ergotamine branch
  state.f <- State$new("F", cost=c.F, utility=1.0, interval=th)
  state.g <- State$new("G", cost=c.G, utility=0.9, interval=th)
  state.h <- State$new("H", cost=c.H, utility=-0.3, interval=th)
  state.i <- State$new("I", cost=c.I, utility=0.1, interval=th)
  state.j <- State$new("J", cost=c.J, utility=-0.3, interval=th)
  c.9 <- ChanceNode$new(
    p = list(0.998, 0.002),
    children = list(state.i, state.j),
    edgelabels = list("Relief", "Hospitalization")
  )
  c.6 <- ChanceNode$new(
    p = list(0.703, 0.297),
    children = list(state.f, state.g),
    edgelabels = list("No recurrence", "Recurrence relieved with 2nd dose")
  )
  c.7 <- ChanceNode$new(
    p = list(0.920, 0.080),
    children = list(state.h, c.9),
    edgelabels = list("Endures attack", "ER")
  )
  c.3 <- ChanceNode$new(
    p = list(0.379, 0.621),
    children = list(c.6, c.7),
    edgelabels = list("Relief", "No relief")
  )
  # decision node
  d <- DecisionNode$new(
    children = list(c.2, c.3),
    edgelabels = list("Sumatriptan", "Caffeine/Ergotamine")
  )
  # evaluate
  RES <- d$evaluateChoices()
  expect_true(is.data.frame(RES))
  c.Sumatriptan <- round(RES[RES$Run==1 & RES$Choice=="Sumatriptan", "Cost"],2)
  expect_equal(c.Sumatriptan, 22.06)
  c.Caffeine <- round(RES[RES$Run==1 & RES$Choice=="Caffeine/Ergotamine", "Cost"],2)
  expect_equal(c.Caffeine, 4.71)
  u.Sumatriptan <- round(RES[RES$Run==1 & RES$Choice=="Sumatriptan", "Utility"],2)
  expect_equal(u.Sumatriptan, 0.42)
  u.Caffeine <- round(RES[RES$Run==1 & RES$Choice=="Caffeine/Ergotamine", "Utility"],2)
  expect_equal(u.Caffeine, 0.20)
})