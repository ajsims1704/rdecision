
# -----------------------------------------------------------------------------
# tests of creating a model
# -----------------------------------------------------------------------------
test_that("incorrect state types are rejected", {
  s.well <- MarkovState$new("WELL")
  expect_error(
    SemiMarkovModel$new(
      V=list(s.well, "DISABLED", "STROKE", "DEAD"), 
      E=list()
    ), 
    class="non-Node_vertex"
  )  
  n1 <- Node$new()
  e.ww <- Transition$new(s.well, s.well)
  expect_error(
    SemiMarkovModel$new(
      V=list(s.well, n1), 
      E=list(e.ww)
    ), 
    class="invalid_state"
  )  
})

test_that("incorrect transition types are rejected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- Transition$new(s.well, s.well)
  e.wd <- Transition$new(s.well, s.dead)
  e.dd <- Arrow$new(s.dead, s.dead)
  expect_error(
    SemiMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.wd, e.dd)
    ), 
    class="invalid_transition"
  )  
})

test_that("edge case graphs are rejected", {
  # empty graph
  expect_error(
    SemiMarkovModel$new(V=list(), E=list()),
    class = "invalid_graph"
  )
  # single node, no edges
  s.dead <- MarkovState$new("Dead")
  expect_error(
    SemiMarkovModel$new(V=list(s.dead), E=list()),
    class = "invalid_graph"
  )
  # minimal model
  e.dd <- Transition$new(s.dead, s.dead)
  expect_silent(SemiMarkovModel$new(V=list(s.dead),E=list(e.dd)))
})

test_that("multiple digraph edges are rejected", {
  s.well <- MarkovState$new("Well")
  s.dead <- MarkovState$new("Dead")
  e.ww <- Transition$new(s.well, s.well)
  e.ww.bleed <- Transition$new(s.well, s.well)
  e.wd <- Transition$new(s.well, s.dead)
  e.dd <- Transition$new(s.dead, s.dead)
  # two self loops from well to well
  expect_error(
    SemiMarkovModel$new(
      V = list(s.well, s.dead), 
      E = list(e.ww, e.ww.bleed, e.wd, e.dd)
    ),
    class = "multiple_edges"
  )
  # two loops from well to dead
  e.wd.bleed <- Transition$new(s.well, s.dead)
  expect_error(
    SemiMarkovModel$new(
      V = list(s.well, s.dead), 
      E = list(e.ww, e.wd.bleed, e.wd, e.dd)
    ),
    class = "multiple_edges"
  )
  # multiple edges for graph but not digraph are allowed
  e.dw <- Transition$new(s.dead, s.well)
  expect_silent(
    SemiMarkovModel$new(
      V = list(s.well, s.dead), 
      E = list(e.ww, e.wd, e.dw, e.dd)
    )
  )
})

test_that("unconnected underlying graphs are detected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- Transition$new(s.well, s.well)
  e.wd <- Transition$new(s.well, s.dead)
  e.dd <- Transition$new(s.dead, s.dead)
  expect_error(
    SemiMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.wd, e.dd)
    ), 
    class="invalid_graph"
  )  
})

test_that("non-unique state labels are detected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Well")
  s.dead <- MarkovState$new("Dead")
  e.ww <- Transition$new(s.well, s.well)
  e.wd <- Transition$new(s.well, s.dead)
  e.ws <- Transition$new(s.well, s.disabled)
  e.ss <- Transition$new(s.disabled, s.disabled)
  e.sd <- Transition$new(s.disabled, s.dead)
  e.dd <- Transition$new(s.dead, s.dead)
  expect_error(
    SemiMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.wd, e.ws, e.ss, e.sd, e.dd)
    ), 
    class="invalid_state_names"
  )  
})  

test_that("invalid discount rates are detected", {
  s.well <- MarkovState$new("Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- Transition$new(s.well, s.well)
  e.wd <- Transition$new(s.well, s.dead)
  e.ws <- Transition$new(s.well, s.disabled)
  e.sd <- Transition$new(s.disabled, s.dead)
  e.ss <- Transition$new(s.disabled, s.disabled)
  e.dd <- Transition$new(s.dead, s.dead)
  expect_error(
    SemiMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.wd, e.ws, e.ss, e.sd, e.dd),
      discount.cost = "0"
    ), 
    class="invalid_discount"
  )  
  expect_error(
    SemiMarkovModel$new(
      V = list(s.well, s.disabled, s.dead),
      E = list(e.ww, e.wd, e.ws, e.ss, e.sd, e.dd),
      discount.utility = "0"
    ), 
    class="invalid_discount"
  )  
})

# -----------------------------------------------------------------------------
# tests of resetting the model
# -----------------------------------------------------------------------------
test_that("invalid population vectors are rejected", {
  # create the model
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- Transition$new(s.well, s.well)
  e.ss <- Transition$new(s.disabled, s.disabled)
  e.dd <- Transition$new(s.dead, s.dead)
  e.ws <- Transition$new(s.well, s.disabled)
  e.wd <- Transition$new(s.well, s.dead)
  e.sd <- Transition$new(s.disabled, s.dead)
  M <- SemiMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
  )
  # check state names
  expect_setequal(M$get_statenames(), list("Well", "Disabled", "Dead"))
  # check default population
  rp <- M$get_populations()
  expect_equal(unname(rp[1]),1000)
  expect_equal(unname(rp[2]),0)
  expect_equal(unname(rp[3]),0)
  # number of elements
  pop <- c(Well=10000, Disabled=0)
  expect_error(M$reset(pop), class="incorrect_state_count")
  # state names
  pop <- c(Well=10000, Poorly=0, Disabled=0)
  expect_error(M$reset(pop), class="unmatched_states")
  pop <- c(10000, 0, 0)
  expect_error(M$reset(pop), class="unmatched_states")
  # type
  pop <- c(Well=10000, Disabled="0", Dead=0)
  expect_error(M$reset(pop), class="non-numeric_state_population")
  # correct
  pop <- c(Well=10000, Disabled=0, Dead=0)
  expect_silent(M$reset(pop))
  rp <- M$get_populations()
  expect_equal(unname(rp["Well"]), 10000)
  expect_equal(unname(rp["Disabled"]), 0)
  expect_equal(unname(rp["Dead"]), 0)
})

test_that("invalid cycle numbers are rejected", {
  # create the model
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- Transition$new(s.well, s.well)
  e.ss <- Transition$new(s.disabled, s.disabled)
  e.dd <- Transition$new(s.dead, s.dead)
  e.ws <- Transition$new(s.well, s.disabled)
  e.wd <- Transition$new(s.well, s.dead)
  e.sd <- Transition$new(s.disabled, s.dead)
  M <- SemiMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
  )
  # attempt to reset with illegal cycle numbers
  expect_error(M$reset(icycle=2), class="invalid_icycle")
  expect_error(M$reset(icycle="2"), class="invalid_icycle")
  expect_error(M$reset(icycle=as.integer(-1)), class="invalid_icycle")
})

test_that("invalid elapsed times are rejected", {
  # create the model
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  e.ww <- Transition$new(s.well, s.well)
  e.ss <- Transition$new(s.disabled, s.disabled)
  e.dd <- Transition$new(s.dead, s.dead)
  e.ws <- Transition$new(s.well, s.disabled)
  e.wd <- Transition$new(s.well, s.dead)
  e.sd <- Transition$new(s.disabled, s.dead)
  M <- SemiMarkovModel$new(
    V = list(s.well, s.disabled, s.dead),
    E = list(e.ww, e.ss, e.dd, e.ws, e.wd, e.sd)
  )
  # attempt to reset with illegal elapsed times
  expect_error(M$reset(elapsed=2), class="invalid_elapsed")
  expect_error(M$reset(elapsed="2"), class="invalid_elapsed")
  expect_error(
    M$reset(icycle=as.difftime(-1, units="days")), 
    class="invalid_icycle"
  )
})

# -----------------------------------------------------------------------------
# tests of cycling
# -----------------------------------------------------------------------------
test_that("model is cyclable", {
  # create states
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new(name="Disabled")
  s.dead <- MarkovState$new(name="Dead")
  # use S&B per-cycle transition probabilities and calculate rates
  snames <- c("Well","Disabled","Dead")
  Pt <- matrix(
    data = c(0.6, 0.2, 0.2, 0, 0.6, 0.4, 0, 0, 1),
    nrow = 3, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  # create transitions
  E <- list(
    Transition$new(s.well, s.well),
    Transition$new(s.dead, s.dead),
    Transition$new(s.disabled, s.disabled),
    Transition$new(s.well, s.disabled),
    Transition$new(s.well, s.dead),
    Transition$new(s.disabled, s.dead)
  )
  # detect illegal parameters to cycle()
  expect_error(
    SemiMarkovModel$new(V = list(s.well, s.disabled, s.dead), E, tcycle=42), 
    class="invalid_tcycle"
  )
  # create the model
  M <- SemiMarkovModel$new(V = list(s.well, s.disabled, s.dead), E)
  # set rates
  M$set_probabilities(Pt)
  # test illegal arguments to cycle
  expect_error(M$cycle(hcc.pop=3), class="invalid_hcc")
  expect_error(M$cycle(hcc.cost=3), class="invalid_hcc")
  expect_error(M$cycle(hcc.pop=FALSE,hcc.cost=TRUE), class="invalid_hcc")
  # test cycles
  DF <- M$cycle()
  expect_true(is.data.frame(DF))
  expect_setequal(
    names(DF), 
    c("State", "Cycle", "Time", "Population", "EntryCost", "OccCost", "Cost", 
      "QALY")
  )
  expect_equal(nrow(DF),3)
})

# -----------------------------------------------------------------------------
# Sonnenberg & Beck, Med Decis Making, 1993;13:322, Fig 3
# (prosthetic heart valve)
# -----------------------------------------------------------------------------
test_that("rdecision replicates Sonnenberg & Beck, Fig 3", {
  # create states
  s.well <- MarkovState$new(name="Well", utility=1)
  s.disabled <- MarkovState$new(name="Disabled",utility=0.7)
  s.dead <- MarkovState$new(name="Dead",utility=0)
  # create transitions leaving rates undefined
  E <- list(
    Transition$new(s.well, s.well),
    Transition$new(s.dead, s.dead),
    Transition$new(s.disabled, s.disabled),
    Transition$new(s.well, s.disabled),
    Transition$new(s.well, s.dead),
    Transition$new(s.disabled, s.dead)
  )
  # create the model
  M <- SemiMarkovModel$new(V = list(s.well, s.disabled, s.dead), E)
  # check the state tabulation
  ST <- M$tabulate_states()
  expect_setequal(names(ST), c("Name", "Cost", "Utility"))
  expect_equal(nrow(ST),3)
  # create transition probability matrix
  snames <- c("Well","Disabled","Dead")
  EPt <- matrix(
    data = c(0.6, 0.2, 0.2, 0, 0.6, 0.4, 0, 0, 1),
    nrow = 3, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  # set and check the transition probabilities
  M$set_probabilities(EPt)
  Pt <- M$transition_probability()
  expect_true(all(EPt-Pt < sqrt(.Machine$double.eps)))
  # set the starting populations
  M$reset(c(Well=10000, Disabled=0, Dead=0)) 
  # cycle
  RC <- M$cycles(25, hcc.pop=FALSE, hcc.cost=FALSE)
  expect_true(is.data.frame(RC))
  expect_equal(round(RC$Well[RC$Cycle==2]), 3600)
  expect_equal(round(RC$Disabled[RC$Cycle==2]), 2400)
  expect_equal(round(RC$Dead[RC$Cycle==2]), 4000)
})

# ---------------------------------------------------------------------------
# Chancellor, 1997 (HIV) with PSA and Briggs exercises 2.5 and 4.7 
# ** This is a bad example because Briggs et al have converted a table of
# ** observed transitions (Table 2.5, p38) into probabilities. They should
# ** have followed the method of Welton and Ades (2005) which describes how to
# ** convert fully observed transitions into rates. It has the consequence of
# ** an unusual usage of the Dirichlet distributions, which should have been
# ** used only for the conditionals. For future releases, consider leaving
# ** the validation case as a point estimate only and choosing a different
# ** validation example for PSA.
# ---------------------------------------------------------------------------
test_that("redecision replicates Briggs' example 4.7", {
  #
  # Discount rates
  # ==============
  cDR <- 6 # annual discount rate, costs (%)
  oDR <- 0 # annual discount rate, benefits (%)
  #
  # Costs
  # =====
  # drug costs
  cAZT <- 2278 # zidovudine drug cost
  cLam <- 2087 # lamivudine drug cost
  # direct medical and community costs (modelled as gamma distributions)
  dmca <- GammaModVar$new("dmca", "GBP", shape=1, scale=1701)
  dmcb <- GammaModVar$new("dmcb", "GBP", shape=1, scale=1774)
  dmcc <- GammaModVar$new("dmcc", "GBP", shape=1, scale=6948)
  ccca <- GammaModVar$new("ccca", "GBP", shape=1, scale=1055)
  cccb <- GammaModVar$new("cccb", "GBP", shape=1, scale=1278)
  cccc <- GammaModVar$new("cccc", "GBP", shape=1, scale=2059)
  # occupancy costs with monotherapy
  cAm <- ExprModVar$new("cA", "GBP", rlang::quo(dmca+ccca+cAZT))
  cBm <- ExprModVar$new("cB", "GBP", rlang::quo(dmcb+cccb+cAZT))
  cCm <- ExprModVar$new("cC", "GBP", rlang::quo(dmcc+cccc+cAZT))
  # occupancy costs with combination therapy
  cAc <- ExprModVar$new("cAc", "GBP", rlang::quo(dmca+ccca+cAZT+cLam))
  cBc <- ExprModVar$new("cBc", "GBP", rlang::quo(dmcb+cccb+cAZT+cLam))
  cCc <- ExprModVar$new("cCc", "GBP", rlang::quo(dmcc+cccc+cAZT+cLam))
  #  
  # Markov model
  # ============
  # states (leave all costs as zero initially)
  sA <- MarkovState$new("A")
  sB <- MarkovState$new("B")
  sC <- MarkovState$new("C")
  sD <- MarkovState$new("D", cost=0, utility=0)
  # transitions (leave all rates as NA initially)
  tAA <- Transition$new(sA, sA)
  tAB <- Transition$new(sA, sB)
  tAC <- Transition$new(sA, sC)
  tAD <- Transition$new(sA, sD)
  tBB <- Transition$new(sB, sB)
  tBC <- Transition$new(sB, sC)
  tBD <- Transition$new(sB, sD)
  tCC <- Transition$new(sC, sC)
  tCD <- Transition$new(sC, sD)
  tDD <- Transition$new(sD, sD)
  # model
  m <- SemiMarkovModel$new(
    V = list(sA, sB, sC, sD),
    E = list(tAA, tAB, tAC, tAD, tBB, tBC, tBD, tCC, tCD, tDD),
    discount.cost = cDR/100,
    discount.utility = oDR/100
  )
  #
  # Treatment effect (modelled as a log normal distribution)
  # ========================================================
  RR <- LogNormModVar$new(
    "Tx effect", "RR", p1=0.509, p2=(0.710-0.365)/(2*1.96), "LN7"
  )
  #
  # Dirichlet distributions for conditional probabilities
  # =====================================================
  DA <- DirichletDistribution$new(c(1251, 350, 116, 17)) # from A
  DB <- DirichletDistribution$new(c(731,512,15))  # from B
  DC <- DirichletDistribution$new(c(1312,437)) # from C
  
  #
  # Function to estimate life years gained and costs
  # ================================================
  runmodel <- function(expected=TRUE, hcc.pop=FALSE, hcc.cost=FALSE) {
    # set variables
    modvars <- m$modvars()
    if (expected) {
      sapply(modvars, FUN=function(mv){mv$set("expected")})
      RR$set("expected")
      Pt <- matrix(
        c(DA$mean(), c(0, DB$mean()), c(0, 0, DC$mean()), c(0, 0, 0, 1)), 
        byrow = TRUE,
        nrow = 4,
        dimnames = list(source=c("A","B","C","D"), target=c("A","B","C","D"))
      )
    } else {
      sapply(modvars, FUN=function(mv){mv$set("random")})
      RR$set("random")
      Pt <- matrix(
        c(DA$r(), c(0, DB$r()), c(0, 0, DC$r()), c(0, 0, 0, 1)), 
        byrow = TRUE,
        nrow = 4,
        dimnames = list(source=c("A","B","C","D"), target=c("A","B","C","D"))
      )
    }
    # 
    # Monotherapy
    # ===========
    # set costs
    sA$set_cost(cAm)
    sB$set_cost(cBm)
    sC$set_cost(cCm)
    sD$set_cost(0)
    # set transition probabilities
    m$set_probabilities(Pt)
    # check them
    TM <- m$transition_probability()
    if (expected) {
      E <- matrix(
        c(0.721, 0.202, 0.067, 0.010,  
          0.000, 0.581, 0.407, 0.012,
          0.000, 0.000, 0.750, 0.250,
          0.000, 0.000, 0.000, 1.000),   # typo in book (D,D) = 1!
        byrow = TRUE,
        nrow = 4
      )
      expect_true(all(TM-E < 0.01))
    }
    # create starting populations
    N <- 1000
    populations <- c(A = N, B = 0, C = 0, D = 0)
    m$reset(populations)
    # run 20 cycles
    MT.mono <- m$cycles(
      ncycles=20, 
      hcc.pop = hcc.pop,
      hcc.cost = hcc.cost
    )
    # expected life years and costs
    el.mono <- sum(MT.mono$QALY)
    cost.mono <- sum(MT.mono$Cost)
    #
    # Combination therapy
    # ===================
    # set costs
    sA$set_cost(cAc)
    sB$set_cost(cBc)
    sC$set_cost(cCc)
    sD$set_cost(0)
    # create Pt for combination therapy (Briggs applied the RR to the transition 
    # probabilities - not recommended, but done here for reproducibility).
    Ptc <- Pt
    for (i in 1:4) {
      for (j in 1:4) {
        Ptc[i,j] <- ifelse(i==j, NA, RR$get()*Ptc[i,j])
      }
      Ptc[i,which(is.na(Ptc[i,]))] <- 1-sum(Ptc[i,],na.rm=TRUE) 
    }
    # set transition rates from probabilities
    m$set_probabilities(Ptc)
    # check them
    TC <- m$transition_probability()
    if (expected) {
      E <- matrix(
        c(0.858, 0.103, 0.034, 0.005,  
          0.000, 0.787, 0.207, 0.006,
          0.000, 0.000, 0.873, 0.127,
          0.000, 0.000, 0.000, 1.000),   
        byrow = TRUE,
        nrow = 4)
      expect_true(all(TC-E < 0.01))
    }
    # run combination therapy model for 2 years
    populations <- c('A'=N, 'B'=0, 'C'=0, 'D'=0)
    m$reset(populations)
    # run 2 cycles
    MT.comb <- m$cycles(
      2, 
      hcc.pop = hcc.pop,
      hcc.cost = hcc.cost
    )
    # set costs
    sA$set_cost(cAm)
    sB$set_cost(cBm)
    sC$set_cost(cCm)
    sD$set_cost(0)
    # set probabilities
    m$set_probabilities(Pt)
    # set populations in mono model & reset cycle counter and time
    populations <- m$get_populations()
    m$reset(
      populations, 
      icycle=as.integer(2), 
      elapsed=as.difftime(365.25*2, units="days")
    )
    # run mono model for next 18 years
    MT.comb <- rbind(
      MT.comb, 
      m$cycles(
        ncycles=18, 
        hcc.pop = hcc.pop,
        hcc.cost = hcc.cost
      )
    ) 
    # expected life years and costs
    el.comb <- sum(MT.comb$QALY)
    cost.comb <- sum(MT.comb$Cost)
    #
    return(c("el.mono"=el.mono, "cost.mono"=cost.mono, 
             "el.comb"=el.comb, "cost.comb"=cost.comb
             ))
  }
  #
  # Point estimate (without half-cycle correction)
  # ==============================================
  # run the model
  M <- runmodel(expected=TRUE, hcc.pop=FALSE, hcc.cost=FALSE)
  # check results
  expect_intol(M["el.mono"], 7.991, tolerance=0.03) # 7.991 from spreadsheet
  expect_intol(M["cost.mono"], 44663, 100) # rounding errors in book
  expect_intol(M["el.comb"], 8.937, tolerance=0.02) # 8.937 from spreadsheet
  expect_intol(M["cost.comb"], 50602, 100) # rounding errors in book
  icer <- (M["cost.comb"]-M["cost.mono"])/(M["el.comb"]-M["el.mono"])
  expect_intol(icer, 6276, 10) # rounding errors in book
  #
  # Point estimate (with population half-cycle correction)
  # ======================================================
  # run the model
  M <- runmodel(expected=TRUE, hcc.pop=TRUE, hcc.cost=FALSE)
  # check results
  expect_intol(M["el.mono"], 8.48, tolerance=0.03) 
  expect_intol(M["cost.mono"], 44663, 100) # rounding errors in book
  expect_intol(M["el.comb"], 9.42, tolerance=0.02) 
  expect_intol(M["cost.comb"], 50602, 100) # rounding errors in book
  icer <- (M["cost.comb"]-M["cost.mono"])/(M["el.comb"]-M["el.mono"])
  expect_intol(icer, 6306, 10) # rounding errors in book
  #
  # PSA
  # ===
  skip_on_cran()
  n <- 100
  DF <- sapply(1:n, FUN=function(i) {
    # run the model
    M <- runmodel(expected=FALSE, hcc.pop=TRUE, hcc.cost=FALSE)
    return(M)
  })
  DF <- as.data.frame(t(DF))
  # calculate ICER and compare with 100 ICER samples from Briggs spreadsheet, 
  # example 4.7
  DF$icer <- (DF$cost.comb - DF$cost.mono)/(DF$el.comb - DF$el.mono)
  esamp <- scan(text=
    "5301.35
    9440.15
    5744.259409
    6389.097064
    6392.624986
    3994.719593
    7172.362209
    7911.346126
    4532.492365
    5624.510108
    7661.375273
    6994.758314
    6443.136695
    8415.490257
    5867.107527
    5233.06391
    6622.541882
    6437.612983
    2911.766414
    8913.801308
    6520.597363
    5578.340444
    4729.782693
    9504.301977
    6146.588061
    3765.76602  
    6489.778374
    8057.35546
    11811.25017
    5670.353086
    3849.923114
    4950.39429
    4122.370475  
    3821.724085
    8336.515228
    4198.215337
    5253.411175
    8011.172733
    5638.448818
    7453.423065
    6658.135907
    4499.134333
    5201.390732
    7122.874494
    4876.858193 
    4023.805442
    5129.919089
    4249.610465
    12462.7631
    7434.229976
    7200.266514
    7952.9198
    6556.010328
    3827.481732
    5443.137027
    6012.291712
    7139.430982
    6533.022468
    6739.577098
    6430.554141
    5167.152795  
    6904.27616
    5573.823787
    8202.449279
    6268.396547
    4602.761381
    2471.876679
    5973.696879
    7414.062825
    6541.740092
    1793.977058
    5224.207019
    3769.477755
    3660.149219
    16972.62138
    6229.749342
    2105.755358
    6150.607183
    6088.493293
    6090.582267
    4299.749596
    6894.101671
    8711.266189
    10316.17458
    7869.832914
    4949.493389
    4319.858777
    5084.068203
    7047.4081
    3261.818576
    7305.940637
    10060.60441
    5851.727982
    8271.551092
    5398.198376
    7150.231396
    8151.732244
    3949.116594
    7574.593941
    4027.405958",
    quiet = TRUE
  ) 
  ht <- ks.test(DF$icer, esamp)
  expect_true(ht$p.value > 0.001)
  
})


