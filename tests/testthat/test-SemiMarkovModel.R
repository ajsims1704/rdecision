
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
  # no outgoing transition from dead
  expect_error(
    SemiMarkovModel$new(
      V = list(s.well, s.dead), 
      E = list(e.ww, e.wd)
    ),
    class = "missing_transition"
  )
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
# tests of setting transition probabilities
# -----------------------------------------------------------------------------
test_that("invalid transition probabilities are rejected", {
  # create states
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new(name="Disabled")
  s.dead <- MarkovState$new(name="Dead")
  # state names 
  snames <- c("Well","Disabled","Dead")
  # create transitions
  E <- list(
    Transition$new(s.well, s.well),
    Transition$new(s.dead, s.dead),
    Transition$new(s.disabled, s.disabled),
    Transition$new(s.well, s.disabled),
    Transition$new(s.well, s.dead),
    Transition$new(s.disabled, s.dead)
  )
  # create model
  M <- SemiMarkovModel$new(V = list(s.well, s.disabled, s.dead), E) 
  # check that initial state has equal probabilities
  EPt <- matrix(
    data = c(1L / 3L, 1L / 3L, 1L / 3L, 0L, 0.5, 0.5, 0L, 0L, 1L),
    nrow = 3L, byrow = TRUE,
    dimnames=list(source = snames, target = snames)
  )
  expect_identical(M$transition_probabilities(), EPt)
  # no probabilities
  expect_error(
    M$set_probabilities(), class = "invalid_Pt"
  )
  # probabilities are not a matrix
  expect_error(
    M$set_probabilities(Pt = 42L), class = "invalid_Pt"
  )
  #  probability matrix is incorrect size
  ePt <- matrix(c(1L, 0L, 0L, 1L), nrow = 2L, byrow=TRUE)  
  expect_error(
    M$set_probabilities(Pt=ePt), class = "invalid_Pt"
  )
  # probability matrix has incorrect state names
  ePt <- matrix(
    data = c(0.6, 0.2, 0.2, 0.0, 0.6, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source=c("a","b","c"), target=c("a","b","c"))
  )
  expect_error(
    M$set_probabilities(Pt=ePt), class = "invalid_Pt"
  )
  ePt <- matrix(
    data = c(0.6, 0.2, 0.2, 0.0, 0.6, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source=snames, target=c("a", "b", "c"))
  )
  expect_error(
    M$set_probabilities(Pt = ePt), class = "invalid_Pt"
  )
  # probability matrix contains multiple NA per row
  ePt <- matrix(
    data = c(0.6, NA, NA, 0.0, 0.6, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  expect_error(
    M$set_probabilities(Pt=ePt), class = "invalid_Pt"
  )
  # probability matrix contains values not in range [0,1]
  ePt <- matrix(
    data = c(0.6, 1.3, -0.9, 0.0, 0.6, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source = snames, target = snames)
  )
  expect_error(
    M$set_probabilities(Pt=ePt), class = "invalid_Pt"
  )
  # probability matrix has non-zero values for undefined transitions
  ePt <- matrix(
    data = c(0.6, 0.2, 0.2, 0.1, 0.5, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source = snames, target = snames)
  )
  expect_error(
    M$set_probabilities(Pt = ePt), class = "invalid_Pt"
  )
  # probability matrix has row sums > 1
  ePt <- matrix(
    data = c(0.6, 0.2, 0.2, 0.0, 0.7, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source = snames, target = snames)
  )
  expect_error(
    M$set_probabilities(Pt=ePt), class = "invalid_Pt"
  )
  # Sonnenberg and Beck probability matrix
  pt_sb <- matrix(
    data = c(0.6, 0.2, 0.2, 0.0, 0.6, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  expect_silent(M$set_probabilities(Pt = pt_sb))
  # Sonnenberg and Beck probability matrix with NAs - check if NAs replaced
  pt_sb_na <- matrix(
    data = c(NA, 0.2, 0.2, NA, 0.6, 0.4, 0.0, 0.0, NA),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source = snames, target = snames)
  )
  expect_silent(M$set_probabilities(Pt = pt_sb_na))
  ept_sb <- M$transition_probabilities()
  expect_identical(ept_sb, pt_sb)
})

test_that("NAs are replaced in transition probability matrix", {
  # create states
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new(name="Disabled")
  s.dead <- MarkovState$new(name="Dead")
  # create transitions
  E <- list(
    Transition$new(s.well, s.well),
    Transition$new(s.dead, s.dead),
    Transition$new(s.disabled, s.disabled),
    Transition$new(s.well, s.disabled),
    Transition$new(s.well, s.dead),
    Transition$new(s.disabled, s.dead)
  )
  # create model
  M <- SemiMarkovModel$new(V = list(s.well, s.disabled, s.dead), E) 
  # use S&B per-cycle transition probabilities
  snames <- c("Well","Disabled","Dead")
  EPt <- matrix(
    data = c(0.6, 0.2, 0.2, 0.0, 0.6, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  Pt <- matrix(
    data = c(0.6, NA, 0.2, 0.0, 0.6, 0.4, 0.0, 0.0, NA),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  M$set_probabilities(Pt)
  expect_identical(round(M$transition_probabilities(),2L), round(EPt, 2L))
})

# -----------------------------------------------------------------------------
# tests of setting transition costs
# -----------------------------------------------------------------------------
test_that("transition cost matrix is correct", {
  # create states
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new(name="Disabled", cost = 750.0)
  s.dead <- MarkovState$new(name="Dead")
  # create transitions
  E <- list(
    Transition$new(s.well, s.well),
    Transition$new(s.dead, s.dead),
    Transition$new(s.disabled, s.disabled),
    Transition$new(s.well, s.disabled, cost = 1000.0),
    Transition$new(s.well, s.dead, cost = 250.0),
    Transition$new(s.disabled, s.dead, cost = 500.0)
  )
  # create model
  M <- SemiMarkovModel$new(V = list(s.well, s.disabled, s.dead), E) 
  # use S&B per-cycle transition probabilities
  snames <- c("Well","Disabled","Dead")
  Pt <- matrix(
    data = c(0.6, NA, 0.2, 0.0, 0.6, 0.4, 0.0, 0.0, NA),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  M$set_probabilities(Pt)
  # check the transition cost matrix
  ECt <- matrix(
    data = c(0.0, 1000.0, 250.0, 0.0, 0.0, 500.0, 0.0, 0.0, 0.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  Ct <- M$transition_cost()
  expect_identical(Ct, ECt)
  # check that transition costs are accumulated
  C1 <- M$cycle(hcc.pop = FALSE, hcc.cost = FALSE)
  ec.disabled <- 0.2*1000.0
  expect_identical(
    round(C1$EntryCost[C1$State=="Disabled"], 2L), round(ec.disabled, 2L)
  )
  ec.dead <- 0.2*250.0
  expect_identical(
    round(C1$EntryCost[C1$State=="Dead"], 2L), round(ec.dead, 2L)
  )
  # check that entry costs and occupancy costs are added
  oc.disabled <- 0.2*750.0
  expect_identical(
    round(C1$Cost[C1$State=="Disabled"], 2L), 
    round(oc.disabled + ec.disabled, 2L)
  )
  expect_identical(
    round(C1$Cost[C1$State=="Dead"], 2L), round(ec.dead, 2L)
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
  expect_identical(unname(rp[1L]), 1000.0)
  expect_identical(unname(rp[2L]), 0.0)
  expect_identical(unname(rp[3L]), 0.0)
  # number of elements
  pop <- c(Well = 10000L, Disabled = 0L)
  expect_error(M$reset(pop), class = "incorrect_state_count")
  # state names
  pop <- c(Well = 10000.0, Poorly = 0.0, Disabled = 0.0)
  expect_error(M$reset(pop), class = "unmatched_states")
  pop <- c(10000L, 0L, 0L)
  expect_error(M$reset(pop), class = "unmatched_states")
  # type
  pop <- c(Well = 10000L, Disabled = "0", Dead = 0L)
  expect_error(M$reset(pop), class = "non-numeric_state_population")
  # correct
  pop <- c(Well = 10000L, Disabled = 0L, Dead = 0L)
  expect_silent(M$reset(pop))
  rp <- M$get_populations()
  expect_identical(unname(rp[["Well"]]), 10000.0)
  expect_identical(unname(rp[["Disabled"]]), 0.0)
  expect_identical(unname(rp[["Dead"]]), 0.0)
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
  expect_error(M$reset(icycle = 2.0), class = "invalid_icycle")
  expect_error(M$reset(icycle = "2"), class="invalid_icycle")
  expect_error(M$reset(icycle = -1L), class="invalid_icycle")
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
  expect_error(M$reset(elapsed = 2.0), class="invalid_elapsed")
  expect_error(M$reset(elapsed = "2"), class="invalid_elapsed")
  expect_error(
    M$reset(icycle = as.difftime(-1.0, units = "days")), 
    class="invalid_icycle"
  )
})

# -----------------------------------------------------------------------------
# tests of model variables
# -----------------------------------------------------------------------------
test_that("model variables are detected", {
  # example of monotherapy from Chancellor, 1997
  # drug costs
  cAZT <- 2278.0 # zidovudine drug cost
  cLam <- 2087.0 # lamivudine drug cost
  # direct medical and community costs (modelled as gamma distributions)
  dmca <- GammaModVar$new("dmca", "GBP", shape = 1.0, scale = 1701.0)
  dmcb <- GammaModVar$new("dmcb", "GBP", shape = 1.0, scale = 1774.0)
  dmcc <- GammaModVar$new("dmcc", "GBP", shape = 1.0, scale = 6948.0)
  ccca <- GammaModVar$new("ccca", "GBP", shape = 1.0, scale = 1055.0)
  cccb <- GammaModVar$new("cccb", "GBP", shape = 1.0, scale = 1278.0)
  cccc <- GammaModVar$new("cccc", "GBP", shape = 1.0, scale = 2059.0)
  # occupancy costs with monotherapy
  cAm <- ExprModVar$new("cA", "GBP", rlang::quo(dmca+ccca+cAZT))
  cBm <- ExprModVar$new("cB", "GBP", rlang::quo(dmcb+cccb+cAZT))
  cCm <- ExprModVar$new("cC", "GBP", rlang::quo(dmcc+cccc+cAZT))
  # Markov model
  # ============
  # states (leave all costs as zero initially)
  sA <- MarkovState$new("A", cost=cAm)
  sB <- MarkovState$new("B", cost=cBm)
  sC <- MarkovState$new("C", cost=cCm)
  sD <- MarkovState$new("D", cost = 0.0, utility = 0.0)
  # transitions 
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
    E = list(tAA, tAB, tAC, tAD, tBB, tBC, tBD, tCC, tCD, tDD)
  )
  # check modvars
  mv <- m$modvars()
  expect_length(mv, 9L)
  mvt <- m$modvar_table()
  expect_identical(nrow(mvt), 9L)
  mvt <- m$modvar_table(expressions = FALSE)
  expect_identical(nrow(mvt), 6L)
})

# -----------------------------------------------------------------------------
# tests of cycling
# -----------------------------------------------------------------------------

test_that("low-level population cycling operates as expected", {
  # create states
  s.well <- MarkovState$new(name = "Well")
  s.disabled <- MarkovState$new(name = "Disabled")
  s.dead <- MarkovState$new(name = "Dead")
  # use S&B per-cycle transition probabilities and calculate rates
  snames <- c("Well", "Disabled", "Dead")
  Pt <- matrix(
    data = c(0.6, 0.2, 0.2, 0.0, 0.6, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source = snames, target = snames)
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
  # use a subclass to add a test wrapper for method cycle_pop
  TestSemiMarkovModel <- R6Class(
    classname = "TestSemiMarkovModel",
    inherit = SemiMarkovModel,
    public = list(
      test_cycle_pop = function() {
        # get the populations and cycle details prior to cycling
        pop_pre <- private$smm.pop
        icycle_pre <- private$smm.icycle
        elapsed_pre <- private$smm.elapsed
        # run one low-level cycle
        n_t <- private$cycle_pop()
        # check that the populations and cycle details have updated
        epop <- drop(pop_pre %*% Pt)
        expect_identical(private$smm.pop, epop)
        expect_identical(private$smm.icycle, icycle_pre + 1L)
        expect_identical(private$smm.elapsed, elapsed_pre + private$smm.tcycle)
        # return the transition count
        return(n_t)
      }
    )
  )
  # create the model
  m <- TestSemiMarkovModel$new(V = list(s.well, s.disabled, s.dead), E)
  m$set_probabilities(Pt)
  m$reset(c(Well = 10000.0, Disabled = 0.0, Dead = 0.0)) 
  # run one low-level population cycle and check against expected populations
  n_t <- m$test_cycle_pop()
  expect_true(is.matrix(n_t))
  expect_identical(nrow(n_t), 3L)
  expect_identical(ncol(n_t), 3L)
  expect_setequal(rownames(n_t), snames)
  expect_setequal(colnames(n_t), snames)
  expect_intol(n_t[["Well", "Disabled"]], 2000.0, tolerance = 1.0)
  # run one more cycle (to 2 years)
  n_t <- m$test_cycle_pop()
  expect_intol(n_t[["Well", "Disabled"]], 1200.0, tolerance = 1.0)
  expect_intol(n_t[["Disabled", "Dead"]], 800.0, tolerance = 1.0)
  pop <- m$get_populations()
  expect_identical(round(pop[["Well"]]), 3600.0)
  expect_identical(round(pop[["Disabled"]]), 2400.0)
  expect_identical(round(pop[["Dead"]]), 4000.0)
  # run 23 more cycles
  for (i in 1:23) {
    m$test_cycle_pop() 
  }
  expect_identical(m$get_elapsed(), as.difftime(25.0*365.25, units = "days"))
})

test_that("model is cyclable", {
  # create states
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new(name="Disabled")
  s.dead <- MarkovState$new(name="Dead")
  # use S&B per-cycle transition probabilities and calculate rates
  snames <- c("Well","Disabled","Dead")
  Pt <- matrix(
    data = c(0.6, 0.2, 0.2, 0.0, 0.6, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
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
    SemiMarkovModel$new(V = list(s.well, s.disabled, s.dead), E, tcycle = 42L), 
    class="invalid_tcycle"
  )
  # create the model
  M <- SemiMarkovModel$new(V = list(s.well, s.disabled, s.dead), E)
  # set rates
  M$set_probabilities(Pt)
  # test illegal arguments to cycle
  expect_error(M$cycle(hcc.pop = 3.0), class = "invalid_hcc")
  expect_error(M$cycle(hcc.cost = 3.0), class = "invalid_hcc")
  # check return object from cycle()
  DF <- M$cycle()
  expect_identical(M$get_elapsed(), as.difftime(365.25, units = "days"))
  expect_s3_class(DF, "data.frame")
  expect_setequal(
    names(DF), 
    c("State", "Cycle", "Time", "Population", "EntryCost", "OccCost", "Cost", 
      "QALY")
  )
  expect_identical(nrow(DF), 3L)
  expect_setequal(
    DF[, "State"], c("Well", "Disabled", "Dead")
  )
})

# cyclng with utilities > 1
test_that("utilities > 1 are supported via model variables", {
  cv <- ConstModVar$new(description = "", units = "", const = 2.0)
  a <- MarkovState$new(name = "A", cost = 0.0, utility = 0.9)
  b <- MarkovState$new(name = "B", cost = 0.0, utility = 0.8)
  c <- MarkovState$new(name = "C", cost = 0.0, utility = cv)
  aa <- Transition$new(source = a, target = a, cost = 0.0)
  ab <- Transition$new(source = a, target = b, cost = 0.0)
  ac <- Transition$new(source = a, target = c, cost = 0.0)
  bb <- Transition$new(source = b, target = b, cost = 0.0)
  cc <- Transition$new(source = c, target = c, cost = 0.0)
  m <- SemiMarkovModel$new(V = list(a, b, c), E = list(aa, ab, ac, bb, cc))
  pt <- matrix(
    data = c(NA, 0.2, 0.1, 0.0, NA, 0.0, 0.0, 0.0, NA),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(source = c("A", "B", "C"), target = c("A", "B", "C"))
  )
  m$set_probabilities(pt)
  tr <- m$cycle(hcc.pop = FALSE, hcc.cost = FALSE)
  expect_intol(
    tr$QALY[tr$State == "C" & tr$Cycle == 1L], 
    (1000.0 * 0.1 * 2.0) / 1000.0,
    0.01
  )
})

# -----------------------------------------------------------------------------
# Sonnenberg & Beck, Med Decis Making, 1993;13:322, Fig 3
# (prosthetic heart valve)
# -----------------------------------------------------------------------------
test_that("rdecision replicates Sonnenberg & Beck, Fig 3", {
  # create states
  s.well <- MarkovState$new(name = "Well", utility = 1.0)
  s.disabled <- MarkovState$new(name = "Disabled", utility = 0.7)
  s.dead <- MarkovState$new(name = "Dead", utility = 0.0)
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
  expect_identical(nrow(ST), 3L)
  # create transition probability matrix
  snames <- c("Well","Disabled","Dead")
  EPt <- matrix(
    data = c(0.6, 0.2, 0.2, 0.0, 0.6, 0.4, 0.0, 0.0, 1.0),
    nrow = 3L, byrow = TRUE,
    dimnames = list(source=snames, target=snames)
  )
  # set and check the transition probabilities
  M$set_probabilities(EPt)
  Pt <- M$transition_probabilities()
  expect_true(all(EPt-Pt < sqrt(.Machine$double.eps)))
  # set the starting populations
  M$reset(c(Well = 10000.0, Disabled = 0.0, Dead = 0.0)) 
  # cycle
  RC <- M$cycles(25L, hcc.pop = FALSE, hcc.cost = FALSE)
  expect_identical(M$get_elapsed(), as.difftime(25.0*365.25, units="days"))
  expect_s3_class(RC, "data.frame")
  expect_identical(round(RC$Well[RC$Cycle == 2L]), 3600.0)
  expect_identical(round(RC$Disabled[RC$Cycle == 2L]), 2400.0)
  expect_identical(round(RC$Dead[RC$Cycle == 2L]), 4000.0)
})
