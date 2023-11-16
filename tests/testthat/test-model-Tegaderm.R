# Script to construct, in rdecision, the model reported by Jenks et al
# (App Health Econ 2016;14:135-149) for a securement dressing for central
# venous and arterial catheters.
#
# The checks are done as part of the testthat framework, which ensures that 
# any changes in the package code which unintentionally result in deviations
# from the reported results of the model are identified.
#
# Code to construct and run the model is contained within labelled knitr code
# chunks and does not contain test expectations, so can be used by a vignette.
# Unlabelled code chunks may contain testthat expectations and should be
# ignored by a vignette.

## @knitr variables -----------------------------------------------------------
# baseline risk
r.CRBSI <- GammaModVar$new(
  "Baseline CRBSI rate",  "/1000 catheter days",
  shape = (1.48 ^ 2L) / (0.12 ^ 2L),
  scale = (0.12 ^ 2L) / 1.48
)
r.LSI <- GammaModVar$new(
  "Baseline LSI rate", "/1000 catheter days",
  shape = (0.14 ^ 2L) / (0.5 ^ 2L),
  scale = (0.5 ^ 2L) / 0.14
)
r.Dermatitis <- BetaModVar$new(
  "Baseline dermatitis risk", "/catheter", alpha = 1L, beta = 475L
)
# relative effectiveness
hr.CRBSI <- LogNormModVar$new(
  "Tegaderm CRBSI HR", "HR",
  p1 = 0.402, p2 = (0.868 - 0.186) / (2L * 1.96), param = "LN7"
)
hr.LSI <- LogNormModVar$new(
  "Tegaderm LSI HR", "HR", 
  p1 = 0.402, p2 = (0.868 - 0.186) / (2L * 1.96), param = "LN7"
)
rr.Dermatitis <- LogNormModVar$new(
  "Tegaderm Dermatitis RR", "RR", p1 = 1.0, p2 = 0.5, param = "LN7"
)
# cost variables
c.CRBSI <- GammaModVar$new(
  "CRBSI cost", "GBP",
  shape = (9900.0 ^ 2L) / (3000.0 ^ 2L),
  scale = (3000.0 ^ 2L) / 9900.0
)
c.LSI <- GammaModVar$new(
  "LSI cost", "GBP",
  shape = (100.0 ^ 2L) / (30.0 ^ 2L),
  scale = (30.0 ^ 2L) / 100.0
)
c.Dermatitis <- GammaModVar$new(
  "Dermatitis cost", "GBP",
  shape = (6.0 ^ 2L) / (3.0 ^ 2L),
  scale = (3.0 ^ 2L) / 6.0
)
# number of dressings and days with catheter
n.dressings <- GammaModVar$new(
  "No. dressings", "dressings",
  shape = (3.0 ^ 2L) / (2.0 ^ 2L),
  scale = (2.0 ^ 2L) / 3.0 
)
n.cathdays <- GammaModVar$new(
  "No. days with catheter", "days",
  shape = (10.0 ^ 2L) / (5.0 ^ 2L),
  scale = (5.0 ^ 2L) / 10.0
)  

## @knitr ---------------------------------------------------------------------
test_that("variables have expected values", {
  # baseline CRBSI
  expect_intol(r.CRBSI$mean(), 1.48, 0.02)
  q <- r.CRBSI$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 1.28, 0.05)
  expect_intol(q[[2L]], 1.75, 0.05)
  # baseline LSI
  expect_intol(r.LSI$mean(), 0.14, 0.01)
  q <- r.LSI$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 0.0, 0.05)
  # baseline dermatitis
  expect_intol(r.Dermatitis$mean(), 1L / 476L, 0.0001)
  q <- r.Dermatitis$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 0.000, 0.005)
  expect_intol(q[[2L]], 0.010, 0.005)
  # HR of CRBSI for Tegaderm
  expect_intol(hr.CRBSI$mean(), 0.402, 0.010)
  q <- hr.CRBSI$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 0.186, 0.05)
  expect_intol(q[[2L]], 0.868, 0.05)
  # HR of LSI for Tegaderm
  expect_intol(hr.LSI$mean(), 0.402, 0.010)
  q <- hr.LSI$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 0.186, 0.05)
  expect_intol(q[[2L]], 0.868, 0.05)
  # RR of dermatitis
  expect_intol(rr.Dermatitis$mean(), 1.0, 0.010)
  q <- rr.Dermatitis$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 0.35, 0.05)
  expect_intol(q[[2L]], 2.26, 0.05)
  # cost of CRBSI
  expect_intol(c.CRBSI$mean(), 9900.0, 10.0)
  q <- c.CRBSI$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 4921.0, 10.0)
  expect_intol(q[[2L]], 16589.0, 10.0)
  # cost of LSI
  expect_intol(c.LSI$mean(), 100.0, 10.0)
  q <- c.LSI$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 50.1, 1.0)
  expect_intol(q[[2L]], 166.8, 1.0)
  # cost of dermatitis
  expect_intol(c.Dermatitis$mean(), 6.0, 0.1)
  q <- c.Dermatitis$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 1.64, 0.1)
  expect_intol(q[[2L]], 13.1, 0.1)
  # number of dressings
  expect_intol(n.dressings$mean(), 3.0, 0.1)
  q <- n.dressings$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 0.4, 0.1)
  expect_intol(q[[2L]], 8.0, 0.1)
  # number of catheter days
  expect_intol(n.cathdays$mean(), 10.0, 0.1)
  q <- n.cathdays$quantile(probs = c(0.025, 0.975))
  expect_intol(q[[1L]], 2.7, 1.0)
  expect_intol(q[[2L]], 21.9, 1.0)
})

## @knitr expressions ---------------------------------------------------------
p.CRBSI.S <- ExprModVar$new(
  "P(CRBSI | standard dressing)", "P",  
  rlang::quo(r.CRBSI * n.cathdays / 1000.0)
)
q.CRBSI.S <- ExprModVar$new(
  "Q(CRBSI | standard dressing)", "1 - P",  rlang::quo(1.0 - p.CRBSI.S)
)
p.CRBSI.T <- ExprModVar$new(
  "P(CRBSI|Tegaderm)", "P", 
  rlang::quo(p.CRBSI.S * hr.CRBSI)
)
q.CRBSI.T <- ExprModVar$new(
  "Q(CRBSI | Tegaderm)", "1 - P", rlang::quo(1.0 - p.CRBSI.T)
)
p.LSI.S <- ExprModVar$new(
  "P(LSI | Standard)", "/patient",
  rlang::quo(r.LSI * n.cathdays / 1000.0) 
)
q.LSI.S <- ExprModVar$new(
  "Q(LSI | Standard)", "1 - P", rlang::quo(1.0 - p.LSI.S) 
)
p.LSI.T <- ExprModVar$new(
  "P(LSI | Tegaderm)", "P", rlang::quo(p.LSI.S * hr.LSI)
)
q.LSI.T <- ExprModVar$new(
  "Q(LSI | Tegaderm)", "1 - P", rlang::quo(1.0 - p.LSI.T)
)
p.Dermatitis.S <- ExprModVar$new(
  "P(dermatitis | standard dressing)", "P", 
  rlang::quo(r.Dermatitis)
)
q.Dermatitis.S <- ExprModVar$new(
  "Q(dermatitis | standard dressing)", "1 - P", 
  rlang::quo(1.0 - p.Dermatitis.S)
)
p.Dermatitis.T <- ExprModVar$new(
  "P(dermatitis | Tegaderm)", "P", 
  rlang::quo(p.Dermatitis.S * rr.Dermatitis)
)
q.Dermatitis.T <- ExprModVar$new(
  "Q(dermatitis | Tegaderm)", "1 - P", 
  rlang::quo(1.0 - p.Dermatitis.T)
)
c.Tegaderm <- ExprModVar$new(
  "Tegaderm CHG cost", "GBP", rlang::quo(6.26 * n.dressings)
)
c.Standard <- ExprModVar$new(
  "Standard dressing cost", "GBP", rlang::quo(1.54 * n.dressings)
)

## @knitr tree ----------------------------------------------------------------
# create decision tree
th <- as.difftime(7L, units = "days")
# standard dressing
t01 <- LeafNode$new("t01", interval = th)
t02 <- LeafNode$new("t02", interval = th)
c01 <- ChanceNode$new()
e01 <- Reaction$new(
  c01, t01, p = p.Dermatitis.S, cost = c.Dermatitis, label = "Dermatitis"
)
e02 <- Reaction$new(
  c01, t02, p = q.Dermatitis.S, cost = 0.0, label = "No dermatitis"
)
t03 <- LeafNode$new("t03", interval = th)
t04 <- LeafNode$new("t04", interval = th)
c02 <- ChanceNode$new()
e03 <- Reaction$new(
  c02, t03, p = p.Dermatitis.S, cost = c.Dermatitis, label = "Dermatitis"
)
e04 <- Reaction$new(
  c02, t04, p = q.Dermatitis.S, cost = 0.0, label = "No dermatitis"
)
c03 <- ChanceNode$new()
e05 <- Reaction$new(c03, c01, p = p.LSI.S, cost = c.LSI, label = "LSI")
e06 <- Reaction$new(c03, c02, p = q.LSI.S, cost = 0.0, label = "No LSI")
t11 <- LeafNode$new("t11", interval = th)
t12 <- LeafNode$new("t12", interval = th)
c11 <- ChanceNode$new()
e11 <- Reaction$new(
  c11, t11, p = p.Dermatitis.S, cost = c.Dermatitis, label = "Dermatitis"
)
e12 <- Reaction$new(
  c11, t12, p = q.Dermatitis.S, cost = 0.0, label = "No Dermatitis"
)
t13 <- LeafNode$new("t13", interval = th)
t14 <- LeafNode$new("t14", interval = th)
c12 <- ChanceNode$new()
e13 <- Reaction$new(
  c12, t13, p = p.Dermatitis.S, cost = c.Dermatitis, label = "Dermatitis"
)
e14 <- Reaction$new(
  c12, t14, p = q.Dermatitis.S, cost = 0.0, label = "No dermatitis"
)
c13 <- ChanceNode$new()
e15 <- Reaction$new(c13, c11, p = p.LSI.S, cost = c.LSI, label = "LSI")
e16 <- Reaction$new(c13, c12, p = q.LSI.S, cost = 0.0, label = "No LSI")
c23 <- ChanceNode$new()
e21 <- Reaction$new(c23, c03, p = p.CRBSI.S, cost = c.CRBSI, label = "CRBSI")
e22 <- Reaction$new(c23, c13, p = q.CRBSI.S, cost = 0.0, label = "No CRBSI")

# Tegaderm branch  
t31 <- LeafNode$new("t31", interval = th)
t32 <- LeafNode$new("t32", interval = th)
c31 <- ChanceNode$new()
e31 <- Reaction$new(
  c31, t31, p = p.Dermatitis.T, cost = c.Dermatitis, label = "Dermatitis"
)
e32 <- Reaction$new(
  c31, t32, p = q.Dermatitis.T, cost = 0.0, label = "no dermatitis"
)
t33 <- LeafNode$new("t33", interval = th)
t34 <- LeafNode$new("t34", interval = th)
c32 <- ChanceNode$new()
e33 <- Reaction$new(
  c32, t33, p = p.Dermatitis.T, cost = c.Dermatitis, label = "Dermatitis"
)
e34 <- Reaction$new(
  c32, t34, p = q.Dermatitis.T, cost = 0.0, label = "No dermatitis"
)
c33 <- ChanceNode$new()
e35 <- Reaction$new(c33, c31, p = p.LSI.T, cost = c.LSI, label = "LSI")
e36 <- Reaction$new(c33, c32, p = q.LSI.T, cost = 0.0, label = "No LSI")
t41 <- LeafNode$new("t41", interval=th)
t42 <- LeafNode$new("t42", interval=th)
c41 <- ChanceNode$new()
e41 <- Reaction$new(
  c41, t41, p = p.Dermatitis.T, cost = c.Dermatitis, label = "Dermatitis"
)
e42 <- Reaction$new(
  c41, t42, p = q.Dermatitis.T, cost = 0.0, label = "No dermatitis"
)
t43 <- LeafNode$new("t43", interval = th)
t44 <- LeafNode$new("t44", interval = th)
c42 <- ChanceNode$new()
e43 <- Reaction$new(
  c42, t43, p = p.Dermatitis.T, cost = c.Dermatitis, label = "Dermatitis"
)
e44 <- Reaction$new(
  c42, t44, p = q.Dermatitis.T, cost = 0.0, label = "No dermatitis"
)
c43 <- ChanceNode$new()
e45 <- Reaction$new(c43, c41, p = p.LSI.T, cost = c.LSI, label = "LSI")
e46 <- Reaction$new(c43, c42, p = q.LSI.T, cost = 0.0, label = "No LSI")
c53 <- ChanceNode$new()
e51 <- Reaction$new(c53, c43, p = p.CRBSI.T, cost = c.CRBSI, label = "CRBSI")
e52 <- Reaction$new(c53, c33, p = q.CRBSI.T, cost = 0.0, label = "no CRBSI")
  
# decision node
d1 <- DecisionNode$new("d1")
e9 <- Action$new(d1, c23, label = "Standard", cost = c.Standard)
e10 <- Action$new(d1, c53, label = "Tegaderm", cost = c.Tegaderm)

# create decision tree
V <- list(
  d1, 
  c01, c02, c03, c11, c12, c13, c23, c31, c32, c33, c41, c42, c43, c53,
  t01, t02, t03, t04, t11, t12, t13, t14, t31, t32, t33, t34, 
  t41, t42, t43, t44
)
E <- list(
  e01, e02, e03, e04, e05, e06, e11, e12, e13, e14, e15, e16, e21, e22,
  e31, e32, e33, e34, e35, e36, e41, e42, e43, e44, e45, e46, e51, e52,
  e9, e10
)
DT <- DecisionTree$new(V, E)

## @knitr ---------------------------------------------------------------------
test_that("model variables are as expected", {
  mv <- DT$modvars()
  expect_length(mv, 25L)
  MVT <- DT$modvar_table()
  expect_identical(nrow(MVT), 25L)
  expect_identical(sum(MVT$Est), 14L)
  MVT <- DT$modvar_table(FALSE)
  expect_identical(nrow(MVT), 11L)
})

## @knitr basecase ------------------------------------------------------------
RES <- DT$evaluate()

## @knitr --------------------------------------------------------------------
test_that("EAC base case agrees with direct calculation", {
  # values from Table 4
  r_crbsi <- 1.48
  r_lsi <- 0.14
  r_derm <- 0.0021
  hr_crbsi_teg <- 0.402
  hr_lsi_teg <- 0.402
  rr_derm_teg <- 1.0
  c_crbsi <- 9900.0
  c_derm <- 6.0
  c_lsi <- 100.0
  n_cdays <- 10.0
  n_dress <- 3L
  c_teg <- 6.26 
  c_std <- 1.54
  # probabilities
  p_crbsi_std <- r_crbsi * (n_cdays / 1000.0)
  p_lsi_std <- r_lsi * (n_cdays / 1000.0)
  p_derm_std <- r_derm
  p_crbsi_teg <- p_crbsi_std * hr_crbsi_teg
  p_lsi_teg <- p_lsi_std * hr_lsi_teg
  p_derm_teg <- rr_derm_teg * p_derm_std
  # component costs
  cdress_std <- c_std * n_dress
  cdress_teg <- c_teg * n_dress
  ccrbsi_std <- c_crbsi * p_crbsi_std
  ccrbsi_teg <- c_crbsi * p_crbsi_teg
  clsi_std <- c_lsi * p_lsi_std
  clsi_teg <- c_lsi * p_lsi_teg
  cderm_std <- c_derm * p_derm_std
  cderm_teg <- c_derm * p_derm_teg
  # total costs
  c_std <- cdress_std + ccrbsi_std + clsi_std + cderm_std
  c_teg <- cdress_teg + ccrbsi_teg + clsi_teg + cderm_teg
  # check against the model
  expect_intol(RES$Cost[RES$d1 == "Standard"], c_std, 2.00)
  expect_intol(RES$Cost[RES$d1 == "Tegaderm"], c_teg, 2.00)
  # check against the Excel model
  expect_intol(RES$Cost[RES$d1 == "Standard"], 151.29, 2.00)
  expect_intol(RES$Cost[RES$d1 == "Tegaderm"], 77.75, 2.00)
})

## @knitr --------------------------------------------------------------------
test_that("illegal arguments to evaluate are rejected", {
  expect_error(DT$evaluate(setvars = 42L), class = "setvars_not_character")
  expect_error(DT$evaluate(setvars = "mode"), class = "setvars_invalid")
  expect_silent(DT$evaluate(setvars = "q2.5"))
  expect_silent(DT$evaluate(setvars = "q50"))
  expect_silent(DT$evaluate(setvars = "q97.5"))
  expect_silent(DT$evaluate(setvars = "current"))
})

## @knitr --------------------------------------------------------------------
test_that("tornado method rejects illegal arguments", {
  grDevices::pdf(file = NULL)
  # tornado
  expect_error(DT$tornado(), class = "missing_strategy")
  expect_error(
    DT$tornado(index = list(e10),ref = list(e52)),
    class = "invalid_strategy"
  )
  expect_error(
    DT$tornado(index = list(e10), ref = list(e9), outcome = "survival"),
    class = "invalid_outcome"
  )
  expect_error(
    DT$tornado(index = list(e10), ref = list(e9), draw = 42L),
    class = "invalid_draw"
  )
  expect_error(
    DT$tornado(
      index = list(e10), ref = list(e9), exclude = 42L,
      draw = TRUE
    ),
    class = "exclude_not_list"
  )
  expect_error(
    DT$tornado(
      index=list(e10), ref=list(e9), exclude = list("SN1", "SN2", "SNX"),
      draw = TRUE
    ),
    class = "exclude_element_not_modvar"
  )
  expect_silent(
    DT$tornado(
      index=list(e10), ref = list(e9), exclude = list(hr.CRBSI$description()),
      draw = TRUE
    )
  )
  to <- DT$tornado(
    index=list(e10), ref = list(e9),
    draw = TRUE
  )
  expect_identical(nrow(to), 11L)
  grDevices::dev.off()
})

## @knitr PSA -----------------------------------------------------------------
N <- 1000L
psa <- DT$evaluate(setvars = "random", by = "run", N = N)
psa$Difference <- psa$Cost.Standard - psa$Cost.Tegaderm

## @knitr ---------------------------------------------------------------------
test_that("PSA replicates values from Excel model", {
  skip_on_cran()
  expect_intol(mean(psa$Cost.Standard), 151.29, 10.00)
  expect_intol(mean(psa$Cost.Tegaderm), 77.75, 10.00)
  expect_intol(mean(psa$Difference), 72.90, 10.00)
  expect_intol(sum(psa$Difference > 0.0) / 1000L, 0.978, tol = 0.05)
})

## @knitr scenario -----------------------------------------------------------
r.CRBSI <- GammaModVar$new(
  "Baseline CRBSI rate",  "/1000 catheter days",
  shape = (0.30 ^ 2L) / (0.102 ^ 2L),
  scale = (0.102 ^ 2L) / 0.30
)
p.CRBSI.S <- ExprModVar$new(
  "P(CRBSI | standard dressing)", "P",  
  rlang::quo(r.CRBSI * n.cathdays / 1000.0)
)
q.CRBSI.S <- ExprModVar$new(
  "Q(CRBSI | standard dressing)", "1 - P",  rlang::quo(1.0 - p.CRBSI.S)
)
p.CRBSI.T <- ExprModVar$new(
  "P(CRBSI|Tegaderm)", "P", 
  rlang::quo(p.CRBSI.S * hr.CRBSI)
)
q.CRBSI.T <- ExprModVar$new(
  "Q(CRBSI | Tegaderm)", "1 - P", rlang::quo(1.0 - p.CRBSI.T)
)
e21 <- Reaction$new(c23, c03, p = p.CRBSI.S, cost = c.CRBSI, label = "CRBSI")
e22 <- Reaction$new(c23, c13, p = q.CRBSI.S, cost = 0.0, label = "No CRBSI")
e51 <- Reaction$new(c53, c43, p = p.CRBSI.T, cost = c.CRBSI, label = "CRBSI")
e52 <- Reaction$new(c53, c33, p = q.CRBSI.T, cost = 0.0, label = "no CRBSI")
E <- list(
  e01, e02, e03, e04, e05, e06, e11, e12, e13, e14, e15, e16, e21, e22,
  e31, e32, e33, e34, e35, e36, e41, e42, e43, e44, e45, e46, e51, e52,
  e9, e10
)
DT <- DecisionTree$new(V, E)

## @knitr --------------------------------------------------------------------
test_that("scenario case agrees with direct calculation", {
  # evaluate the scenario as a point estimate
  s_sco <- DT$evaluate()
  # values from Table 4
  r_crbsi <- 0.30
  r_lsi <- 0.14
  r_derm <- 0.0021
  hr_crbsi_teg <- 0.402
  hr_lsi_teg <- 0.402
  rr_derm_teg <- 1.0
  c_crbsi <- 9900.0
  c_derm <- 6.0
  c_lsi <- 100.0
  n_cdays <- 10.0
  n_dress <- 3L
  c_teg <- 6.26 
  c_std <- 1.54
  # probabilities
  p_crbsi_std <- r_crbsi * (n_cdays / 1000.0)
  p_lsi_std <- r_lsi * (n_cdays / 1000.0)
  p_derm_std <- r_derm
  p_crbsi_teg <- p_crbsi_std * hr_crbsi_teg
  p_lsi_teg <- p_lsi_std * hr_lsi_teg
  p_derm_teg <- rr_derm_teg * p_derm_std
  # component costs
  cdress_std <- c_std * n_dress
  cdress_teg <- c_teg * n_dress
  ccrbsi_std <- c_crbsi * p_crbsi_std
  ccrbsi_teg <- c_crbsi * p_crbsi_teg
  clsi_std <- c_lsi * p_lsi_std
  clsi_teg <- c_lsi * p_lsi_teg
  cderm_std <- c_derm * p_derm_std
  cderm_teg <- c_derm * p_derm_teg
  # total costs
  c_std <- cdress_std + ccrbsi_std + clsi_std + cderm_std
  c_teg <- cdress_teg + ccrbsi_teg + clsi_teg + cderm_teg
  # check against the model
  expect_intol(s_sco$Cost[s_sco$d1 == "Standard"], c_std, 2.00)
  expect_intol(s_sco$Cost[s_sco$d1 == "Tegaderm"], c_teg, 2.00)
  # check against the excel model
  expect_intol(s_sco$Cost[s_sco$d1 == "Standard"], 34.47, 2.00)
  expect_intol(s_sco$Cost[s_sco$d1 == "Tegaderm"], 30.79, 2.00)
})

## @knitr scenario-PSA --------------------------------------------------------
psa <- DT$evaluate(setvars = "random", by = "run", N = N)
psa$Difference <- psa$Cost.Standard - psa$Cost.Tegaderm

## @knitr ---------------------------------------------------------------------
test_that("scenario PSA agrees with reported results", {
  skip_on_cran()
  expect_intol(mean(psa$Cost.Standard), 34.47, 5.00)
  expect_intol(mean(psa$Cost.Tegaderm), 30.79, 5.00)
  expect_intol(mean(psa$Difference), 3.56, 3.00)
  expect_intol(sum(psa$Difference > 0.0) / 1000L, 0.579, tol = 0.2)
})

## @knitr threshold-hr -------------------------------------------------------
hr_threshold <- DT$threshold(
  index = list(e10),
  ref = list(e9),
  outcome = "saving",
  mvd = "Tegaderm CRBSI HR",
  a <- 0.1,
  b <- 0.9,
  tol = 0.01
)

## @knitr --------------------------------------------------------------------
test_that("scenario hazard rate threshold agrees with that reported", {
  expect_intol(hr_threshold, 0.53, tol = 0.05)  
})

## @knitr threshold_ccrbsi ----------------------------------------------------
c_crbsi_threshold <- DT$threshold(
  index = list(e10),
  ref = list(e9),
  outcome = "saving",
  mvd = "CRBSI cost",
  a <- 0.0,
  b <- 9900.0,
  tol = 10.0
)

## @knitr --------------------------------------------------------------------
test_that("scenario CRBSI cost threshold agrees with reported value", {
  expect_intol(c_crbsi_threshold, 8000.0, tol = 300.0)
})
