# Script to construct, in rdecision, a model of the fictitious and idealized
# decision problem of choosing between providing two forms of lifestyle advice,
# offered to people with vascular disease, which reduce the risk of needing an
# interventional procedure.
#
# The checks are done as part of the testthat framework, ensuring that
# changes in the package code which unintentionally result in deviations
# from the expected results of the model are identified.
#
# Code to construct and run the model is contained within labelled knitr code
# chunks and do not contain test expectations, so can be used by a vignette.
# Unlabelled code chunks may contain testthat expectations and should be
# ignored by a vignette.

## @knitr cost-vars -----------------------------------------------------------
cost_diet <- 50.0
cost_exercise <- 750.0
cost_stent <- 5000.0

## @knitr decision-node -------------------------------------------------------
decision_node <- DecisionNode$new("Programme")

## @knitr chance-nodes --------------------------------------------------------
chance_node_diet <- ChanceNode$new("Outcome")
chance_node_exercise <- ChanceNode$new("Outcome")

## @knitr leaf-nodes -----------------------------------------------------------
leaf_node_diet_no_stent <- LeafNode$new("No intervention")
leaf_node_diet_stent <- LeafNode$new("Intervention")
leaf_node_exercise_no_stent <- LeafNode$new("No intervention")
leaf_node_exercise_stent <- LeafNode$new("Intervention")

## @knitr actions -------------------------------------------------------------
action_diet <- Action$new(
  decision_node, chance_node_diet, cost = cost_diet, label = "Diet"
)
action_exercise <- Action$new(
  decision_node, chance_node_exercise, cost = cost_exercise, label = "Exercise"
)

## @knitr trial-results -------------------------------------------------------
s_diet <- 12L
f_diet <- 56L
s_exercise <- 18L
f_exercise <- 40L

## @knitr nnt -----------------------------------------------------------------
ip_diet <- f_diet / (s_diet + f_diet)
ip_exercise <- f_exercise / (s_exercise + f_exercise)
nnt <- 1.0 / (ip_diet - ip_exercise)

## @knitr prob-vars -----------------------------------------------------------
p_diet <- 1.0 - ip_diet
p_exercise <- 1.0 - ip_exercise
q_diet <- 1.0 - p_diet
q_exercise <- 1.0 - p_exercise

## @knitr reactions -----------------------------------------------------------
reaction_diet_success <- Reaction$new(
  chance_node_diet, leaf_node_diet_no_stent,
  p = p_diet, cost = 0.0, label = "Success"
)

reaction_diet_failure <- Reaction$new(
  chance_node_diet, leaf_node_diet_stent,
  p = q_diet, cost = cost_stent, label = "Failure"
)

reaction_exercise_success <- Reaction$new(
  chance_node_exercise, leaf_node_exercise_no_stent,
  p = p_exercise, cost = 0.0, label = "Success"
)

reaction_exercise_failure <- Reaction$new(
  chance_node_exercise, leaf_node_exercise_stent,
  p = q_exercise, cost = cost_stent, label = "Failure"
)

## @knitr decision-tree -------------------------------------------------------
dt <- DecisionTree$new(
  V = list(
    decision_node,
    chance_node_diet,
    chance_node_exercise,
    leaf_node_diet_no_stent,
    leaf_node_diet_stent,
    leaf_node_exercise_no_stent,
    leaf_node_exercise_stent
  ),
  E = list(
    action_diet,
    action_exercise,
    reaction_diet_success,
    reaction_diet_failure,
    reaction_exercise_success,
    reaction_exercise_failure
  )
)

## @knitr evaluate ------------------------------------------------------------
rs <- dt$evaluate()

## @knitr verify-base ---------------------------------------------------------
o_netc_diet <- rs[[which(rs[, "Programme"] == "Diet"), "Cost"]]
e_netc_diet <- cost_diet + q_diet * cost_stent
o_netc_exercise <- rs[[which(rs[, "Programme"] == "Exercise"), "Cost"]]
e_netc_exercise <- cost_exercise + q_exercise * cost_stent
incc <- nnt * (cost_exercise - cost_diet)
o_deltac <- o_netc_exercise - o_netc_diet
e_deltac <- (incc - cost_stent) / nnt
e_cost_threshold <- (cost_stent / nnt) + cost_diet
nnt_threshold <- cost_stent / (cost_exercise - cost_diet)
e_success_threshold <- 1.0 - (ip_diet - (1.0 / nnt_threshold))

## @knitr ---------------------------------------------------------------------
test_that("base case evaluation agrees with direct calculation", {
  expect_intol(o_netc_diet, e_netc_diet, 5.0)
  expect_intol(o_netc_exercise, e_netc_exercise, 5.0)
  expect_intol(o_deltac, e_deltac, 1.0)
})

## @knitr evaluate-by-path ----------------------------------------------------
rp <- dt$evaluate(by = "path")

## @knitr lower-utility -------------------------------------------------------
du_stent <- 0.05
leaf_node_diet_stent$set_utility(1.0 - du_stent)
leaf_node_exercise_stent$set_utility(1.0 - du_stent)
rs <- dt$evaluate()

## @knitr icer ----------------------------------------------------------------
delta_c <- rs[[which(rs[, "Programme"] == "Exercise"), "Cost"]] -
  rs[[which(rs[, "Programme"] == "Diet"), "Cost"]]
delta_u <- rs[[which(rs[, "Programme"] == "Exercise"), "Utility"]] -
  rs[[which(rs[, "Programme"] == "Diet"), "Utility"]]
icer <- delta_c / delta_u

## @knitr verify-icer ---------------------------------------------------------
e_du <- du_stent * (p_exercise - p_diet)
e_icer <- (e_netc_exercise - e_netc_diet) / e_du

## @knitr ---------------------------------------------------------------------
test_that("ICER agrees with direct calculation", {
  expect_intol(icer, e_icer, 100.0)
})

## @knitr cost-modvars ---------------------------------------------------------
cost_diet <- ConstModVar$new("Cost of diet programme", "GBP", 50.0)
cost_exercise <- ConstModVar$new("Cost of exercise programme", "GBP", 750.0)
cost_stent <- ConstModVar$new("Cost of stent intervention", "GBP", 5000.0)

## @knitr prob-modvars ---------------------------------------------------------
p_diet <- BetaModVar$new(
  alpha = s_diet, beta = f_diet, description = "P(diet)", units = ""
)
p_exercise <- BetaModVar$new(
  alpha = s_exercise, beta = f_exercise, description = "P(exercise)", units = ""
)

q_diet <- ExprModVar$new(
  rlang::quo(1.0 - p_diet), description = "1 - P(diet)", units = ""
)
q_exercise <- ExprModVar$new(
  rlang::quo(1.0 - p_exercise), description = "1 - P(exercise)", units = ""
)

## @knitr actions-probabilistic -----------------------------------------------
action_diet$set_cost(cost_diet)
action_exercise$set_cost(cost_exercise)

## @knitr reactions-probabilistic ---------------------------------------------
reaction_diet_success$set_probability(p_diet)

reaction_diet_failure$set_probability(q_diet)
reaction_diet_failure$set_cost(cost_stent)

reaction_exercise_success$set_probability(p_exercise)

reaction_exercise_failure$set_probability(q_exercise)
reaction_exercise_failure$set_cost(cost_stent)

## @knitr dt-evaluate-expected ------------------------------------------------
rs <- dt$evaluate()

## @knitr ---------------------------------------------------------------------
test_that("base case evaluation agrees with direct calculation", {
  # direct calculations
  o_netc_diet <- rs[[which(rs[, "Programme"] == "Diet"), "Cost"]]
  o_netc_exercise <- rs[[which(rs[, "Programme"] == "Exercise"), "Cost"]]
  o_deltac <- o_netc_exercise - o_netc_diet

  expect_intol(o_netc_diet, e_netc_diet, 5.0)
  expect_intol(o_netc_exercise, e_netc_exercise, 5.0)
  expect_intol(o_deltac, e_deltac, 1.0)
})

## @knitr dt-evaluate-quantiles -----------------------------------------------
rs_025 <- dt$evaluate(setvars = "q2.5")
rs_975 <- dt$evaluate(setvars = "q97.5")

## @knitr dt-evaluate-random -------------------------------------------------
N <- 1000L
rs <- dt$evaluate(setvars = "random", by = "run", N = N)

## @knitr dt-difference -------------------------------------------------------
rs[, "Difference"] <- rs[, "Cost.Diet"] - rs[, "Cost.Exercise"]
CI <- quantile(rs[, "Difference"], c(0.025, 0.975))

## @knitr ---------------------------------------------------------------------
test_that("invalid arguments to threshold are rejected", {
  expect_error(
    dt$threshold(),
    class = "missing_strategy"
  )
  expect_error(
    dt$threshold(
      ref = list(action_exercise),
      index = list(reaction_diet_success)
    ),
    class = "invalid_strategy"
  )
  expect_error(
    dt$threshold(
      ref = list(action_exercise),
      index = list(action_diet),
      outcome = "widgets"
    ),
    class = "invalid_outcome"
  )
  expect_error(
    dt$threshold(
      ref = list(action_exercise),
      index = list(action_diet),
      outcome = "saving",
      mvd = "P(dyet)"
    ),
    class = "invalid_mvd"
  )
  expect_error(
    dt$threshold(
      ref = list(action_exercise),
      index = list(action_diet),
      outcome = "saving",
      mvd = 42L
    ),
    class = "invalid_mvd"
  )
  expect_error(
    dt$threshold(
      ref = list(action_exercise),
      index = list(action_diet),
      outcome = "saving",
      mvd = cost_exercise$description(),
      a = 999.0,
      b = 1000.0
    ),
    class = "invalid_tol"
  )
  expect_error(
    dt$threshold(
      ref = list(action_exercise),
      index = list(action_diet),
      outcome = "saving",
      mvd = cost_exercise$description(),
      a = 1000.0,
      b = 999.0,
      tol = 10.0
    ),
    class = "invalid_brackets"
  )
  expect_error(
    dt$threshold(
      ref = list(action_exercise),
      index = list(action_diet),
      outcome = "saving",
      mvd = cost_exercise$description(),
      a = cost_exercise$mean(),
      b = 1000.0,
      tol = -2.0
    ),
    class = "invalid_tol"
  )
  expect_error(
    dt$threshold(
      index = list(action_exercise),
      ref = list(action_diet),
      mvd = c.exercise$description(),
      a = c.exercise$mean(),
      b = 1000.0,
      tol = 1.0
    ),
    class = "invalid_outcome"
  )
})

## @knitr threshold -----------------------------------------------------------
cost_threshold <- dt$threshold(
  index = list(action_exercise),
  ref = list(action_diet),
  outcome = "saving",
  mvd = cost_exercise$description(),
  a = 0.0, b = 5000.0, tol = 0.1
)

success_threshold <- dt$threshold(
  index = list(action_exercise),
  ref = list(action_diet),
  outcome = "saving",
  mvd = p_exercise$description(),
  a = 0.0, b = 1.0, tol = 0.001
)

## @knitr ---------------------------------------------------------------------
test_that("thresholds are evaluated correctly", {
  expect_intol(cost_threshold, e_cost_threshold, 5.0)
  expect_intol(success_threshold, e_success_threshold, 0.03)
})
