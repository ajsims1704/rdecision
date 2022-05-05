# To do

## 2022-04

* In DecisionTree$strategy_table(), check data frame column headings.
* In documentation for SemiMarkovModel, define units for discounts (it is a
  rate, not a percentage, e.g. 0.035 for 3.5%)
* In documentation for SemiMarkovModel$set_probabilities(), clarify that 
  each NA is replaced with one minus the sum of the defined values. DONE.
* In MarkovState, annual occupancy cost should default to zero (check if already
  changed).
* In GammaModVar, add an option to fit from an estimate of mean and sd, by the
  method of moments.
* As per discussions with PC, provide a modvar tabulation function separate
  from DecisionTree/SemiMarkovModel, so that a list of ModVars (from any 
  source) can be tabulated.
* In SemiMarkovModel$transition_cost, generate more informative error message
  if `e$cost()` is NA in `Ic[is,it] <- e$cost()`.
* In LogNormModVar, add an option to fit using erfi function and quantiles given
  an observed mean and 95% CI.