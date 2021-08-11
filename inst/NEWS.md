
# rdecision 1.0.4.9000

* Fixed `transition_probability` in `CohortMarkovModel` to ensure
  that the relative probabilities for each outgoing state are preserved
  correctly. Added note to documentation as the realtionshiop between
  rates and probabilities requires careful implementation.
* Deprecated function `r()` from `Modvar` and its subclasses. Use set/get
  methods to sample from associated uncertainty distribution.
* Refactored model variable classes into much smaller convenience classes
  with an underlying distribution. For example `BetaModVar` has a
  `BetaDistribution` uncertainty.
* Refactored `ModVar` with a "has-a" relationship to an underlying uncertainty
  distribution. Incorporated ability to link several model variables to a 
  common underlying distribution (for use with multinomial Dirichlet etc.).
* Added distribution class `DiracDistribution`. 
* Added subclasses of `Distribution` for each of the currently
  supported distributions (Beta, Normal, Log Normal, Gamma).
* Added base class `Distribution` to represent multivariate distributions.
* Added single/combined therapy HIV vignette.
* Added class `CohortMarkovModel` and its test script.
* Added class `MarkovTransition` (inherits from `Node`) and its test script.
* Added class `MarkovState` (inherits from `Edge`) and its test script.
* Self loops in digraphs have a value of zero in the incidence matrix.

# rdecision 1.0.4

* Added option `value` to method `set` in class `ModVar`. This allows
  variables to be set to an explicit value; used in threshold finding.
* Added `threshold` function to `DecisionTree` to calculate the value of a 
  model variable at which the cost difference becomes zero or ICER crosses
  a threshold.
* Added option `run` to `by` argument of `DecisionTree$evaluate()`. Avoids
  application having to `reshape` output before reporting PSA results.
* Fixed bug in method `DecisionTree$tornado` which caused bars to be clipped 
  under some circumstances.
* Minor revisions to the Tegaderm vignette.

# rdecision 1.0.3

* Package tests that involve sampling randomly from a distribution and
  comparing the results with parameters of an expected distribution have been
  excluded when running CRAN tests. Otherwise the central limit theorem or 
  empirical distributions are used to find 99.9% confidence limits on sample
  mean and SD.
* Added common test helpers and bespoke expectations to `testthat/setup.R`.
* Changed vignette titles to reflect what kind of problem they illustrate,
  rather than the problems themselves, to make it clearer on the CRAN page.
* Added method `as_DOT` to `Graph` and `Digraph` for export to graphviz DOT
  file format to aid visualization of graphs.

# rdecision 1.0.2

* Added tests to give 100% coverage and replaced `tolerance` in
  `expect_equal` with `abs` in expect_true for approximate equality tests.
* Further description for documentation.
* Converted vignettes to HTML.
* Added `WORDLIST` file and sundry administrative changes for clean package
  build.
* Added `README` file, with an example and acknowledgements.

# rdecision 1.0.1

* Added `draw()` method to `DecisionTree`.
* Added `tornado()` method to `DecisionTree` for univariate sensitivity 
  analysis.
* Optimized probabilistic sensitivity analysis loop in `DecisionTree` 
  (1000 evaluations of a typical HTA tree takes < 5s on a typical PC).

# rdecision 1.0.0

* First full release of the package.
* Added graph theory classes. Decision trees and Markov models are
  forms of graph.
* Renamed `ModelVariable` as `ModVar` for compactness, and renamed its derived
  classes similarly.
* Added test harnesses for more classes.
* Collected vignette citations to file references.bib and changed 
  to *BMJ* csl style.
* Added extra graph theory and decision tree vignettes.  
  
# rdecision 0.1.7

* Removed the label argument from `ModelVariable`. 
* Improved auto-detection of variable label in `ModelVariable`.
* Added NEWS.md and `CITATION` file to inst folder in CRAN preparation.
* Added `tests/testthat` folder with tests for `ModelVariable`.
* Added scripts to call devtools::check/build on Windows/Mac.
* Fixed notes issued by R CMD check.

# rdecision 0.1.6

* Introduced the `ModelVariable` class as the new base class from which to 
  construct the variables in an economic model. The class includes
  methods to support parametrization of uncertainty in the model variable.
* Introduced sub-classes of `ModelVariable` to model particular forms
  of uncertainty. These are `ConstModelVariable`, `NormalModelVariable`,
  `GammaModelVariable`, `BetaModelVariable`, `LogNormalModelVariable`. They
  do as expected from their names. Some support alternative forms
  of parametrization.
* Introduced `ExpressionModelVariable`. A sub-class of `ModelVariable`, objects
  of this class are defined with an expression involving other model 
  variables. The concept permits variables to be combined in any mathematical
  expression that R itself will support. Because `ExpressionModelVariable`s are
  themselves `ModelVariables`, they can can appear in an expression that 
  is used to define another model variable.
* Introduced tabulation functions to list the properties of a model variable
  and its operands.
* Revised `Node` and its sub classes to accept `ModelVariables` as arguments
  to costs, utilities and probabilities, thus embedding probabilistic
  sensitivity analysis into decision tree models.
* Added the Tegaderm vignette. This is a published example of a decision tree
  model with PSA and is partial validation of the `ModelVariable` approach
  to PSA.
* Updated the Sumatriptan vignette, after subsuming some of its pathway
  traversal code into `Node` classes.
* Removed `node.apply` and `path.apply` functions, and subsumed them into 
  `Node`.
* Removed functions intended for use with `node.apply` and `path.apply`, and
  subsumed them into `Node`.
* Provided `Node` objects with a Document Object Model (DOM) interface, as
  far as practicable.
  
# rdecision 0.1.3

* Moved citations in vignettes from external file `references.bib` to directly
  embed them in the YAML headers. To do: explore whether references can be
  saved in preferred bib format.
* Replaced call to `nullfile()`, for suppressed output, in function `des` with
  detection of OS to support older R versions (`nullfile` was introduced to
  base R at 3.6.0).

# rdecision 0.1.2

* For the Markov solver:
    * Function is now called `des`
    * It returns a list of summary matrices (the same ones written to csv files) 
      instead of a single number.
    * Output can be suppressed by setting stub=NA.
    * Some minor bugs fixed.
    
# rdecision 0.1.1

* First local release of rdecision as a package.
* Added classes for solving decision trees (`Node`, `LeafNode`, `ChanceNode`,
  `DecisionNode`) and pathway detection and traversal functions.
* Incorporated our discrete event solver, originally written in Matlab
  for the WatchBP model, then translated as a stand-alone R script, into
  the package.
* Added vignettes for Sumatriptan model from Briggs (Box 2.3) and
  from Sonnenberg and Beck's original 3-state example.
