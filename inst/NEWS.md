# rdecision 1.3.0

* Added rhub workflow to git repository.
* Increased minimum version of R in DESCRIPTION to 3.6.0 to be consistent with
  functions `str2lang` and `hcl.colors` which require it, and added backport
  linter to list of linters to identify whether the minimum version of R is
  required to be changed in future builds.
* Moved packages `igraph`, `knitr`, `rmarkdown` and `testthat` to Imports
  section of DESCRIPTION to avoid R CMD build and R CMD test failures on some
  architectures via `rhub`.
* Added utility discount rate argument to `LeafNode` class, and modified
  method `QALY` in `LeafNode` to calculate QALYs gained, assuming present
  value of future utility under a continuous discount assumption.
  Revised documentation of `DecisionTree` and `LeafNode` to clarify the
  timing assumptions used by `rdecision` in decision tree modelling (that
  all costs are incurred at time t = 0, and that QALYs are gained over the
  time horizons defined in the leaf nodes).
* Added citation to O'Mahony et al (2015) to HIV vignette, and `SemiMarkovModel`
  and `DecisionTree` documentation.
* Calling methods `set_cost` and `set_utility` in class `MarkovState` without
  an argument is trapped as an error.
* Calling method `set_cost` in class `Transition` without an argument is
  trapped as an error.
* Arguments `index` and `ref` in the `tornado` method of `DecisionTree` may
  now each be expressed as a single Action edge, rather than a list of action
  edges, if there is one decision node in the tree. As many decision problems
  are a choice between two strategies, this is a common usage of `tornado`,
  and will lead to more compact code. Method `is_strategy` also modified to
  permit compact notation of index and reference strategies.
* Decision tree drawing algorithm improved to place leaf node labels to the
  right of the symbol, and to accurately calculate margin sizes required to
  accommodate symbols and labels efficiently. Node and edge grid drawing
  functions moved to classes for those objects.
* Added package qualifier for function dev.size in grDevices, and functions
  abline, axis, rect, strwidth and text in package graphics.
* Moved code to plot a tornado diagram into 'utils.R` to make it available
  for creating such diagrams given any input source (e.g., Markov models),
  rather than restricting its use to `ModVar`s in decision trees.
* Added vignette `SM00`, a narrative explanation of replicating Sonnenberg 
  and Beck's three-state model of people having a prosthetic heart valve. 
  Removed dependency of the package readme file on test-model-Lifestyle.R
  and test-model-PHV.R, and revised, simplified and corrected the readme
  file.
* Removed dependency on external file for vignette DT00 by direct inclusion of
  model code from test-model-Lifestyle.R. Test expectations replaced by
  `stopifnot` assertions to ensure that the vignette model results agree with
  published values. Replaced calls to knitr::kable with base R table output
  for purled output.
* Removed dependency on external file for vignette DT01 by direct inclusion of
  model code from test-model-Sumatriptan.R. Test expectations replaced by
  `stopifnot` assertions to ensure that the vignette model results agree with
  published values. Replaced calls to knitr::kable with base R table output
  for purled output.
* Removed dependency on external file for vignette DT03 by direct inclusion of
  model code from test-model-ShaleGas.R. Test expectations replaced by
  `stopifnot` assertions to ensure that the vignette model results agree with
  published values. Replaced calls to knitr::kable with base R table output
  for purled output.
* Removed dependency on external file for vignette GT01 by direct inclusion of
  model code from test-model-BurgerRun.R. Test expectations replaced by
  `stopifnot` assertions to ensure that the vignette model results agree with
  published values. Replaced calls to knitr::kable with base R table output
  for purled output.
* Removed dependency on external R files from vignette SM02-TKR by direct
  inclusion of model code from test-model-SM02.R. Test expectations replaced
  by `stopifnot` assertions in the vignette to ensure that vignette model
  results agree with published values. Removed test-model-SM02.R from
  testthat folder. Replaced calls to pander and knitr::kable with base R table
  output so that tables and plots are displayed when purled code is executed
  using source with echo.
* Removed dependency on external R files from vignette DT02-Tegaderm by direct
  inclusion of model code from test-model-Tegaderm.R. Test expectations replaced
  by `stopifnot` assertions in the vignette to ensure that vignette model
  results agree with published values. Removed test-model-Tegaderm.R from
  testthat folder and added extra tests to test-DecisionTree. Replaced calls to
  pander and knitr::kable with base R table output so that tables and plots are
  displayed when purled code is executed using source with echo.
* Removed dependency on external R files from vignette SM01-HIV by direct
  inclusion of model code from test-model-AZT.R. Test expectations replaced by
  `stopifnot` assertions in the vignette to ensure that vignette model results
  agree with published values. Removed test-model-AZT.R from testthat folder.
  This is because some package build and check environments break during
  vignette building if the vignette R markdown refers to external scripts.
* Moved function `gbp` from vutils.R (in vignettes) to utils.R, and removed
  vutils.R. This function is now available to the package, and vignettes no
  longer need to refer to it in an external file. Removed functions
  `gv2png` and `placeholder` which are no longer required due to use of
  `igraph` for drawing models.
* Removed suggested package dependency on `DiagrammeR` and added a suggested
  package dependency on `igraph`. Diagrams of Markov models in package vignettes
  and readme file are now drawn with package `igraph`. Extra code is required
  to produce desired appearance, compared with using GraphViz, but the resultant
  image files are compact and can be generated without the need for a command
  line tool.
* Removed argument `expressions` from `modvar_table` in classes `DecisionTree`
  and `SemiMarkovModel`. This is because operands of expressions are included
  in the table, and it makes no sense to exclude expression model variables
  which depend on those operands.
* Modified `modvars` function in various classes to use vectorization. Added
  to base classes `Node` and `Edge` to avoid type checks.
* Removed name attributes from internal representation of node and edge lists
  provided directly or indirectly to class `Graph`. This was causing errors
  under certain circumstances when creating graphs with named lists, e.g., 
  `g <- Graph(V = list(a = nodea, b = nodeb), E = list(e = edgee))` because
  internal graph logic tests using `identical` gave false negatives.  
* Added methods `as_gml` to classes `Graph` and `Digraph` for exporting of
  graphs in the Graph Modelling Language (GML). Graph representations written
  in this format can be imported to other packages, such as `igraph` to
  permit modification of nodes, edges and their attributes.
* Added parameters `rankdir`, `width` and `height` to function `as_DOT` in
  `Graph` for consistency with function of the same name in `Digraph`. Removed
  requirement for all nodes to have labels before using them by identifying
  nodes with their ID, and using the `label` attribute in the file.

# rdecision 1.2.1

* Added fields URL and BugReports to DESCRIPTION file, linking to GitHub
  repository.
* Added method `set_utility` to class `MarkovState` to allow setting
  dynamically.
* Added `get_cycle` to class `SemiMarkovModel` to query the current cycle
  number.
* Added `get_tcycle` to class `SemiMarkovModel` to query the cycle duration.
* Added argument `hcc.QALY` to methods for cycling in class `SemiMarkovModel`.
  The `hcc` arguments control whether half cycle correction is applied to the
  values written to the Markov trace but do not affect the value of the
  end-cycle population passed to the next cycle, or when the state populations
  are queried. The new argument permits half cycle correction to be applied
  independently to the trace values of state populations, costs and QALYs, which
  can be useful if, for example, it is desired to display end-cycle populations
  with mid-cycle costs and utilities.
* Revised method `draw` of class `DecisionTree` to avoid unintentional clipping
  of node labels. Modified algorithm to make better use of unit conversions
  that are implicit in package grid and to ensure that symbols for nodes retain
  the correct aspect ratio when drawn in plot windows that are not square.
* Reaction edges have a default conditional probability of 0.
* Added a test to method `evaluate` in class `DecisionTree` to check that the
  sum of probabilities for each strategy is one, for each run. Confirmed that
  the check operates correctly when the conditional probabilities of traversing
  an edge are scalars or ModVars.
* Added facility to set the probability of at most one reaction edge from each
  chance node in a decision tree to NA. The missing value is replaced at each
  evaluation of the tree by a value which ensures that the sum of probabilities
  for all reaction edges leaving the node sums to unity. This avoids the need
  to create expression model variables to represent `1 - p` where `p` is
  sampled from a Beta distribution, for example. Revised the Tegaderm vignette
  to demonstrate this feature.
* Disallowed use of some reserved words for decision node labels when added to
  a decision tree because they would conflict with column titles used during
  decision tree evaluation.
* Added checks to `Action` and `Reaction` to disallow values of NA for cost and
  benefit.
* Added `set_interval` to `LeafNode` to allow dynamic setting. Disallowed a
  utility of NA.

# rdecision 1.2.0

* Updated citation for ProbOnto in `LogNormModVar` and `LogNormDistribution`
  to reference the article in `BioInformatics`.
* Updated URLs for GraphViz and codecov.
* Escaped brackets in Rd method documentation that are not Rd macros.
* Tidied up image file names and badges used by README.md.
* Placed code for the shale gas example in a separate script with test
  expectations and referenced it from the vignette to avoid code duplication
  in the package.
* Placed code for the New Scientist digraph puzzle in a separate script with
  test expectations, and referenced it from the vignette, to avoid code
  duplication in the package.
* Placed code for Sonnenberg and Beck's canonical prosthetic heart valve example
  in a separate model script with test expectations. Referenced it in the
  second README example, to avoid code duplication in the package.
* Changed README decision tree code ("lifestyle" example) to reference code in
  a separate script (the same code used for the decision tree tutorial).
* Placed decision tree tutorial vignette code in a separate script with
  `test_that` expectations, and referenced code from the vignette's markdown.
  This avoided duplication of code between the vignette and test code.
* Added method `set_utility` to `LeafNode`.
* Changed `evaluate` method of `DecisionTree` to ensure that the components of
  strategy labels are ordered by the lexicographical order of node labels. For
  example, if there is a decision node labelled `d1` with emanating action edges
  `d1a` and `d1b`, and a decision node labelled `d2` with emanating action edges
  `d2a` and `d2b`, the set of strategy labels is `{d1a_d2a, d1a_d2b, d1b_d2a,
  d1b_d2b}`, not `{d2a_d1a, d2a_d1b, d2b_d1a, d2b_d1b}`, even if node `d1` has
  a greater index than node `d2`.
* Changed `evaluate_walks` method of `DecisionTree` to return the index of the
  leaf node for each path as the row name of the numeric matrix. Formerly it
  was coerced into a real value and saved in the matrix in a column called
  `Leaf`.
* Changed `reset` function of `SemiMarkovModel` to default to zero population
  in each state. Previously the first state was allocated 1000 people, but if
  vertexes were reordered, the first state is not the same as the first one
  provided, leading to unexpected behaviour.
* Changed `postree` method of `Arborescence` to use lexicographic order of node
  labels to define sibling order. Changed local array indexing to use `[[]]` and
  fixed unintentional use of array slicing under some circumstances.
* Replaced calls to `par` in the tornado plot section of `DecisionTree` with
  calls to `withr::with_par`. This avoids the need to change the global plot
  defaults. Package `withr` added to dependency list.
* Placed Tegaderm model vignette code in a separate script with `test_that`
  expectations, and referenced code from the vignette's markdown. This
  reduced duplication of code between vignette and test harness.
* Added functions `vertex_label` and `edge_label` to `Graph` to support checked
  iteration of lists of nodes and edges when retrieving labels.
* Vectorized functions `edge_index`, `edge_at` and `has_edge` in `Graph`.
* Vectorized functions `vertex_index`, `vertex_at` and `has_vertex` in `Graph`.
* Added functions `arrow_source` and `arrow_target` to `Digraph` to support
  checked iteration of lists of arrows to retrieve their source or target nodes.
* Placed code for the Sumatriptan vignette into a single script with tests,
  (`test-model-Sumatriptan.R`), to avoid replicating code in test scripts.
* Added `set_cost`, `set_benefit` and `set_probability` to class `Reaction` to
  allow dynamic setting of costs, benefits and probabilities in decision trees
  without the need to rebuild the model.
* Added `set_cost` and `set_benefit` to class `Action` to allow dynamic setting
  of costs and benefits in decision trees without the need to rebuild the model.
* Created `vutils.R` in folder vignettes as a home for helper functions used in
  vignette building.
* Optimised cycling speed in `SemiMarkovModel` by creating private methods
  to manage the kernel operations of cycling the population and tallying the
  costs and benefits. Approx factor of 10 improvement in speed, which is
  helpful with PSA.
* Clarified the role of `hcc.pop` and `hcc.cost` in function `cycle` of
  `SemiMarkovModel`. Removed the requirement for `hcc.pop` to be TRUE if
  `hcc.cost` is TRUE (i.e., the corrections are applied independently).
* Combined code for Chancellor model of combination therapy for HIV into a
  single script with tests (`test-model-AZT.R`), taking code from the SM01-HIV
  vignette and the SemiMarkovModel test script. Non-test chunks are referenced
  by the vignette to avoid repetition. Edited and clarified the vignette
  and added PSA (as per Briggs example 4.7). Used `DiagrammeR` to create
  Markov model diagram.
* Added vignette for total knee replacement, a semi Markov model with PSA,
  replicating Dong and Buxton, 2006. R code is taken from chunks in a 
  `test_that` context, thus avoiding repetition of code between the vignette
  and the test case.
* Code chunks in vignettes which are entirely presentational are marked as
  "purl = FALSE" to remove them from the R scripts that are generated at
  vignette build. 
* Added arguments `rankdir`, `width` and `height` in `Digraph::as_DOT` to permit
  the drawing direction and size of canvas to be adjusted in the dot syntax.
* Changed the first argument in utility functions `abortif` and `abortifnot` to
  be `...`, to increase their similarity with `stopifnot`, allowing testing
  of multiple conditions with a single call.

# rdecision 1.1.3

* Method `threshold` in `DecisionTree` now has no default value for parameter
  `outcome`.
* Method `chance_nodes` in `DecisionTree` now has the option to return nodes,
  indices or labels, consistent with similar functions for decision nodes and
  leaf nodes. 
* The incidence matrix of `Digraph` now contains integers.
* The adjacency matrices of `Graph` and `Digraph` now contain integers.
* The sample size of the empirical distribution associated with an expression
  model variable must be specified as an integer.
* Added utility function `abortif`, which is identical to `abortifnot` but
  with a negated condition. Intended to make argument checks more readable by
  avoiding double negatives.
* Function `is_probabilistic` for `ExprModVar` now returns correct result if
  the expression involves integers (previously and erroneously it returned NA).
* Function `is_probabilistic` for base class `ModVar` now returns `FALSE`
  rather then `NA`.
* Various internal improvements suggested by linter to increase efficiency
  and maintainability.
* Changed the name of function `as_value` to `as_numeric` to more accurately
  reflect its behaviour.

# rdecision 1.1.2

* Confirmed that utilities > 1 are supported in semi Markov models and added
  an example to the test suite to check it. See similar notes on `DecisionTree`.
* Removed the warning issued if a utility value of > 1 is sampled via function
  `MarkovState$utility`.
* Confirmed that utilities > 1 are supported in decision trees and added a
  fictitious model to the test suite to check it. To achieve this, the
  utility should be defined as a model variable, e.g. 
  `u <- ConstModVar$new(description = "", units = "", const = 2)` and passed as
  the `utility` argument to `LeafNode`. Scalar arguments remain subject to 
  range checking in [-Inf,1] for normal usage and to retain the previous 
  behaviour. 
* Added vectorised function `as_value` to return the value of an object if
  it is a `ModVar` (via its `get()` method) or if it is a numeric object.
  Intended as an internal function to avoid type testing on polymorphic 
  variables.
* Added vectorised function `is_ModVar` to test whether an object is a model
  variable. Intended as an internal function to the package.
* Added function `abortifnot` (a replacement for `stopifnot` using rlang but 
  with limited capability). It is a non-exported function intended
  for use in checking function arguments without increasing the cyclomatic
  complexity of the function itself.
* Clarified the documentation for argument `W` (list of walks) for 
  `DecisionTree$evaluate_walks`. Added the alternative argument `Wi` in 
  which the indices of edges in each walk are provided, to improve efficiency
  in avoiding repeated conversion between edges and indices during PSA. Added 
  function `edge_properties` to collect information for computing path products
  and sums without repeated tree walking.
* Added functions `vertex_along`, `edge_along`, `vertex_at` and `edge_at` to
  class `Graph` to clarify the relationship between nodes and edges and their
  indices, and to help optimise iterations in graph algorithms.

# rdecision 1.1.1

* Edited codecov badge reference in readme.Rmd with revised preferred URL.
* Changed citation style to one that does not write DOIs (which sometimes
  cause errors on CRAN checks).
* Changed difftime class checks to use inherits(), not class(), as per CRAN
  checks.
* Removed empty labels in \describe blocks for DecisionTree$evaluate(), as per
  new CRAN warnings.
* Improved code efficiency in `SemiMarkovModel$cycle()` by generating 
  intermediate results as matrices.
* Added Paola to the package author list.
* Added Paola's Decision Tree tutorial vignette.
* Added extra tests to the test harness for `ExprModVar` to check that nested
  autocorrelation is supported (i.e. when at least one model variable appears
  twice or more as an operand of an expression, when it is evaluated
  recursively).
* Clarified the meanings of the options to `set` for `ModVar` and `ExprModVar`
  in the documentation for those classes.
* Each test in test-ExprModVar that involves sampling has an expected failure
  rate of around 0.1% and is excluded from CRAN.
* Each `ExprModVar` now has an empirical distribution, which is sampled on 
  creation, to optimize functions `mu_hat`, `sigma_hat` and `q_hat`, at Paola's
  suggestion.
* Added class `EmpiricalDistribution` and its test harness. 
* Changed `CohortMarkovModel` to `SemiMarkovModel` in README. 
* Corrected `OccCost` and `EntryCost` columns in `SemiMarkovModel$cycle` to make
  them per person costs.
* Default occupancy cost per state set to zero in `SemiMarkovModel`.

# rdecision 1.1.0

* Added data/BriggsEx47, as example 4.7 from Briggs et al to /data.
* Added elementary semi-Markov model vignette (Chancellor).
* Added narrative to `SemiMarkovModel` as caution for converting between 
  transition rates and per-cycle probabilities. Cited work of Jones et al
  (2017) and Welton (2005) which motivated the approach taken.
* Added `set_probabilities` method to `SemiMarkovModel` to set transition 
  probabilities from a matrix. 
* Added multivariate `DirichletDistribution` class, mainly to support
  PSA in semi-Markov models.
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
* Added class `SemitMarkovModel` and its test script.
* Added class `Transition` (inherits from `Node`) and its test script.
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
