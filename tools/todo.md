# To do

## 2022-04 (from GreenLight model)

* In DecisionTree$strategy_table(), check data frame column headings. **Done**,
* In documentation for SemiMarkovModel, define units for discounts (it is a
  rate, not a percentage, e.g. 0.035 for 3.5%). **Done**.
* In documentation for SemiMarkovModel$set_probabilities(), clarify that 
  each NA is replaced with one minus the sum of the defined values. **Done**.
* In MarkovState, annual occupancy cost should default to zero. **Done**.
* In GammaModVar, add an option to fit from an estimate of mean and sd, by the
  method of moments.
* As per discussions with PC, provide a modvar tabulation function separate
  from DecisionTree/SemiMarkovModel, so that a list of ModVars (from any 
  source) can be tabulated. This requires each ModVar to have an ID assigned
  at creation, otherwise an individual ModVar cannot be identified from the
  set (map).
* In SemiMarkovModel$transition_cost, generate more informative error message
  if `e$cost()` is NA in `Ic[is,it] <- e$cost()`.
* In LogNormModVar, add an option to fit using erfi function and quantiles given
  an observed mean and 95% CI.
  
## 2021-10 Paola's comments  
  
### Target
* For clinical-background people intending to add CEA, probably assumes too 
  much stats expertise; for stats/HE people who choose to use R instead of
  existing dedicated software or Excel, it may be a bit redundant **No action**.
* For replication, it should probably include “bad” techniques (e.g. triangular
  or uniform-in-range distributions)
* For de novo analysis, perhaps see Drummond 2005 (Methods for the economic
  evaluation of health care programs: acquire?) for recommendations and 
  incorporate in help/vignettes?
* For internal EAC work: does this cover all our use cases? (Probably priority
  to address any gaps here first)

### Features
* An ICER method should probably be included (and/or related values like net
  benefit?)
* The vast majority of published CEA uses non-parametric bootstrap resampling
  of observed data (ref Drummond 2005). Should this be implemented (e.g. ModObs
  objects with similar usage as ModVars)?
* Non probabilistic sensitivity analysis (e.g. one-way): could this be 
  implemented (e.g. as a type of non-probabilistic ModVar that can become part
  of an ExpModVar)? Would need different tabulations and plotting
* More distributions (e.g. Weibull, exponential): could be implemented, or 
  write a vignette to show how to create custom classes that inherit from 
  Distribution and ModVar (or do both?)
* Method to define a distribution (of a given family) given mode/“peak”, lower
  bound, upper bound: this is the only info reported in many publications, also
  used in Briggs’ example
* More plots e.g. cost-effectiveness scatterplots, acceptability curves?

## Documentation
* In method/class documentation, type signatures of the parameters would be 
  helpful (e.g. the tornado method expects lists as index and ref, this is not
  documented anywhere) **done**.
* In method documentation, each method should include executable examples
* Some of the help is extremely obscure (e.g. parametrisation options for 
  LogNormModVar, which is also blocked in the Trust)
* In vignettes, all code (including e.g. plotting) should be displayed 
  (not everyone knows how to access the vignette source code) **no action**.
* Vignettes don’t always clarify what is being done with the package, 
  describing instead the underlying statistical model or problem
* There should be an introductory vignette on the basics of how to use the 
  package - note that the README file is not included in the installed package
  library **done**.

### Functionality/display
* Error messages could be more informative (e.g. when DT fails because not all
  nodes/edges have been passed, report something like “Node X is specified in
  the model but has not been included in the list”)
* Error: Every state must have at least one outgoing transition: this is a 
  slightly peculiar behaviour for absorbing states and should probably be 
  documented **Done**.
* The tornado plot method should return something even for point estimate 
  models (even if only a warning)
* The distribution names would be clearer with full names, not 1-2 letter 
  abbreviations
* Should the tcycle argument of SemiMarkovModel take a numeric 
  instead/additionally (and apply as.difftime internally)?
* Completely a matter of taste: I prefer the “significant” args (required 
  numerical) to be listed first, and the “descriptive” ones (e.g. label, unit)
  to be afterwards and optional (unit default to "" and label to variable name
  note that the latter is a pain to implement)
* Integer values can be specified using the Literal argument, e.g. 1L for 
  as.integer(1)

### Questions/problems
* A typical method section for prob analysis is something like

> For each variable a best estimate was determined, and a range of plausible 
  values was selected to represent the degree of uncertainty surrounding the 
  variable.

* Published work often uses one of the following:
    - sample uniformly from the feasible range as extracted from literature
    - do a not-really-probabilistic analysis of best case (use the most 
      optimistic extreme of the range for everything) and worst case (v/v) + 
      reference case (equivalent of point estimate)
    - select and report a distribution family but not the parameters (or poorly
      descriptive parameters such as mode and 95% CI/quantiles)
    - sample from bootstrapped observed data (often insufficient N…) without
      assigning a distribution
    - (more generally) any papers with genuine PSA tend to be done by people 
      with some stats background and typically have enormously complex models
      with many different distributions (incl. several not in this package) 
      that can’t be usefully replicated in a vignette
      