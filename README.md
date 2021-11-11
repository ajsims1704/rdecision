
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdecision

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rdecision)](https://CRAN.R-project.org/package=rdecision)
[![Codecov test
coverage](https://codecov.io/gh/ajsims1704/rdecision/branch/master/graph/badge.svg)](https://codecov.io/gh/ajsims1704/rdecision?branch=master)
<!-- badges: end -->

The goal of `rdecision` is to provide methods for assessing health care
interventions using cohort models (decision trees and semi-Markov
models) which can be constructed using only a few lines of R code.
Mechanisms are provided for associating an uncertainty distribution with
each source variable and for ensuring transparency of the mathematical
relationships between variables. The package terminology follows Briggs
*et al* “Decision Modelling for Health Economic Evaluation”.<sup>1</sup>

## Installation

You can install the released version of rdecision from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rdecision")
```

## Examples

### A decision tree with parameter uncertainty

Consider the fictitious and idealized decision problem of choosing
between providing two forms of lifestyle advice, offered to people with
vascular disease, which reduce the risk of needing an interventional
procedure. The model has a time horizon of 1 year. The cost to a
healthcare provider of the interventional procedure (e.g. inserting a
stent) is 5000 GBP; the cost of providing the current form of lifestyle
advice, an appointment with a dietician (“diet”), is 50 GBP and the cost
of providing an alternative form, attendance at an exercise programme
(“exercise”), is 500 GBP. If the advice programme is successful, there
is no need for an interventional procedure. In a small trial of the
“diet” programme, 12 out of 68 patients (17.6%) avoided having a
procedure, and in a separate small trial of the “exercise” programme 18
out of 58 patients (31.0%) avoided the procedure. It is assumed that the
baseline characteristics in the two trials were comparable, that the
model is from the perspective of the healthcare provider and that the
utility is the same for all patients.

A decision tree can be constructed to estimate the uncertainty of the
cost difference between the two types of advice programme, due to the
finite sample sizes of each trial. The proportions of each advice
programme being successful (i.e. avoiding intervention) are represented
by model variables with uncertainties which follow Beta distributions.
Probabilities of the failure of the programmes are calculated using
expression model variables to ensure that the total probability
associated with each chance node is one.

``` r
library("rdecision")
# probabilities of programme success & failure
p.diet <- BetaModVar$new("P(diet)", "", alpha=12, beta=68-12)
p.exercise <- BetaModVar$new("P(exercise)", "", alpha=18, beta=58-18)
q.diet <- ExprModVar$new("1-P(diet)", "", rlang::quo(1-p.diet))
q.exercise <- ExprModVar$new("1-P(exercise)", "", rlang::quo(1-p.exercise))
# costs
c.diet <- 50
c.exercise <- ConstModVar$new("Cost of exercise programme", "GBP", 500)
c.stent <- 5000
```

The decision tree is constructed from nodes and edges as follows:

``` r
t.ds <- LeafNode$new("no stent")
t.df <- LeafNode$new("stent")
t.es <- LeafNode$new("no stent")
t.ef <- LeafNode$new("stent")
c.d <- ChanceNode$new("Outcome")
c.e <- ChanceNode$new("Outcome")
d <- DecisionNode$new("Programme")

e.d <- Action$new(d, c.d, cost=c.diet, label = "Diet")
e.e <- Action$new(d, c.e, cost=c.exercise, label = "Exercise")
e.ds <- Reaction$new(c.d, t.ds, p=p.diet, cost = 0, label = "success")
e.df <- Reaction$new(c.d, t.df, p=q.diet, cost=c.stent, label="failure")
e.es <- Reaction$new(c.e, t.es, p=p.exercise, cost=0, label="success")
e.ef <- Reaction$new(c.e, t.ef, p=q.exercise, cost=c.stent, label="failure")

DT <- DecisionTree$new(
  V = list(d, c.d, c.e, t.ds, t.df, t.es, t.ef),
  E = list(e.d, e.e, e.ds, e.df, e.es, e.ef)
)
```

<img src="man/figures/README-treedraw-1.png" width="75%" style="display: block; margin: auto;" />

The expected per-patient net cost of each option is obtained by
evaluating the tree with expected values of all variables using
`DT$evaluate()` and threshold values with `DT$threshold()`. Examination
of the results of evaluation shows that the expected per-patient net
cost of the diet advice programme is 4167.65 GBP and the per-patient net
cost of the exercise programme is 3948.28 GBP, a point estimate saving
of 219.37 GBP per patient if the exercise advice programme is adopted.
By univariate threshold analysis, the exercise program will be cost
saving when its cost of delivery is less than 719.73 GBP or when its
success rate is greater than 26.6%.

The confidence interval of the cost saving is estimated by repeated
evaluation of the tree, each time sampling from the uncertainty
distribution of the two probabilities using, for example,
`DT$evaluate(setvars="random", N=1000)` and inspecting the resulting
data frame. From 1000 runs, the 95% confidence interval of the per
patient cost saving is -517.42 GBP to 940.59 GBP, with 71.6% being cost
saving, and it can be concluded that more evidence is required to be
confident that the exercise programme is cost saving.

## A three-state Markov model

Sonnenberg and Beck<sup>2</sup> introduced an illustrative example of a
semi-Markov process with three states: “Well”, “Disabled” and “Dead” and
one transition between each state, each with a per-cycle probability. In
`rdecision` such a model is constructed as follows. Note that
transitions from a state to itself must be specified if allowed,
otherwise the state would be a temporary state.

``` r
# create states
s.well <- MarkovState$new(name="Well", utility=1)
s.disabled <- MarkovState$new(name="Disabled",utility=0.7)
s.dead <- MarkovState$new(name="Dead",utility=0)
# create transitions, leaving rates undefined
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
# create transition probability matrix
snames <- c("Well","Disabled","Dead")
Pt <- matrix(
  data = c(0.6, 0.2, 0.2, 0, 0.6, 0.4, 0, 0, 1),
  nrow = 3, byrow = TRUE,
  dimnames = list(source=snames, target=snames)
)
# set the transition rates from per-cycle probabilities
M$set_probabilities(Pt)
```

With a starting population of 10,000, the model can be run for 25 years
as follows. The output of the `cycles` function is the Markov trace,
shown below, which replicates Table 2.<sup>2</sup>

``` r
# set the starting populations
M$reset(c(Well=10000, Disabled=0, Dead=0)) 
# cycle
MT <- M$cycles(25, hcc.pop=FALSE, hcc.cost=FALSE)
```

| Years |  Well | Disabled |  Dead | Cumulative Utility |
|------:|------:|---------:|------:|-------------------:|
|     0 | 10000 |        0 |     0 |                  0 |
|     1 |  6000 |     2000 |  2000 |               0.74 |
|     2 |  3600 |     2400 |  4000 |              1.268 |
|     3 |  2160 |     2160 |  5680 |              1.635 |
|    23 |     0 |        1 |  9999 |              2.375 |
|    24 |     0 |        0 | 10000 |              2.375 |
|    25 |     0 |        0 | 10000 |              2.375 |

# Acknowledgements

In addition to using base R,<sup>3</sup> `redecision` relies heavily on
the `R6` implementation of classes<sup>4</sup> and the `rlang` package
for error handling and non-standard evaluation used in expression model
variables.<sup>5</sup> Building the package vignettes and documentation
relies on the `testthat` package,<sup>6</sup> the `devtools`
package<sup>7</sup> and `rmarkdown`.<sup>10</sup>

Underpinning graph theory is based on terminology, definitions and
algorithms from Gross *et al*,<sup>11</sup> the Wikipedia
glossary<sup>12</sup> and links therein. Topological sorting of graphs
is based on Kahn’s algorithm.<sup>13</sup> Some of the terminology for
decision trees was based on the work of Kaminski *et al*<sup>14</sup>
and an efficient tree drawing algorithm was based on the work of
Walker.<sup>15</sup> In semi-Markov models, representations are exported
in the DOT language.<sup>16</sup>

Terminology for decision trees and Markov models in health economic
evaluation was based on the book by Briggs *et al*<sup>1</sup> and the
output format and terminology follows ISPOR
recommendations.<sup>18</sup>

Citations for examples used in vignettes are given in applicable
vignette files.

# References

<div id="refs" class="references csl-bib-body">

<div id="ref-briggs2006" class="csl-entry">

<span class="csl-left-margin">1 </span><span
class="csl-right-inline">Briggs A, Claxton K, Sculpher M. *Decision
modelling for health economic evaluation*. Oxford, UK: Oxford University
Press; 2006.</span>

</div>

<div id="ref-sonnenberg1993" class="csl-entry">

<span class="csl-left-margin">2 </span><span
class="csl-right-inline">Sonnenberg FA, Beck JR. Markov Models in
Medical Decision Making: A Practical Guide. *Medical Decision Making*
1993;**13**:322–38. <https://doi.org/10.1177/0272989X9301300409>.</span>

</div>

<div id="ref-rcoreteam2020" class="csl-entry">

<span class="csl-left-margin">3 </span><span class="csl-right-inline">R
Core Team. *R: A language and environment for statistical computing*.
Vienna, Austria: R Foundation for Statistical Computing; 2020.</span>

</div>

<div id="ref-chang2020" class="csl-entry">

<span class="csl-left-margin">4 </span><span
class="csl-right-inline">Chang W. *R6: Encapsulated classes with
reference semantics*. 2020.</span>

</div>

<div id="ref-henry2020" class="csl-entry">

<span class="csl-left-margin">5 </span><span
class="csl-right-inline">Henry L, Wickham H. *Rlang: Functions for base
types and core r and ’tidyverse’ features*. 2020.</span>

</div>

<div id="ref-wickham2011" class="csl-entry">

<span class="csl-left-margin">6 </span><span
class="csl-right-inline">Wickham H. Testthat: Get started with testing.
*The R Journal* 2011;**3**:5–10.</span>

</div>

<div id="ref-wickham2020" class="csl-entry">

<span class="csl-left-margin">7 </span><span
class="csl-right-inline">Wickham H, Hester J, Chang W. *Devtools: Tools
to make developing r packages easier*. 2020.</span>

</div>

<div id="ref-xie2018a" class="csl-entry">

<span class="csl-left-margin">8 </span><span
class="csl-right-inline">Xie Y, Allaire JJ, Grolemund G. *R markdown:
The definitive guide*. Boca Raton, Florida: Chapman and Hall/CRC;
2018.</span>

</div>

<div id="ref-allaire2020" class="csl-entry">

<span class="csl-left-margin">9 </span><span
class="csl-right-inline">Allaire J, Xie Y, McPherson J, Luraschi J,
Ushey K, Atkins A, *et al.* *Rmarkdown: Dynamic documents for r*.
2020.</span>

</div>

<div id="ref-xie2020" class="csl-entry">

<span class="csl-left-margin">10 </span><span
class="csl-right-inline">Xie Y, Dervieux C, Riederer E. *R markdown
cookbook*. Boca Raton, Florida: Chapman and Hall/CRC; 2020.</span>

</div>

<div id="ref-gross2013" class="csl-entry">

<span class="csl-left-margin">11 </span><span
class="csl-right-inline">Gross JL, Yellen J, Zhang P. *Handbook of Graph
Theory*. 2nd ed. Chapman and Hall/CRC.; 2013.</span>

</div>

<div id="ref-wikipedia2021" class="csl-entry">

<span class="csl-left-margin">12 </span><span
class="csl-right-inline">Wikipedia. Glossary of graph theory.
*Wikipedia* 2021.</span>

</div>

<div id="ref-kahn1962" class="csl-entry">

<span class="csl-left-margin">13 </span><span
class="csl-right-inline">Kahn AB. Topological sorting of large networks.
*Communications of the ACM* 1962;**5**:558–62.
<https://doi.org/10.1145/368996.369025>.</span>

</div>

<div id="ref-kaminski2018" class="csl-entry">

<span class="csl-left-margin">14 </span><span
class="csl-right-inline">Kamiński B, Jakubczyk M, Szufel P. A framework
for sensitivity analysis of decision trees. *Central European Journal of
Operational Research* 2018;**26**:135–59.
<https://doi.org/10.1007/s10100-017-0479-6>.</span>

</div>

<div id="ref-walker1989" class="csl-entry">

<span class="csl-left-margin">15 </span><span
class="csl-right-inline">Walker JQ. *A node-positioning algorithm for
general trees*. Chapel Hill: University of North Carolina; 1989.</span>

</div>

<div id="ref-gansner1993" class="csl-entry">

<span class="csl-left-margin">16 </span><span
class="csl-right-inline">Gansner ER, Koutsofios E, North SC, Vo K-P. A
technique for drawing directed graphs. *IEEE Transactions on Software
Engineering* 1993;**19**:214–30.
<https://doi.org/10.1109/32.221135>.</span>

</div>

<div id="ref-briggs2012a" class="csl-entry">

<span class="csl-left-margin">17 </span><span
class="csl-right-inline">Briggs AH, Weinstein MC, Fenwick EAL, Karnon J,
Sculpher MJ, Paltiel AD. Model Parameter Estimation and Uncertainty: A
Report of the ISPOR-SMDM Modeling Good Research Practices Task Force-6.
*Value in Health* 2012;**15**:835–42.
<https://doi.org/10.1016/j.jval.2012.04.014>.</span>

</div>

<div id="ref-siebert2012" class="csl-entry">

<span class="csl-left-margin">18 </span><span
class="csl-right-inline">Siebert U, Alagoz O, Bayoumi AM, Jahn B, Owens
DK, Cohen DJ, *et al.* State-Transition Modeling: A Report of the
ISPOR-SMDM Modeling Good Research Practices Task Force-3. *Value in
Health* 2012;**15**:812–20.
<https://doi.org/10.1016/j.jval.2012.06.014>.</span>

</div>

</div>
