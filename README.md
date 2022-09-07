
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdecision

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rdecision)](https://CRAN.R-project.org/package=rdecision)
[![codecov](https://codecov.io/github/ajsims1704/rdecision/branch/master/graph/badge.svg?token=HHZXK56ZAR)](https://codecov.io/github/ajsims1704/rdecision)
<!-- badges: end -->

The goal of `rdecision` is to provide methods for assessing health care
interventions using cohort models (decision trees and semi-Markov
models) which can be constructed using only a few lines of R code.
Mechanisms are provided for associating an uncertainty distribution with
each source variable and for ensuring transparency of the mathematical
relationships between variables. The package terminology follows Briggs
*et al* “Decision Modelling for Health Economic Evaluation”<sup>1</sup>.

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
patient cost saving is -529.95 GBP to 1006.12 GBP, with 71.6% being cost
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

<img src="man/figures/README-sb-1.png" width="75%" style="display: block; margin: auto;" />

With a starting population of 10,000, the model can be run for 25 years
as follows. The output of the `cycles` function is the Markov trace,
shown below, which replicates Table 2<sup>2</sup>.

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

In addition to using base R<sup>3</sup>, `redecision` relies heavily on
the `R6` implementation of classes<sup>4</sup> and the `rlang` package
for error handling and non-standard evaluation used in expression model
variables<sup>5</sup>. Building the package vignettes and documentation
relies on the `testthat` package<sup>6</sup>, the `devtools`
package<sup>7</sup> and `rmarkdown`<sup>10</sup>.

Underpinning graph theory is based on terminology, definitions and
algorithms from Gross *et al*<sup>11</sup>, the Wikipedia
glossary<sup>12</sup> and links therein. Topological sorting of graphs
is based on Kahn’s algorithm<sup>13</sup>. Some of the terminology for
decision trees was based on the work of Kaminski *et al*<sup>14</sup>
and an efficient tree drawing algorithm was based on the work of
Walker<sup>15</sup>. In semi-Markov models, representations are exported
in the DOT language<sup>16</sup>.

Terminology for decision trees and Markov models in health economic
evaluation was based on the book by Briggs *et al*<sup>1</sup> and the
output format and terminology follows ISPOR
recommendations<sup>18</sup>.

Citations for examples used in vignettes are given in applicable
vignette files.

# References

<div id="refs" class="references csl-bib-body" line-spacing="2">

<div id="ref-briggs2006" class="csl-entry">

<span class="csl-left-margin">1. </span><span
class="csl-right-inline">Briggs, A., Claxton, K. & Sculpher, M.
*Decision modelling for health economic evaluation*. (Oxford University
Press, 2006).</span>

</div>

<div id="ref-sonnenberg1993" class="csl-entry">

<span class="csl-left-margin">2. </span><span
class="csl-right-inline">Sonnenberg, F. A. & Beck, J. R. [Markov Models
in Medical Decision Making: A Practical
Guide](https://doi.org/10.1177/0272989X9301300409). *Medical Decision
Making* **13,** 322–338 (1993).</span>

</div>

<div id="ref-rcoreteam2020" class="csl-entry">

<span class="csl-left-margin">3. </span><span class="csl-right-inline">R
Core Team. *R: A language and environment for statistical computing*. (R
Foundation for Statistical Computing, 2020). at
\<<https://www.R-project.org/>\></span>

</div>

<div id="ref-chang2020" class="csl-entry">

<span class="csl-left-margin">4. </span><span
class="csl-right-inline">Chang, W. *R6: Encapsulated classes with
reference semantics*. (2020). at
\<<https://CRAN.R-project.org/package=R6>\></span>

</div>

<div id="ref-henry2020" class="csl-entry">

<span class="csl-left-margin">5. </span><span
class="csl-right-inline">Henry, L. & Wickham, H. *Rlang: Functions for
base types and core r and ’tidyverse’ features*. (2020). at
\<<https://CRAN.R-project.org/package=rlang>\></span>

</div>

<div id="ref-wickham2011" class="csl-entry">

<span class="csl-left-margin">6. </span><span
class="csl-right-inline">Wickham, H. [Testthat: Get started with
testing](https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf).
*The R Journal* **3,** 5–10 (2011).</span>

</div>

<div id="ref-wickham2020" class="csl-entry">

<span class="csl-left-margin">7. </span><span
class="csl-right-inline">Wickham, H., Hester, J. & Chang, W. *Devtools:
Tools to make developing r packages easier*. (2020). at
\<<https://CRAN.R-project.org/package=devtools>\></span>

</div>

<div id="ref-xie2018a" class="csl-entry">

<span class="csl-left-margin">8. </span><span
class="csl-right-inline">Xie, Y., Allaire, J. J. & Grolemund, G. *R
markdown: The definitive guide*. (Chapman and Hall/CRC, 2018). at
\<<https://bookdown.org/yihui/rmarkdown>\></span>

</div>

<div id="ref-allaire2020" class="csl-entry">

<span class="csl-left-margin">9. </span><span
class="csl-right-inline">Allaire, J., Xie, Y., McPherson, J., Luraschi,
J., Ushey, K., Atkins, A., Wickham, H., Cheng, J., Chang, W. & Iannone,
R. *Rmarkdown: Dynamic documents for r*. (2020). at
\<<https://github.com/rstudio/rmarkdown>\></span>

</div>

<div id="ref-xie2020" class="csl-entry">

<span class="csl-left-margin">10. </span><span
class="csl-right-inline">Xie, Y., Dervieux, C. & Riederer, E. *R
markdown cookbook*. (Chapman and Hall/CRC, 2020). at
\<<https://bookdown.org/yihui/rmarkdown-cookbook>\></span>

</div>

<div id="ref-gross2013" class="csl-entry">

<span class="csl-left-margin">11. </span><span
class="csl-right-inline">Gross, J. L., Yellen, J. & Zhang, P. *Handbook
of Graph Theory*. (Chapman and Hall/CRC., 2013). at
\<<https://doi.org/10.1201/b16132>\></span>

</div>

<div id="ref-wikipedia2021" class="csl-entry">

<span class="csl-left-margin">12. </span><span
class="csl-right-inline">Wikipedia. Glossary of graph theory.
*Wikipedia* (2021). at
\<<https://en.wikipedia.org/wiki/Glossary_of_graph_theory>\></span>

</div>

<div id="ref-kahn1962" class="csl-entry">

<span class="csl-left-margin">13. </span><span
class="csl-right-inline">Kahn, A. B. [Topological sorting of large
networks](https://doi.org/10.1145/368996.369025). *Communications of the
ACM* **5,** 558–562 (1962).</span>

</div>

<div id="ref-kaminski2018" class="csl-entry">

<span class="csl-left-margin">14. </span><span
class="csl-right-inline">Kamiński, B., Jakubczyk, M. & Szufel, P. [A
framework for sensitivity analysis of decision
trees](https://doi.org/10.1007/s10100-017-0479-6). *Central European
Journal of Operational Research* **26,** 135–159 (2018).</span>

</div>

<div id="ref-walker1989" class="csl-entry">

<span class="csl-left-margin">15. </span><span
class="csl-right-inline">Walker, J. Q. *A node-positioning algorithm for
general trees*. (University of North Carolina, 1989). at
\<<http://www.cs.unc.edu/techreports/89-034.pdf>\></span>

</div>

<div id="ref-gansner1993" class="csl-entry">

<span class="csl-left-margin">16. </span><span
class="csl-right-inline">Gansner, E. R., Koutsofios, E., North, S. C. &
Vo, K.-P. [A technique for drawing directed
graphs](https://doi.org/10.1109/32.221135). *IEEE Transactions on
Software Engineering* **19,** 214–230 (1993).</span>

</div>

<div id="ref-briggs2012a" class="csl-entry">

<span class="csl-left-margin">17. </span><span
class="csl-right-inline">Briggs, A. H., Weinstein, M. C., Fenwick, E. A.
L., Karnon, J., Sculpher, M. J. & Paltiel, A. D. [Model Parameter
Estimation and Uncertainty: A Report of the ISPOR-SMDM Modeling Good
Research Practices Task
Force-6](https://doi.org/10.1016/j.jval.2012.04.014). *Value in Health*
**15,** 835–842 (2012).</span>

</div>

<div id="ref-siebert2012" class="csl-entry">

<span class="csl-left-margin">18. </span><span
class="csl-right-inline">Siebert, U., Alagoz, O., Bayoumi, A. M., Jahn,
B., Owens, D. K., Cohen, D. J. & Kuntz, K. M. [State-Transition
Modeling: A Report of the ISPOR-SMDM Modeling Good Research Practices
Task Force-3](https://doi.org/10.1016/j.jval.2012.06.014). *Value in
Health* **15,** 812–820 (2012).</span>

</div>

</div>
