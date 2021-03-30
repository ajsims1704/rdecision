## Test environments
* local R installation, R 4.0.4
* ubuntu 16.04 (on travis-ci), R 4.0.4
* win-builder (devel)

## R CMD check results

1 error | 0 warnings | 2 notes

* This is a new release.

* ERROR: *test_check("rdecision"), failed tests (test-ExprModVar.R:186:3)*. 
  Package tests that involve sampling randomly from a distribution and
  comparing the results with parameters of an expected distribution have been 
  excluded when running CRAN tests because they fail at a type I error rate
  of about 0.1%.

* NOTE: *'LazyData' is specified without a 'data' directory*. In DESCRIPTION
  LazyData now set to false.

* NOTE: *Namespaces in Imports field not imported from: R6, grid, rlang, stats,
  utils. All declared Imports should be used*. import statements now added to 
   NAMESPACE file.
  
