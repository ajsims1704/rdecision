

# --------------------------------------------
# Tests of variable label, used in tabulations
# --------------------------------------------

test_that("the MV returns the correct environment, case 1", {
  mv <- ModelVariable$new('Variable description', 'GBP')
  v.env <- mv$get_environment()
  expect_type(v.env, 'environment')
  c.env <- rlang::current_env()
  rlang::env_print(c.env)
  print(rlang::env_parents(c.env))
  expect_identical(c.env, v.env)
})

test_that("the MV returns the correct environment, case 2", {
  mv <- ModelVariable$new('Variable description', 'GBP')
  c.env <- rlang::current_env()
  f <- function() {
    v.env <- mv$get_environment()
    expect_identical(c.env, v.env)
  }
  f()
})

test_that("an MV's label is auto-detected, case 1", {
  mv <- ModelVariable$new('Variable description', 'GBP')
  expect_equal(mv$get_label(), "mv")
})

test_that("an MV's label is auto-detected, case 2", {
  mv <- ModelVariable$new('Variable description', 'GBP')
  cmv <- mv
  expect_equal(cmv$get_label(), 'mv')
})

test_that("an MV's label is auto-detected, case 3", {
  mv <- ModelVariable$new('Variable description', 'GBP')
  f <- function() {
    fmv <- mv
    expect_equal(fmv$get_label(), 'mv')
  }
  f()
})

test_that("an MV's label is auto-detected, case 4", {
  mv1 <- ModelVariable$new('First variable', 'GBP')
  mv2 <- ModelVariable$new('Second variable', 'GBP')
  mvlist <- list(mv1, mv2)
  labels <- sapply(mvlist, FUN=function(x) {
    return(x$get_label())
  })
  expect_equal(labels, c('mv1', 'mv2'))
})

## not run - this edge condition currently fails
#test_that("an MV's label is auto-detected, case 5", {
#  f <- function() {
#    mv <- ModelVariable$new('MV in f', 'GBP')
#    return(mv)
#  }
#  g <- function(modvar) {
#    expect_equal(modvar$get_label(), 'mv')
#  }
#  pmv <- f()
#  g(pmv)
#})

test_that("an MV's label can be set and unset", {
  mv <- ModelVariable$new('Variable description', 'GBP')
  mv$set_label("mylabel")
  expect_equal(mv$get_label(), "mylabel")
  mv$unset_label()
  expect_equal(mv$get_label(), "mv")
})

