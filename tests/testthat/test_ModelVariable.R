


test_that("the MV returns the correct environment", {
  mv <- ModelVariable$new(NA, 'Variable description', 'GBP')
  c.env <- rlang::current_env()
  expect_type(c.env, 'environment')
  v.env <- mv$junk()
  expect_type(v.env, 'environment')
  #expect_identical(c.env, v.env)
})

test_that("an MV's label is detected from its environment", {
  mv <- ModelVariable$new(NA, 'Variable description', 'GBP')
  expect_equal(mv$get_label(), "mv")
})

#test_that("Auto-detect label in different environment", {
#  mv <- ModelVariable$new(NA, 'Variable description', 'GBP')
#  f <- function() {
#    expect_equal(mv$get_label(), "mv")
#  }
#  f()  
#})