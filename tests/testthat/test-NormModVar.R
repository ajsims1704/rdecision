
test_that("illegal initializations are rejected", {
  expect_silent(NormModVar$new("norm", "GBP", 0, 1))
  expect_error(NormModVar$new(42,42,0,1), class="description_not_string")
  expect_error(NormModVar$new("norm",42,0,1), class="units_not_string")
  expect_error(NormModVar$new("norm","GBP","0",1), class="mu_not_numeric")
  expect_error(NormModVar$new("norm","GBP",0,"1"), class="sigma_not_numeric")
})

test_that("modvar is not an expression", {
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  expect_false(sn$is_expression())
})

test_that("modvar has correct distribution name", {
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  expect_equal(sn$distribution(), "N(0,1)")  
  n <- NormModVar$new("n", "GBP", 42, 1)
  expect_equal(n$distribution(), "N(42,1)")  
})
