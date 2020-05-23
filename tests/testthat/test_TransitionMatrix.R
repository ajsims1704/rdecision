
# ----------------
# state name tests
# ----------------

test_that("state names are processed", {
  names <- c("Well", "Disabled", "Dead")
  Ip <- TransitionMatrix$new(names)
  expect_equal(Ip$get_statenames(), names)
})

test_that("illegal state names are rejected, case 1", {
  names <- c("Well", "Disabled", NA)
  expect_error(TransitionMatrix$new(names))
})

test_that("illegal state names are rejected, case 2", {
  names <- c("Well", "Disabled", 42)
  expect_error(TransitionMatrix$new(names))
})
