
test_that("illegal descriptions are rejected", {
  expect_error(ModVarSet$new(42), class="description_not_string")
  expect_error(ModVarSet$new(TRUE), class="description_not_string")
})

test_that("empty sets are allowed", {
  expect_silent(ModVarSet$new("scenario"))
  expect_silent(ModVarSet$new("scenario", list()))
})
