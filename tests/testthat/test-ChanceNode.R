

test_that("labels are checked", {
  expect_silent(ChanceNode$new())
  expect_error(ChanceNode$new(42), class="non-string_label")
  expect_silent(ChanceNode$new(""))
  expect_silent(ChanceNode$new("my node"))
})


