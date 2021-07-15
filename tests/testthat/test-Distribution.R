
test_that("Invalid base distributions are rejected", {
  # missing name
  expect_error(
    Distribution$new(),
    class = "invalid_name"
  )
  # invalid name
  expect_error(
    Distribution$new(name=42),
    class = "invalid_name"
  )
  # invalid dimensions
  expect_error(
    Distribution$new(name="Base", k=1),
    class = "invalid_order"
  )
  expect_error(
    Distribution$new(name="Base", k=as.integer(0)),
    class = "order_not_supported"
  )
  # valid dimension
  expect_silent(
    Distribution$new(name="Base", k=as.integer(3))
  )
  
})