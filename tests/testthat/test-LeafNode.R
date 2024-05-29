
test_that("missing, non-character or empty labels are rejected", {
  expect_error(LeafNode$new(), class = "invalid_label")
  expect_error(LeafNode$new(42L), class = "invalid_label")
  expect_error(LeafNode$new(""), class = "invalid_label")
  expect_silent(LeafNode$new("my node"))
})

test_that("object parameters can be set dynamically", {
  t1 <- LeafNode$new(label = "poorly")
  expect_error(t1$set_utility(), class = "invalid_utility")
  expect_error(t1$set_utility(NA_real_), class = "invalid_utility")
  t1$set_utility(0.5)
  expect_intol(t1$utility(), 0.5, 0.01)
  expect_error(t1$set_interval(), class = "invalid_interval")
  expect_error(t1$set_interval(NA_real_), class = "invalid_interval")
  expect_error(t1$set_interval(42L), class = "invalid_interval")
  t1$set_interval(as.difftime(7.0, units = "days"))
  expect_intol(as.numeric(t1$interval(), units = "weeks"), 1.0, 0.01)
})

test_that("utility values and distributions are supported", {
  # invalid type
  expect_error(LeafNode$new("QALY", "ill"), class = "invalid_utility")
  # valid type, out of range
  expect_error(LeafNode$new("QALY", 2.0), class = "invalid_utility")
  # check that utility is returned
  t1 <- LeafNode$new("QALY", 0.5)
  expect_intol(t1$utility(), 0.5, 0.01)
  mv <- t1$modvars()
  expect_length(mv, 0L)
  # check that ModVars are supported
  u <- ConstModVar$new("poorly", "U", 0.25)
  t1 <- LeafNode$new("QALY", utility = u)
  expect_intol(t1$utility(), 0.25, 0.01)
  um <- ExprModVar$new("depressed", "U", rlang::quo(0.9 * u))
  t1 <- LeafNode$new("QALY", utility = um)
  mv <- t1$modvars()
  expect_length(mv, 2L)
  # check that using ModVar permits utilities > 1 (e.g. maternity)
  umat <- ConstModVar$new("Pregnant", "U", 2.0)
  t1 <- LeafNode$new("QALY", utility = umat)
  expect_intol(t1$utility(), 2.0, 0.01)
})

test_that("utility can be set and got after node is created", {
  t1 <- LeafNode$new(label = "poorly", utility = 0.75)
  expect_intol(t1$utility(), 0.75, 0.01)
  t1$set_utility(utility = 0.50)
  expect_intol(t1$utility(), 0.50, 0.01)
  expect_error(t1$set_utility("very ill"))
  expect_intol(t1$utility(), 0.50, 0.01)
  expect_length(t1$modvars(), 0L)
  t1$set_utility(utility = ConstModVar$new("poorly", "U", 0.75))
  expect_intol(t1$utility(), 0.75, 0.01)
  expect_length(t1$modvars(), 1L)
})

test_that("intervals are supported", {
  expect_error(
    LeafNode$new("QALY", utility = 1.0, interval = 42.0),
    class = "invalid_interval"
  )
  t1 <- LeafNode$new(
    "QALY", utility = 1.0, interval = as.difftime(7L, units = "days")
  )
  expect_intol(as.numeric(t1$interval(), units = "weeks"), 1.0, 0.01)
})

test_that("QALYs are calculated correctly", {
  t1 <- LeafNode$new(
    "QALY", utility = 0.5, interval = as.difftime(365.25 / 2.0, units = "days")
  )
  expect_intol(t1$QALY(), 0.25, 0.01)
})
