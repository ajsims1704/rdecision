
test_that("missing, non-character or empty labels are rejected", {
  expect_error(LeafNode$new(), class="missing_label")
  expect_error(LeafNode$new(42), class="non-string_label")
  expect_error(LeafNode$new(""), class="empty_label")
  expect_silent(LeafNode$new("my node"))
})

test_that("utility values and distributions are supported", {
  expect_error(LeafNode$new("QALY", "ill"), class="invalid_utility")
  expect_error(LeafNode$new("QALY", 2), class="invalid_utility")
  t1 <- LeafNode$new("QALY", 0.5)
  expect_intol(t1$utility(), 0.5, 0.01)
  u <- ConstModVar$new("poorly", "U", 0.25)
  t1 <- LeafNode$new("QALY", utility=u)
  expect_intol(t1$utility(), 0.25, 0.01)
  um <- ExprModVar$new("depressed", "U", rlang::quo(0.9*u))
  t1 <- LeafNode$new("QALY", utility=um)
  mv <- t1$modvars()
  expect_length(mv,2)
})

test_that("intervals are supported", {
  expect_error(
    LeafNode$new("QALY", utility=1, interval=42), 
    class="invalid_interval"
  )
  t1 <- LeafNode$new("QALY", utility=1, interval=as.difftime(7,units="days"))
  expect_intol(as.numeric(t1$interval(), units="weeks"), 1, 0.01)
})

test_that("QALYs are calculated correctly", {
  t1 <- LeafNode$new(
    "QALY", utility=0.5, interval=as.difftime(365.25/2,units="days")
  )
  expect_intol(t1$QALY(), 0.25, 0.01)
})
