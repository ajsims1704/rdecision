
# -----------------------------------------------------------------------------
# Sonnenburg and Beck reference case
# -----------------------------------------------------------------------------

test_that("state names are processed", {
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  Ip <- matrix(
    data = c(  NA, 0.2, 0.2,
              0.0,  NA, 0.4,
              0.0, 0.0,  NA),
    nrow = 3,
    ncol = 3, 
    byrow = TRUE,
    dimnames = list(c("Well", "Disabled", "Dead"), c("Well", "Disabled", "Dead"))
  )
  m <- MarkovModel$new(c(s.well, s.disabled, s.dead), Ip)
  expect_equal(m$getStatenames(), c('Well', 'Disabled', "Dead"))
})



# -----------------------------------------------------------------------------
# transition matrix tests
# -----------------------------------------------------------------------------

test_that("a transition matrix with no NAs is rejected", {
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  Ip <- matrix(
    data = c( 0.6, 0.2, 0.2,
              0.0, 0.6, 0.4,
              0.0, 0.0, 1.0),
    nrow = 3,
    ncol = 3, 
    byrow = TRUE,
    dimnames = list(c("Well", "Disabled", "Dead"), c("Well", "Disabled", "Dead"))
  )
  expect_error(model <- MarkovModel$new(c(s.well, s.disabled, s.dead), Ip))
})

test_that("a transition matrix with too many NAs is rejected", {
  s.well <- MarkovState$new(name="Well")
  s.disabled <- MarkovState$new("Disabled")
  s.dead <- MarkovState$new("Dead")
  Ip <- matrix(
    data = c(  NA, 0.2, 0.2,
              0.0,  NA, 0.4,
               NA, 0.0,  NA),
    nrow = 3,
    ncol = 3, 
    byrow = TRUE,
    dimnames = list(c("Well", "Disabled", "Dead"), c("Well", "Disabled", "Dead"))
  )
  expect_error(model <- MarkovModel$new(c(s.well, s.disabled, s.dead), Ip))
})


