
# is.element function for R6 objects
r6_is_element <- function(e, x) {
  return(any(vapply(x, FUN.VALUE = TRUE, FUN = identical, y = e)))
}

# setequal function for R6 objects
r6_setequal <- function(A, B) {
  # is e an element of x
  AinB <- all(
    vapply(A, FUN.VALUE = TRUE, FUN = r6_is_element, x = B)
  )
  BinA <- all(
    vapply(B, FUN.VALUE = TRUE, FUN = r6_is_element, x = A)
  )
  return(AinB & BinA)
}

# custom expectation to compare two sets of R6 objects
expect_r6_setequal <- function(object, eset) {
  # capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  # object must be a list of R6 objects
  if (!is.list(act$val)) {
    testthat::expect(
      ok = FALSE,
      sprintf("%s must be a list", act$lab)
    )
  }
  is_r6 <- vapply(act$val, FUN.VALUE = TRUE, FUN = inherits, what = "R6")
  if (!all(is_r6)) {
    testthat::expect(
      ok = FALSE,
      sprintf("%s must be a list of R6 objects", act$lab)
    )
  }
  # compare sets 
  testthat::expect(
    ok = r6_setequal(act$val, eset),
    sprintf("%s is not equal to to the expected set", act$lab)
  )
  # Invisibly return the value
  invisible(act$val)    
}

# expectation that a numeric value is equal within tolerance. The expectation
# expect_equal from testthat appears from its documentation to do exactly this,
# but seems to be configured to ignore small floating point differences. In this
# version any specified tolerance is permitted.
expect_intol <- function(object, E, tolerance) {
  # capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  # object must be numeric
  if (!is.numeric(act$val)) {
    testthat::expect(
      ok = FALSE,
      sprintf("%s must be numeric", act$lab)
    )
  }
  # test if object value is equal to E within tolerance
  testthat::expect(
    ok = (abs(act$val - E) <= tolerance),
    sprintf("%s (%f) is not within %f of %f", act$lab, act$val, tolerance, E)
  )
  # Invisibly return the value
  invisible(act$val)    
}

# expectation that object, lies in an interval (including its limits).
expect_between <- function(object, lower, upper) {
  # capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  # test
  testthat::expect(
    ok = ((act$val>=lower) && (act$val<=upper)),
    sprintf(
      "%s (%f) does not lie in the interval [%f,%f]",
      act$lab, act$val, lower, upper
    )
  ) 
  # Invisibly return the value
  invisible(act$val)    
}
