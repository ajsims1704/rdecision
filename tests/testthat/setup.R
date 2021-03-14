
# setequal function for R6 objects
R6setequal <- function(A,B) {
  AinB <- all(sapply(A, function(a) {
    return(any(sapply(B, function(b){identical(a,b)})))
  }))
  BinA <- all(sapply(B, function(b) {
    return(any(sapply(A, function(a){identical(a,b)})))
  }))
  return(AinB & BinA)
}

# custom expectation to compare two sets of R6 objects
expect_R6setequal <- function(object, eset) {
  # capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # object must be a list of R6 objects
  if (!is.list(act$val)) {
    expect(
      ok = FALSE,
      sprintf("%s must be a list", act$lab)
    )
  }
  if (!all(sapply(act$val, function(r){inherits(r,what="R6")}))) {
    expect(
      ok = FALSE,
      sprintf("%s must be a list of R6 objects", act$lab)
    )
  }
  # compare sets 
  expect(
    ok = R6setequal(act$val, eset),
    sprintf("%s is not equal to to the expected set", act$lab)
  )
  # Invisibly return the value
  invisible(act$val)    
}

# expectation that a numeric object is within a range
expect_inrange <- function(object, lower, upper) {
  # capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # object must be numeric
  if (!is.numeric(act$val)) {
    expect(
      ok = FALSE,
      sprintf("%s must be numeric", act$lab)
    )
  }
  # test if in range
  expect(
    ok = ((act$val >= lower) && (act$val <= upper)),
    sprintf(
      "%s (%f) is not in the range [%f,%f]", act$lab, act$val, lower, upper
    )
  )
  # Invisibly return the value
  invisible(act$val)    
}

# expectation that a numeric value is equal within tolerance
expect_intol <- function(object, E, tolerance) {
  # capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # object must be numeric
  if (!is.numeric(act$val)) {
    expect(
      ok = FALSE,
      sprintf("%s must be numeric", act$lab)
    )
  }
  # test if object value is equal to E within tolerance
  expect(
    ok = (abs(act$val - E) < tolerance),
    sprintf("%s is not within %f of %f", act$lab, tolerance, E)
  )
  # Invisibly return the value
  invisible(act$val)    
}