
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

# expectation that a sample, S, is from a Beta distribution; assumes central 
# limit theorem applies and tests whether the standard error of the mean
# and sample error of the SD are within the defined significance level. The
# test will fail at a rate of sig.level (skip for CRAN)
expect_betasample <- function(object, alpha, beta, sig.level=0.001) {
  # capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # sample size, mean and standard deviation
  n <- length(object)
  m <- mean(object)
  s <- sd(object)
  # population mean and standard deviation
  sigma <- sqrt(alpha*beta / ((alpha+beta)^2 * (alpha+beta+1)))
  mu <- alpha / (alpha+beta)
  # expect that the mean is in the CI
  mu.l <- mu - qnorm(p=1-sig.level/2)*sigma/sqrt(n)
  mu.u <- mu + qnorm(p=1-sig.level/2)*sigma/sqrt(n)
  expect(
    ok = ((m >= mu.l) && (m <= mu.u)),
    sprintf(
      "Sample mean (%f) is not within %.2f%% CI [%f,%f]", 
      m, 100*(1-sig.level), mu.l, mu.u
    )
  )
  # expect that the SD is within the CI
  sigma.l <- sigma*sqrt((n-1)/qchisq(p=1-sig.level/2, df=n-1))
  sigma.u <- sigma*sqrt((n-1)/qchisq(p=sig.level/2, df=n-1))
  expect(
    ok = ((s>=sigma.l) && (s<=sigma.u)),
    sprintf(
      "Sample SD (%f) is not within %.2f%% CI [%f,%f]",
      s, 100*(1-sig.level), sigma.l, sigma.u
    )
  ) 
  # Invisibly return the value
  invisible(act$val)    
}
