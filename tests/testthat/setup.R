
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

# expectation that a numeric value is equal within tolerance. The expectation
# expect_equal from testthat appears from its documentation to do exactly this,
# but seems to be configured to ignore small floating point differences. In this
# version any specified tolerance is permitted.
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
    ok = (abs(act$val - E) <= tolerance),
    sprintf("%s is not within %f of %f", act$lab, tolerance, E)
  )
  # Invisibly return the value
  invisible(act$val)    
}

# expectation that the mean of a sample, object, is from a Normal distribution
# with mean mu and standard deviation sigma. Can be used for any sampling
# distribution provided the central limit theorem applies. Tests whether the
# standard error of the mean is within the confidence interval defined by the
# significance level (i.e. the test will fail with a type I error at a rate of
# sig.level, skip for CRAN).
expect_samplemean <- function(object, mu, sigma, sig.level=0.001) {
  # capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # sample size, mean and standard deviation
  n <- length(object)
  m <- mean(object)
  # expect that the mean is in the CI
  mu.l <- mu - qnorm(p=1-sig.level/2)*sigma/sqrt(n)
  mu.u <- mu + qnorm(p=1-sig.level/2)*sigma/sqrt(n)
  expect(
    ok = ((m >= mu.l) && (m <= mu.u)),
    sprintf(
      "Sample mean (%f) is not within %.2f%% CI [%f,%f] for %i samples", 
      m, 100*(1-sig.level), mu.l, mu.u, n
    )
  )
  # Invisibly return the value
  invisible(act$val)    
}

# expectation that the SD of a sample, object, is from a Normal distribution
# with standard deviation sigma. Can be used for any sampling distribution
# provided the central limit theorem applies. Tests whether the standard error 
# of the SD is within the confidence interval defined by the significance level.
# The test will fail (type I error) at a rate of sig.level (skip for CRAN).
expect_sampleSD <- function(object, sigma, sig.level=0.001) {
  # capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # sample size, mean and standard deviation
  n <- length(object)
  s <- sd(object)
  # expect that the SD is within the CI
  sigma.l <- sigma*sqrt((n-1)/qchisq(p=1-sig.level/2, df=n-1))
  sigma.u <- sigma*sqrt((n-1)/qchisq(p=sig.level/2, df=n-1))
  expect(
    ok = ((s>=sigma.l) && (s<=sigma.u)),
    sprintf(
      "Sample SD (%f) is not within %.2f%% CI [%f,%f] for %i samples",
      s, 100*(1-sig.level), sigma.l, sigma.u, n
    )
  ) 
  # Invisibly return the value
  invisible(act$val)    
}

