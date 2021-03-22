
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

# CI for sample mean and sample SD of n samples from Beta(alpha,beta)
beta.sampleCI <- function(alpha, beta, n, sig.level=0.001) {
  # TODO: approximate with CLT under some circumstances
  # generate distributions of sample mean and sample sd
  nsamples <- 10000
  m <- vector(mode="numeric", length=nsamples)
  s <- vector(mode="numeric", length=nsamples)
  for (i in 1:nsamples) {
    S <- rbeta(n, shape1=alpha, shape2=beta)
    m[i] <- mean(S)
    s[i] <- sd(S)
  }
  # find quantiles
  m.ci <- quantile(m, probs=c(sig.level/2, 1-sig.level/2))
  s.ci <- quantile(s, probs=c(sig.level/2, 1-sig.level/2))
  # return 
  return(list(mean.CI=m.ci, sd.CI=s.ci))
}

# CI for sample mean and sample SD of n samples from Chisq(df)
chisq.sampleCI <- function(df, n, sig.level=0.001) {
  # generate distributions of sample mean and sample sd
  nsamples <- 10000
  m <- vector(mode="numeric", length=nsamples)
  s <- vector(mode="numeric", length=nsamples)
  for (i in 1:nsamples) {
    S <- rchisq(n, df=df)
    m[i] <- mean(S)
    s[i] <- sd(S)
  }
  # find quantiles
  m.ci <- quantile(m, probs=c(sig.level/2, 1-sig.level/2))
  s.ci <- quantile(s, probs=c(sig.level/2, 1-sig.level/2))
  # return 
  return(list(mean.CI=m.ci, sd.CI=s.ci))
}

# CI for sample mean and sample SD of n samples from Gamma(k,theta)
gamma.sampleCI <- function(k, theta, n, sig.level=0.001) {
  # generate distributions of sample mean and sample sd
  nsamples <- 10000
  m <- vector(mode="numeric", length=nsamples)
  s <- vector(mode="numeric", length=nsamples)
  for (i in 1:nsamples) {
    S <- rgamma(n, shape=k, scale=theta)
    m[i] <- mean(S)
    s[i] <- sd(S)
  }
  # find quantiles
  m.ci <- quantile(m, probs=c(sig.level/2, 1-sig.level/2))
  s.ci <- quantile(s, probs=c(sig.level/2, 1-sig.level/2))
  # return 
  return(list(mean.CI=m.ci, sd.CI=s.ci))
}

# CI for sample mean and sample SD of n samples from Gamma(k,theta)
lognorm.sampleCI <- function(meanlog, sdlog, n, sig.level=0.001) {
  # generate distributions of sample mean and sample sd
  nsamples <- 10000
  m <- vector(mode="numeric", length=nsamples)
  s <- vector(mode="numeric", length=nsamples)
  for (i in 1:nsamples) {
    S <- rlnorm(n, meanlog=meanlog, sdlog=sdlog)
    m[i] <- mean(S)
    s[i] <- sd(S)
  }
  # find quantiles
  m.ci <- quantile(m, probs=c(sig.level/2, 1-sig.level/2))
  s.ci <- quantile(s, probs=c(sig.level/2, 1-sig.level/2))
  # return 
  return(list(mean.CI=m.ci, sd.CI=s.ci))
}

# CI for sample mean and sample SD of n samples from N(mu,sigma^2)
norm.sampleCI <- function(mu, sigma, n, sig.level=0.001) {
  # assume central limit theorem applies to sample mean
  m.ci <- vector(mode="numeric", length=2)
  m.ci[1] <- mu - qnorm(p=1-sig.level/2)*sigma/sqrt(n)
  m.ci[2] <- mu + qnorm(p=1-sig.level/2)*sigma/sqrt(n)
  # assume central limit applies to sample SD
  s.ci <- vector(mode="numeric", length=2)
  s.ci[1] <- sqrt(sigma^2*(qchisq(p=sig.level/2, df=n-1)/(n-1)))
  s.ci[2] <- sqrt(sigma^2*(qchisq(p=1-sig.level/2, df=n-1)/(n-1)))
  # return 
  return(list(mean.CI=m.ci, sd.CI=s.ci))
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

# expectation that object, lies in an interval (including its limits).
expect_between <- function(object, lower, upper) {
  # capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # test
  expect(
    ok = ((act$val>=lower) && (act$val<=upper)),
    sprintf(
      "%s (%f) does not lie in the interval [%f,%f]",
      act$lab, act$val, lower, upper
    )
  ) 
  # Invisibly return the value
  invisible(act$val)    
}


