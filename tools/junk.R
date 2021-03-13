
N <- 100000
m <- vector(mode="numeric", length=N)
s <- vector(mode="numeric", length=N)
for (i in 1:N) {
  S <- rbeta(n=1000, shape1=9, shape2=1)
  m[i] <- mean(S)
  s[i] <- sd(S)
}
print(quantile(m, probs=c(0.005, 0.995)))
print(quantile(s, probs=c(0.005, 0.995)))