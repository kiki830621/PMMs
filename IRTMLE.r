IRTMLE <- function(x,
                   a,
                   b,
                   L = 2,
                   d = 1,
                   eps = .Machine$double.eps) {
  flag <- 0
  hardest <- which.max(b)
  easiest <- which.min(b)
  if (sum(x) == length(x)) {
    x[hardest] <- 0
    flag <- 1
  } else if (sum(x) == 0) {
    x[easiest] <- 1
    flag <- -1
  }
  score_function <- function(theta, b, a, X) {
    score0 <- sum(a * X) - sum(a * (plogis(a * (theta - b))))
    Score <- score0
    return(Score)
  }
  while (score_function(L, b, a, X = x) * score_function(-L, b, a, X = x) >=
         0) {
    L <- L * 2
  }
  f <- function(y) {
    S <- score_function(y, b = b, a = a, X = x)
    return(S)
  }
  R <- uniroot(f, interval = c(-L, L))
  return(R$root + flag * d)
}

# example

a <- rchisq(20, 2)
b <- rnorm(20, 0, 1)

IRTMLE(x = rep(1, 20), a = a, b = b)
IRTMLE(x = rep(0, 20), a = a, b = b)
