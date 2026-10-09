## Week 7 -- LIML and Fuller against 2SLS with many weak instruments.
## Every number on the "LIML and Fuller" frames comes from here.  Base R only.
##   Rscript figs/w7_liml.R > figs/w7_liml.out
## Design (same as lab 7's simulation): n = 500, one real instrument with pi = 0.5 (strong),
## confounder u with rho = 0.6, true beta = 1, plus `noise` pure-noise instruments.
set.seed(723)
kclass <- function(y, D, Z, kappa) {            # k-class estimator, constant only
  n <- length(y); X <- cbind(1, D); Zf <- cbind(1, Z)
  M <- diag(n) - Zf %*% solve(crossprod(Zf), t(Zf))
  A <- crossprod(X) - kappa * t(X) %*% M %*% X
  b <- solve(A, t(X) %*% y - kappa * t(X) %*% M %*% y)
  b[2]
}
liml_kappa <- function(y, D, Z) {               # smallest root of |W'M_1 W - kappa W'M_Z W| = 0, W = [y D]
  n <- length(y); Zf <- cbind(1, Z); W <- cbind(y, D)   # M_1 = residual maker on the constant alone
  M <- diag(n) - Zf %*% solve(crossprod(Zf), t(Zf))      # M_Z = residual maker on constant + instruments
  W1 <- scale(W, scale = FALSE)
  min(Re(eigen(solve(t(W) %*% M %*% W, crossprod(W1)))$values))
}
one <- function(n = 500, pi = 0.5, rho = 0.6, beta = 1, noise = 20) {
  z <- rnorm(n); u <- rnorm(n)
  d <- pi * z + rho * u + rnorm(n); y <- beta * d + u + rnorm(n)
  Z <- cbind(z, matrix(rnorm(n * noise), n))
  k <- liml_kappa(y, d, Z); L <- ncol(Z)
  c(ols = cov(d, y) / var(d),
    tsls_1 = kclass(y, d, z, 1),
    tsls_many = kclass(y, d, Z, 1),
    liml_many = kclass(y, d, Z, k),
    fuller_many = kclass(y, d, Z, k - 1 / (n - L - 1)),
    kappa = k)
}
for (noise in c(20, 50)) {
  r <- t(replicate(1000, one(noise = noise)))
  cat(sprintf("\n==== one strong instrument + %d noise instruments, n = 500, 1,000 draws, beta = 1 ====\n", noise))
  out <- rbind(mean = colMeans(r), median = apply(r, 2, median), sd = apply(r, 2, sd),
               `share |b-1| > 0.2` = colMeans(abs(r - 1) > 0.2))
  out[, "kappa"] <- c(mean(r[, "kappa"]), median(r[, "kappa"]), sd(r[, "kappa"]), NA)
  print(round(out, 3))
}
