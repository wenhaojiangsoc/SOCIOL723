## Week 7 -- judge-leniency designs: leave-in vs leave-one-out leniency.
## Every number on the "Constructing the Instrument" frame comes from here.
##   Rscript figs/w7_judge.R > figs/w7_judge.out
set.seed(723)
tau <- 0.20            # true effect of incarceration on the outcome (homogeneous, to isolate bias)
J   <- 100             # judges in one court
R   <- 500             # Monte Carlo replications

one_rep <- function(nj) {
  n   <- J * nj
  j   <- rep(1:J, each = nj)
  lam <- runif(J, 0.25, 0.65)                   # judge-specific incarceration propensities
  u   <- rnorm(n)                               # unobserved dangerousness: raises D and Y
  D   <- as.integer(qnorm(lam)[j] + 0.8 * u + rnorm(n) > 0)
  Y   <- tau * D + 1.0 * u + rnorm(n)
  ## leave-in leniency = judge mean including own case = fitted value from judge dummies
  z_in  <- ave(D, j)
  ## leave-one-out leniency: judge mean of the OTHER cases
  nj_   <- ave(D, j, FUN = length)
  z_loo <- (z_in * nj_ - D) / (nj_ - 1)
  b_ols <- unname(coef(lm(Y ~ D))[2])
  b_in  <- unname(coef(AER::ivreg(Y ~ D | z_in))[2])
  b_fe  <- unname(coef(AER::ivreg(Y ~ D | factor(j)))[2])   # judge dummies as instruments (2SLS)
  b_loo <- unname(coef(AER::ivreg(Y ~ D | z_loo))[2])
  F_loo <- unname(summary(lm(D ~ z_loo))$fstatistic[1])
  c(ols = b_ols, dummies = b_fe, leave_in = b_in, loo = b_loo, F = F_loo)
}

for (nj in c(50, 200)) {
  res <- t(replicate(R, one_rep(nj)))
  cat(sprintf("\n==== %d judges x %d cases each (n = %d), true tau = %.2f, %d reps ====\n",
              J, nj, J * nj, tau, R))
  out <- rbind(mean = colMeans(res), sd = apply(res, 2, sd))
  print(round(out, 3))
  cat(sprintf("judge dummies 2SLS and leave-in leniency identical in every rep: %s\n",
              isTRUE(all.equal(unname(res[, "dummies"]), unname(res[, "leave_in"])))))
}
