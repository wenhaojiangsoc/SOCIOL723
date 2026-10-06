## Week 7 -- why the exogenous covariates W must appear in BOTH stages of 2SLS.
## Every number on the 'Why W Goes in Both Stages' frame comes from here.
##   Rscript figs/w7_twostage_W.R > figs/w7_twostage_W.out
## DAG: W -> Z, W -> D, W -> Y; Z -> D -> Y; U -> D, U -> Y (unobserved). True beta = 1.
set.seed(723)
n <- 5000; R <- 500; beta <- 1
one <- function(w_to_z) {
  W <- rnorm(n); U <- rnorm(n)
  Z <- w_to_z * W + rnorm(n)                       # W affects the instrument (or not)
  D <- 0.5 * Z + 0.8 * W + U + rnorm(n)             # W and U affect the treatment
  Y <- beta * D + 1.0 * W + U + rnorm(n)            # W and U affect the outcome
  ## (a) W in both stages: 2SLS proper (= Wald on residuals after W, by FWL)
  Dhat_a <- fitted(lm(D ~ Z + W)); a <- coef(lm(Y ~ Dhat_a + W))[2]
  ## (b) W in the second stage only: first stage on Z alone
  Dhat_b <- fitted(lm(D ~ Z));     b <- coef(lm(Y ~ Dhat_b + W))[2]
  ## (c) W in the first stage only: second stage omits W
  Dhat_c <- fitted(lm(D ~ Z + W)); cc <- coef(lm(Y ~ Dhat_c))[2]
  ## (d) no W anywhere
  Dhat_d <- fitted(lm(D ~ Z));     d <- coef(lm(Y ~ Dhat_d))[2]
  c(both = a, second_only = b, first_only = cc, neither = d, ols = coef(lm(Y ~ D + W))[2])
}
for (wz in c(0.8, 0)) {
  res <- t(replicate(R, one(wz)))
  cat(sprintf("\n==== W -> Z coefficient = %.1f (n = %d, %d reps, true beta = %d) ====\n", wz, n, R, beta))
  print(round(rbind(mean = colMeans(res), sd = apply(res, 2, sd)), 3))
}
