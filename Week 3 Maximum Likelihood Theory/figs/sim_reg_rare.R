## Poisson regression, beta1 = 0 under the null: does the parabola arrive at n = 10
## when the outcome is rare? Two settings: mean count ~1.65 (intercept 0.5) and
## mean count ~0.1 (intercept -2.3). Reports Wald and LR rejection rates and the
## median profile-likelihood drop at 2 SEs below the peak (parabola: 2.00).
set.seed(723)
one_run <- function(n, b0, reps = 4000) {
  W <- LR <- drop2 <- rep(NA_real_, reps); nev <- integer(reps)
  for (r in 1:reps) {
    x <- rnorm(n); y <- rpois(n, exp(b0 + 0.3 * 0 * x))   # null: beta1 = 0
    nev[r] <- sum(y)
    if (sum(y) == 0) next
    m1 <- tryCatch(suppressWarnings(glm(y ~ x, family = poisson)), error = function(e) NULL)
    m0 <- suppressWarnings(glm(y ~ 1, family = poisson))
    if (is.null(m1) || !m1$converged) next
    b  <- coef(m1)[2]; se <- sqrt(vcov(m1)[2, 2])
    W[r]  <- (b / se)^2
    LR[r] <- 2 * as.numeric(logLik(m1) - logLik(m0))
    if (r <= 500) {
      mp <- tryCatch(suppressWarnings(glm(y ~ 1 + offset((b - 2 * se) * x), family = poisson)),
                     error = function(e) NULL)
      if (!is.null(mp)) drop2[r] <- as.numeric(logLik(m1) - logLik(mp))
    }
  }
  c(n = n, zero_events = mean(nev == 0), no_test = mean(is.na(W)), W_rej = mean(W > 3.84, na.rm = TRUE),
    LR_rej = mean(LR > 3.84, na.rm = TRUE),
    W_rej_all = mean(W > 3.84 & !is.na(W)), LR_rej_all = mean(LR > 3.84 & !is.na(LR)),
    drop2_median = median(drop2, na.rm = TRUE), mean_count = exp(b0))
}
for (b0 in c(0.5, -2.3)) {
  cat("\nintercept", b0, "mean count", round(exp(b0), 2), "\n")
  print(round(t(sapply(c(10, 50, 250, 1000), one_run, b0 = b0)), 3))
}
