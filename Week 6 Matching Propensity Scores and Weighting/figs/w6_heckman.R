## Week 6 -- Heckman coda: every number on the "Selection on Unobservables"
## frames comes from this script.  Run from the week folder:
##   Rscript figs/w6_heckman.R
## Writes figs/heckman_bias.pdf and prints the two tables used on the slides.

set.seed(723)

## ---- 1. the bias term, checked against a simulation ------------------------
## Model:  D_i = 1[c + u_i > 0],  Y_i = tau D_i + eps_i,  (u, eps) bivariate
## normal, Var(u) = 1, Var(eps) = sigma^2, Corr = rho.
## Derived:  E[eps | D=1] - E[eps | D=0] = rho sigma phi(c) / (Phi(c)(1-Phi(c))).
rho <- 0.5; sigma <- 1; tau <- 1
bias_fun <- function(c) rho * sigma * dnorm(c) / (pnorm(c) * (1 - pnorm(c)))
cat("Bias at c = 0 (formula):", round(bias_fun(0), 3), "\n")   # 0.798

N <- 1e6
u   <- rnorm(N)
eps <- sigma * (rho * u + sqrt(1 - rho^2) * rnorm(N))
D   <- as.integer(u > 0)
Y   <- tau * D + eps
cat("Naive gap, 10^6 draws:", round(mean(Y[D == 1]) - mean(Y[D == 0]), 3), "\n") # 1.80

## figure: the bias term as a function of the treated share Phi(c), rho sigma = 0.5
pdf("figs/heckman_bias.pdf", width = 4.2, height = 2.3)
par(mar = c(3.2, 3.4, 0.6, 0.6), mgp = c(1.9, 0.6, 0), cex = 0.8)
cs <- seq(-2.3, 2.3, length.out = 400)
plot(pnorm(cs), bias_fun(cs), type = "l", lwd = 2, col = "#012169",
     xlab = expression(paste("share treated  ", Phi(c))),
     ylab = expression(paste(E, "[", epsilon, " | ", D == 1, "] - ", E, "[", epsilon, " | ", D == 0, "]")),
     ylim = c(0, 3), las = 1)
abline(h = bias_fun(0), lty = 3, col = "gray50")
points(0.5, bias_fun(0), pch = 19, col = "#C84E00")
text(0.5, bias_fun(0), labels = "0.80", pos = 3, col = "#C84E00", cex = 0.9)
dev.off()

## ---- 2. what the correction costs: 1,000 replications --------------------
## Selection:  D = 1[0.5 X + g_Z Z + u > 0];  Outcome:  Y = 1 + 0.5 X + tau D + eps.
## X is the observed control (in both equations), Z the excluded variable
## (selection only).  The two-step estimator is coded by hand.
n <- 2000; R <- 1000

heckman2 <- function(dat, sel_formula_rhs) {
  ## step 1: probit of D on the selection regressors
  pr <- glm(reformulate(sel_formula_rhs, response = "D"), data = dat,
            family = binomial(link = "probit"))
  c_hat <- predict(pr, type = "link")
  ## step 2: generalized residual, then OLS with it as an extra regressor
  dat$lam <- ifelse(dat$D == 1,  dnorm(c_hat) / pnorm(c_hat),
                                -dnorm(c_hat) / (1 - pnorm(c_hat)))
  fit <- lm(Y ~ X + D + lam, data = dat)
  ## how much of the correction term do X and D already explain linearly?
  r2 <- summary(lm(lam ~ X + D, data = dat))$r.squared
  c(tau = unname(coef(fit)["D"]), rho_sigma = unname(coef(fit)["lam"]), r2 = r2)
}

one_rep <- function(skewed = FALSE) {
  X <- rnorm(n); Z <- rnorm(n)
  if (skewed) {           # common shock is a centred, scaled chi-square(1)
    v <- (rchisq(n, 1) - 1) / sqrt(2)
    u <- v
  } else u <- rnorm(n)
  eps <- sigma * (rho * u + sqrt(1 - rho^2) * rnorm(n))
  D <- as.integer(0.5 * X + 1 * Z + u > 0)
  Y <- 1 + 0.5 * X + tau * D + eps
  dat <- data.frame(Y, D, X, Z)
  ols   <- coef(lm(Y ~ X + D, data = dat))["D"]
  withZ <- heckman2(dat, c("X", "Z"))
  noZ   <- heckman2(dat, c("X"))
  c(ols = unname(ols), withZ = withZ["tau"], noZ = noZ["tau"],
    rs_withZ = withZ["rho_sigma"], r2_withZ = withZ["r2"], r2_noZ = noZ["r2"],
    share = mean(D))
}

res  <- t(replicate(R, one_rep()))
resS <- suppressWarnings(t(replicate(R, one_rep(skewed = TRUE))))

summ <- function(m, cols) {
  out <- rbind(mean = colMeans(m[, cols]), sd = apply(m[, cols], 2, sd))
  round(out, 3)
}
cat("\nNormal errors, tau = 1, rho sigma = 0.5, n =", n, ", reps =", R, "\n")
print(summ(res, c("ols", "withZ.tau", "noZ.tau")))
cat("mean coefficient on lambda (rho sigma), with Z:", round(mean(res[, "rs_withZ.rho_sigma"]), 3), "\n")
cat("R2 of lambda on X and D: with Z", round(mean(res[, "r2_withZ.r2"]), 3),
    "; without Z", round(mean(res[, "r2_noZ.r2"]), 3), "\n")
cat("mean treated share:", round(mean(res[, "share"]), 3), "\n")
cat("\nSkewed (chi-square) errors, same design\n")
print(summ(resS, c("ols", "withZ.tau", "noZ.tau")))
