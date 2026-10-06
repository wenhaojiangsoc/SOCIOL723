## Week 7 -- inference under weak instruments: why the t-test fails, why Anderson-Rubin does not.
## Every number on the 'Inference Under Weak Instruments' frames comes from here.
##   Rscript figs/w7_weakiv.R > figs/w7_weakiv.out ; writes figs/weakiv_sim.pdf
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
if (!requireNamespace("ivreg", quietly = TRUE))
  install.packages("ivreg", lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)

## ---- scalar tools (one Y, one D, one Z; homoskedastic formulas) -----------------
ar_stat <- function(y, D, Z, b0) {           # regress y - b0 D on Z, squared t of the Z slope
  w <- y - b0 * D; zt <- Z - mean(Z); n <- length(y)
  g <- sum(zt * w) / sum(zt^2); e <- w - mean(w) - g * zt
  g^2 * sum(zt^2) / (sum(e^2) / (n - 2))
}
iv_fit <- function(y, D, Z) {
  zt <- Z - mean(Z); dt <- D - mean(D); n <- length(y)
  b <- sum(zt * y) / sum(zt * D); e <- y - mean(y) - b * dt
  se <- sqrt(sum(e^2) / (n - 2) * sum(zt^2)) / abs(sum(zt * dt))
  pi <- sum(zt * D) / sum(zt^2); v <- D - mean(D) - pi * zt
  F1 <- pi^2 * sum(zt^2) / (sum(v^2) / (n - 2))
  c(b = b, se = se, F = F1)
}

## ---- 1. simulation: true beta = 1, endogeneity corr(eps, v) = 0.9, n = 500 -----
## AR set in closed form (homoskedastic): AR(b0) <= crit is a quadratic inequality in b0
n <- 500; R <- 4000; beta <- 1; crit <- qchisq(0.95, 1)
ar_set <- function(y, D, Z) {
  zt <- Z - mean(Z); S <- sum(zt^2)
  g <- sum(zt * y) / S; pi <- sum(zt * D) / S
  ey <- y - mean(y) - g * zt; ed <- D - mean(D) - pi * zt
  syy <- sum(ey^2); syd <- sum(ey * ed); sdd <- sum(ed^2)
  a <- pi^2 * S * (n - 2) - crit * sdd
  b <- -2 * g * pi * S * (n - 2) + 2 * crit * syd
  cc <- g^2 * S * (n - 2) - crit * syy
  disc <- b^2 - 4 * a * cc
  ## a > 0: bounded interval [r1, r2]; a < 0 and two roots: the line MINUS (r1, r2); else the whole line
  if (a > 0) { r <- sort((-b + c(-1, 1) * sqrt(max(disc, 0))) / (2 * a)); c(bounded = 1, lo = r[1], hi = r[2], gap_lo = NA, gap_hi = NA) }
  else if (disc >= 0) { r <- sort((-b + c(-1, 1) * sqrt(disc)) / (2 * a)); c(bounded = 0, lo = -Inf, hi = Inf, gap_lo = r[1], gap_hi = r[2]) }
  else c(bounded = 0, lo = -Inf, hi = Inf, gap_lo = NA, gap_hi = NA)
}
covers <- function(s, b) {              # is b in the AR set?
  if (s[["bounded"]] == 1) return(as.numeric(s[["lo"]] <= b & b <= s[["hi"]]))
  if (is.na(s[["gap_lo"]])) return(1)
  as.numeric(b <= s[["gap_lo"]] | b >= s[["gap_hi"]])
}
sim <- function(pi) {
  out <- t(replicate(R, {
    Z <- rnorm(n); v <- rnorm(n); eps <- 0.9 * v + sqrt(1 - 0.81) * rnorm(n)
    D <- pi * Z + v; y <- beta * D + eps
    f <- iv_fit(y, D, Z); s <- ar_set(y, D, Z)
    c(b = unname(f["b"]), F = unname(f["F"]),
      wald_cover = as.numeric(abs(f[["b"]] - beta) <= 1.96 * f[["se"]]),
      wald_width = 2 * 1.96 * f[["se"]],
      ar_cover = covers(s, beta),
      ar_bounded = s[["bounded"]], ar_width = s[["hi"]] - s[["lo"]])
  }))
  bd <- out[, "ar_bounded"] == 1
  c(median_F = median(out[, "F"]), median_bias = median(out[, "b"]) - beta,
    wald_cover = mean(out[, "wald_cover"]), AR_cover = mean(out[, "ar_cover"]),
    AR_bounded = mean(bd),
    wald_width = median(out[, "wald_width"]), AR_width_bounded = median(out[bd, "ar_width"]))
}
pis <- c(weak = 0.06, moderate = 0.11, decent = 0.14, strong = 0.45)
tab <- t(sapply(pis, sim))
cat("OLS probability limit in this design: beta +", 0.9, "\n")
print(round(tab, 3))

## ---- 2. Card's data: AR statistic over a grid, bivariate (log wage, education, near4)
library(ivreg); data("SchoolingReturns", package = "ivreg")
d <- transform(SchoolingReturns, lwage = log(wage), near4 = as.integer(nearcollege4 != "none"))
f <- iv_fit(d$lwage, d$education, d$near4)
grid <- seq(-0.1, 0.5, by = 0.001)
ar_card <- sapply(grid, function(b0) ar_stat(d$lwage, d$education, d$near4, b0))
set_card <- range(grid[ar_card <= crit])
cat(sprintf("\nCard: IV %.3f (se %.4f), Wald 95%% CI [%.3f, %.3f]; homoskedastic first-stage F %.1f\n",
            f["b"], f["se"], f["b"] - 1.96 * f["se"], f["b"] + 1.96 * f["se"], f["F"]))
cat(sprintf("Card: AR 95%% set [%.3f, %.3f]; AR at +-infinity -> F = %.1f\n", set_card[1], set_card[2], f["F"]))

## ---- 3. one weak draw for the picture (pi = 0.06): AR set unbounded -----------------
set.seed(7)
Z <- rnorm(n); v <- rnorm(n); eps <- 0.9 * v + sqrt(0.19) * rnorm(n); D <- 0.06 * Z + v; y <- beta * D + eps
fw <- iv_fit(y, D, Z); gridw <- seq(-40, 40, by = 0.02)
ar_w <- sapply(gridw, function(b0) ar_stat(y, D, Z, b0))
rej <- gridw[ar_w > crit]
cat(sprintf("weak draw: IV %.2f (se %.2f), Wald 95%% CI [%.2f, %.2f], first-stage F %.2f, max AR on the grid %.2f; %s\n",
            fw["b"], fw["se"], fw["b"] - 1.96 * fw["se"], fw["b"] + 1.96 * fw["se"], fw["F"], max(ar_w),
            if (length(rej) == 0) "AR set = the whole real line" else
              sprintf("AR set = everything except [%.2f, %.2f]", min(rej), max(rej))))

## ---- figure -------------------------------------------------------------------------
set.seed(723)
dens <- function(pi) replicate(R, { Z <- rnorm(n); v <- rnorm(n); eps <- 0.9 * v + sqrt(0.19) * rnorm(n)
  D <- pi * Z + v; iv_fit(beta * D + eps, D, Z)["b"] })
b_weak <- dens(0.06); b_strong <- dens(0.45)
pdf("figs/weakiv_sim.pdf", width = 10, height = 3.4, family = "Times")
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), cex = 1.0)
plot(density(b_strong, from = -0.5, to = 2.5), col = "navy", lwd = 2, xlim = c(-0.5, 2.5),
     main = "A. sampling distribution", xlab = expression(hat(beta)[IV]), ylab = "density")
lines(density(b_weak, bw = 0.08, from = -0.5, to = 2.5), col = "firebrick", lwd = 2); abline(v = beta, lty = 3)
abline(v = beta + 0.9, lty = 2, col = "gray40")
legend("topright", c(expression(F %~~% 100), expression(F %~~% 2), "OLS limit"),
       col = c("navy", "firebrick", "gray40"), lty = c(1, 1, 2), lwd = c(2, 2, 1), bty = "n")
plot(grid, ar_card, type = "l", col = "navy", lwd = 2, ylim = c(0, 25), main = "B. Card: bounded",
     xlab = expression(beta[0]), ylab = expression(AR(beta[0])))
abline(h = crit, lty = 3)
plot(gridw, ar_w, type = "l", col = "firebrick", lwd = 2, ylim = c(0, 12), main = "C. weak draw: unbounded",
     xlab = expression(beta[0]), ylab = expression(AR(beta[0])))
abline(h = crit, lty = 3); abline(h = fw["F"], lty = 2, col = "gray40")
text(25, fw["F"] + 0.8, sprintf("F = %.1f", fw["F"]), cex = 0.8)
dev.off()

## ---- 4. the AR statistic step by step on Card's data, for the slide table -----------
cat("\n==== Card, step by step: m(b0) = Cov_n(Z, Y - b0 D), slope of (Y - b0 D) on Z, C(b0) ====\n")
y <- d$lwage; D <- d$education; Z <- d$near4; n <- length(y)
cat(sprintf("Cov_n(Z,Y) = %.4f, Cov_n(Z,D) = %.4f, Var_n(Z) = %.4f, ratio = %.4f\n",
            cov(Z, y), cov(Z, D), var(Z), cov(Z, y) / cov(Z, D)))
for (b0 in c(0, 0.10, 0.144, 0.188, 0.25, 0.30)) {
  U <- y - b0 * D; m <- cov(Z, U); sl <- coef(lm(U ~ Z))[2]; tt <- summary(lm(U ~ Z))$coefficients[2, 3]
  cat(sprintf("b0 = %.3f: m(b0) = %7.4f  slope = %7.4f  t = %6.2f  C = t^2 = %6.2f  %s\n",
              b0, m, sl, tt, tt^2, ifelse(tt^2 > crit, "reject", "keep")))
}
