## Week 6 -- sensitivity to an unobserved confounder, on the GSS.
## Every number on the Oster / sensemakr frames comes from here.
##   R_LIBS=<lib with sensemakr> Rscript figs/w6_sensitivity.R
g <- readRDS("../Data/gss_earnings.rds")
g$D <- g$ba; g$Y <- g$lnearn

## ---- 1. the two regressions Oster compares -----------------------------------
short <- lm(Y ~ D, g)                                              # no controls
long  <- lm(Y ~ D + pareduc + wordsum + female + black + otherace + exper + I(exper^2), g)
b_dot <- coef(short)["D"]; r_dot <- summary(short)$r.squared        # uncontrolled
b_til <- coef(long)["D"];  r_til <- summary(long)$r.squared         # controlled
cat(sprintf("short: beta %.3f R2 %.3f | long: beta %.3f R2 %.3f\n", b_dot, r_dot, b_til, r_til))

## ---- 2. Oster (2019): coefficient movement scaled by R2 movement --------------
## beta*(delta, Rmax) = b_til - delta * (b_dot - b_til) * (Rmax - r_til) / (r_til - r_dot)
## delta* = value of delta that drives beta* to zero
oster_beta  <- function(delta, rmax) b_til - delta * (b_dot - b_til) * (rmax - r_til) / (r_til - r_dot)
oster_delta <- function(rmax) b_til * (r_til - r_dot) / ((b_dot - b_til) * (rmax - r_til))
for (rmax in c(1.3 * r_til, 2 * r_til, 1)) {
  cat(sprintf("Rmax = %.3f: beta*(delta = 1) = %.3f ; delta* = %.2f\n",
              rmax, oster_beta(1, rmax), oster_delta(rmax)))
}

## ---- 3. Cinelli & Hazlett (2020): partial R2 and the robustness value -----------
library(sensemakr)
s <- sensemakr(model = long, treatment = "D", benchmark_covariates = "wordsum", kd = 1:3, ky = 1:3)
st <- s$sensitivity_stats
cat(sprintf("estimate %.3f se %.4f t %.1f df %d\n", st$estimate, st$se, st$t_statistic, st$dof))
cat(sprintf("partial R2 of D with Y: %.4f | RV(q=1) = %.3f | RV(q=1, alpha=.05) = %.3f\n",
            st$r2yd.x, st$rv_q, st$rv_qa))
## by hand: RV_q = 0.5 * (sqrt(f^4 + 4 f^2) - f^2), f = q |t| / sqrt(df)
f <- abs(st$t_statistic) / sqrt(st$dof)
cat(sprintf("RV by hand: %.3f\n", 0.5 * (sqrt(f^4 + 4 * f^2) - f^2)))
## benchmarks: a confounder k times as strong as wordsum
b <- s$bounds
b <- b[, c("bound_label", "r2dz.x", "r2yz.dx", "adjusted_estimate", "adjusted_lower_CI", "adjusted_upper_CI")]
b[, -1] <- round(b[, -1], 4); print(b)
## bias formula by hand for the 1x wordsum benchmark
r2d <- b$r2dz.x[1]; r2y <- b$r2yz.dx[1]
bias <- sqrt(r2y * r2d / (1 - r2d)) * st$se * sqrt(st$dof)
cat(sprintf("bias at 1x wordsum, by hand: %.4f ; adjusted estimate %.3f\n", bias, st$estimate - bias))
pdf("figs/sensemakr_contours.pdf", width = 5.2, height = 4.4)
plot(s, lim = 0.3, lim.y = 0.3)
dev.off()
