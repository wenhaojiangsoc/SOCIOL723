## Week 6 -- continuous X: fuzzy matching versus extrapolation.
## Every number on the "Fuzzy Matching" and "Extrapolation" frames comes from here.
##   Rscript figs/w6_fuzzy.R
set.seed(723)
n <- 4000
X <- runif(n)
e <- pnorm((X - 0.6) / 0.12)            # steep; overlap roughly x in [0.3, 0.9]; no controls above 0.9
D <- rbinom(n, 1, e)
m0 <- function(x) 3 * x^2               # curved control surface
tauf <- function(x) 2 * x               # effect grows with x: ATE = 1
Y <- m0(X) + tauf(X) * D + rnorm(n, 0, 0.3)
dat <- data.frame(X, D, Y)

## ---- truths in this sample ----------------------------------------------------
ATE <- mean(tauf(X)); ATT <- mean(tauf(X[D == 1])); ATU <- mean(tauf(X[D == 0]))

## ---- fuzzy matching: bins of width h are the cells ---------------------------
h <- 0.05
bin <- cut(X, seq(0, 1, h), include.lowest = TRUE)
tab <- aggregate(cbind(n1 = D, n0 = 1 - D, y1 = Y * D, y0 = Y * (1 - D)) ~ bin, data.frame(bin, D, Y), sum)
tab$n <- tab$n1 + tab$n0
## a bin counts as matched only with at least 5 units in each arm (one stray control is not overlap)
tab$tau <- ifelse(tab$n1 >= 5 & tab$n0 >= 5, tab$y1 / tab$n1 - tab$y0 / tab$n0, NA)
ok <- !is.na(tab$tau)
mid <- seq(h / 2, 1 - h / 2, h)
overlap <- range(mid[ok])
match_ate <- sum(tab$tau[ok] * tab$n[ok]) / sum(tab$n[ok])      # ATE on the overlap population
match_att <- sum(tab$tau[ok] * tab$n1[ok]) / sum(tab$n1[ok])    # ATT on matchable treated
ATE_overlap <- mean(tauf(X[X >= overlap[1] - h/2 & X <= overlap[2] + h/2]))
cat(sprintf("bins with both arms: %d of %d, x in [%.2f, %.2f]; units in them: %d of %d (%.0f%%); treated outside: %d of %d\n",
            sum(ok), nrow(tab), overlap[1] - h/2, overlap[2] + h/2, sum(tab$n[ok]), n,
            100 * sum(tab$n[ok]) / n, sum(tab$n1[!ok]), sum(D)))

## ---- regression: the coefficient, and g-computation --------------------------
ols_lin  <- coef(lm(Y ~ D + X, dat))["D"]              # tau_R with a linear control for x
ols_quad <- coef(lm(Y ~ D + X + I(X^2), dat))["D"]     # tau_R with the right curvature, still one coefficient
gcomp <- function(form) {                              # separate model per arm, impute both, average
  f1 <- lm(form, dat[dat$D == 1, ]); f0 <- lm(form, dat[dat$D == 0, ])
  d <- predict(f1, dat) - predict(f0, dat)
  c(ATE = mean(d), ATT = mean(d[dat$D == 1]))
}
g_lin  <- gcomp(Y ~ X)
g_quad <- gcomp(Y ~ X + I(X^2))
## what the linear control model says at x = 0.95, where there are no controls
f0_lin <- lm(Y ~ X, dat[dat$D == 0, ]); f0_quad <- lm(Y ~ X + I(X^2), dat[dat$D == 0, ])
cat(sprintf("controls with x > 0.9: %d.  At x = 0.95: true m0 = %.2f; linear fit extrapolates %.2f; quadratic fit %.2f\n",
            sum(D == 0 & X > 0.9), m0(0.95), predict(f0_lin, data.frame(X = 0.95)), predict(f0_quad, data.frame(X = 0.95))))
## regression weights
xs <- seq(0.005, 0.995, 0.01); ex <- pnorm((xs - 0.6) / 0.12); w_reg <- ex * (1 - ex) / sum(ex * (1 - ex))
cat(sprintf("share of regression weight on x in [0.45, 0.75]: %.2f (ATE weight on it: 0.30)\n", sum(w_reg[xs >= 0.45 & xs <= 0.75])))

res <- rbind(
  c("truth, whole sample",          ATE, ATT),
  c("truth, overlap population",     ATE_overlap, NA),
  c("binned matching, overlap only", match_ate, match_att),
  c("OLS coefficient, linear in x",  ols_lin, NA),
  c("OLS coefficient, x and x^2",    ols_quad, NA),
  c("g-computation, linear per arm", g_lin["ATE"], g_lin["ATT"]),
  c("g-computation, quadratic per arm", g_quad["ATE"], g_quad["ATT"]))
colnames(res) <- c("estimator", "ATE", "ATT"); print(res, quote = FALSE)

## ---- figure ---------------------------------------------------------------
pdf("figs/fuzzy.pdf", width = 8.4, height = 2.6)
par(mfrow = c(1, 3), mar = c(3.2, 3.2, 1.6, 0.6), mgp = c(1.9, 0.6, 0), cex = 0.8)
plot(X, Y, col = ifelse(D == 1, adjustcolor("#C84E00", 0.25), adjustcolor("#012169", 0.25)),
     pch = 16, cex = 0.4, xlab = "x", ylab = "y", main = "(a) data and a linear control fit")
curve(m0(x), add = TRUE, col = "#012169", lwd = 2)
curve(m0(x) + tauf(x), add = TRUE, col = "#C84E00", lwd = 2)
abline(f0_lin, col = "#012169", lwd = 2, lty = 2)
legend("topleft", bty = "n", cex = 0.8, lwd = 2, lty = c(1, 2), col = c("gray30", "#012169"),
       legend = c("true curves", "control line, linear, extended"))
plot(mid, tab$tau, pch = 19, col = "#012169", ylim = c(-0.5, 2.5), xlim = c(0, 1),
     xlab = "x (bins of width 0.05)", ylab = expression(hat(tau)(x)), main = "(b) bin-by-bin differences")
curve(tauf(x), add = TRUE, lty = 3, col = "gray40")
rect(overlap[2] + h/2, -0.5, 1, 2.5, col = adjustcolor("gray", 0.3), border = NA)
rect(0, -0.5, overlap[1] - h/2, 2.5, col = adjustcolor("gray", 0.3), border = NA)
text(0.95, 2.2, "no\ncontrols", cex = 0.8); text(0.12, 2.2, "no treated", cex = 0.8)
plot(xs, w_reg / max(w_reg), type = "l", lwd = 2, col = "#012169", ylim = c(0, 1.05),
     xlab = "x", ylab = "weight (scaled)", main = "(c) weights by estimand")
abline(h = 1, lwd = 2, col = "#C84E00")
lines(xs, ex / max(ex), lwd = 2, col = "#C84E00", lty = 2)
legend("topleft", bty = "n", cex = 0.75, lwd = 2, lty = c(1, 2, 1), col = c("#C84E00", "#C84E00", "#012169"),
       legend = c("ATE: f(x)", "ATT: e(x) f(x)", "regression: e(x)(1-e(x)) f(x)"))
dev.off()

## ---- 7. what OLS does with a continuous x: the two implicit models ---------
## OLS of Y on D and x residualizes D on the LINEAR projection of D on x,
## i.e. an implicit linear probability model for e(x). Check the decomposition
##   tau_OLS = { E[e(x)(1 - e_lin(x)) tau(x)] + E[(e(x) - e_lin(x)) m0(x)] } / E[(D - e_lin(x))^2]
## Sample version, exact: with D-check = D - e_lin(x) and Y = m0(x) + tau(x) D + eps,
##   tau_OLS = mean(D-check * Y) / mean(D-check^2)
##           = [ mean(D-check * D * tau(x)) + mean(D-check * m0(x)) + mean(D-check * eps) ] / mean(D-check^2)
eps <- Y - m0(X) - tauf(X) * D
decomp <- function(e_hat, label) {
  dch <- D - e_hat; den <- mean(dch^2)
  parts <- c(weighted_tau = mean(dch * D * tauf(X)) / den,
             form_bias    = mean(dch * m0(X)) / den,
             noise        = mean(dch * eps) / den)
  cat(sprintf("%s: coefficient %.3f = weighted tau %.3f + functional-form bias %.3f + noise %.3f\n",
              label, sum(parts), parts[1], parts[2], parts[3]))
  invisible(parts)
}
e_lin <- fitted(lm(D ~ X, dat))                       # implicit propensity: linear in x
decomp(e_lin, "OLS linear in x")
cat(sprintf("implicit linear propensity ranges [%.2f, %.2f]; share of weight D(1 - e_lin) on x in [0.45, 0.75]: %.2f\n",
            min(e_lin), max(e_lin), sum((D * (1 - e_lin))[X >= .45 & X <= .75]) / sum(D * (1 - e_lin))))
e_q <- fitted(lm(D ~ X + I(X^2), dat))                # implicit propensity: quadratic
decomp(e_q, "OLS with x and x^2")                     # m0 = 3x^2 is in the span, so the bias term vanishes

## single-panel figure for the g-computation frame: data, truth, and the linear control fit
pdf("figs/fuzzy_gcomp.pdf", width = 4.6, height = 2.7)
par(mar = c(3.2, 3.2, 0.6, 0.6), mgp = c(1.9, 0.6, 0), cex = 0.8)
plot(X, Y, col = ifelse(D == 1, adjustcolor("#C84E00", 0.25), adjustcolor("#012169", 0.25)),
     pch = 16, cex = 0.4, xlab = "x", ylab = "y")
curve(m0(x), add = TRUE, col = "#012169", lwd = 2)
curve(m0(x) + tauf(x), add = TRUE, col = "#C84E00", lwd = 2)
abline(f0_lin, col = "#012169", lwd = 2, lty = 2)
abline(v = 0.9, lty = 3, col = "gray50")
legend("topleft", bty = "n", cex = 0.8, lwd = 2, lty = c(1, 1, 2), col = c("#C84E00", "#012169", "#012169"),
       legend = c("treated: true mean", "controls: true mean", "controls: linear fit, extended"))
dev.off()
