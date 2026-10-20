## Week 9 -- numbers and figures for the step-by-step estimation frames (Senate data).
##   Rscript figs/w9_estimation.R > figs/w9_estimation.out
##   writes figs/senate_windows.pdf, figs/senate_means_lines.pdf, figs/senate_three_h.pdf
library(rdrobust)
data(rdrobust_RDsenate)
s <- subset(rdrobust_RDsenate, !is.na(vote))
x <- s$margin; y <- s$vote
h <- rdrobust(y, x)$bws[1, 1]

## ---- 1. naive comparisons in shrinking windows -----------------------------------
cat("## 1. winners minus losers, mean vote at t+6, in windows |margin| < w\n")
win <- t(sapply(c(Inf, 50, 20, 10, 5, 2), function(w) {
  k <- abs(x) < w
  c(w = w, n_lost = sum(k & x < 0), n_won = sum(k & x >= 0),
    mean_lost = mean(y[k & x < 0]), mean_won = mean(y[k & x >= 0]),
    diff = mean(y[k & x >= 0]) - mean(y[k & x < 0])) }))
print(round(win, 2))
cat("races with |margin| < 0.5:", sum(abs(x) < 0.5), "; exactly 0:", sum(x == 0), "\n")
cat("lagged vote (t-6) winners minus losers, all races:",
    round(mean(s$demvoteshlag1[x >= 0], na.rm = TRUE) - mean(s$demvoteshlag1[x < 0], na.rm = TRUE), 2),
    "; |margin| < 2:", round(mean(s$demvoteshlag1[x >= 0 & x < 2], na.rm = TRUE) -
                              mean(s$demvoteshlag1[x < 0 & x > -2], na.rm = TRUE), 2), "\n")

## ---- 2. means vs lines at the MSE-optimal h ---------------------------------------
d <- subset(s, abs(margin) <= h); d$D <- as.integer(d$margin >= 0)
mb <- mean(d$vote[d$D == 0]); ma <- mean(d$vote[d$D == 1])
xb <- mean(d$margin[d$D == 0]); xa <- mean(d$margin[d$D == 1])
fu <- lm(vote ~ D * margin, data = d)                     # uniform kernel
d$w <- 1 - abs(d$margin) / h
ft <- lm(vote ~ D * margin, data = d, weights = d$w)       # triangular kernel
cat("\n## 2. h =", round(h, 2), "\n")
cat("means: below", round(mb, 2), "at mean margin", round(xb, 2), "; above", round(ma, 2), "at", round(xa, 2),
    "; difference", round(ma - mb, 2), "\n")
cat("uniform lines: at 0 below", round(coef(fu)[1], 2), " above", round(coef(fu)[1] + coef(fu)[2], 2),
    " gap", round(coef(fu)[2], 2), "\n")
cat("triangular lines: at 0 below", round(coef(ft)[1], 2), " above", round(coef(ft)[1] + coef(ft)[2], 2),
    " gap", round(coef(ft)[2], 2), "\n")
cat("triangular weight of a race at margin 2, 9, 16:", round(1 - c(2, 9, 16) / h, 2), "\n")
cat("share of total triangular weight from |margin| < h/2:", round(sum(d$w[abs(d$margin) < h / 2]) / sum(d$w), 3),
    "; share of races:", round(mean(abs(d$margin) < h / 2), 3), "\n")

## ---- 3. local linear vs local quadratic, same h ------------------------------------
cat("\n## 3. degree of the local fit at h =", round(h, 2), "(triangular)\n")
for (p in 0:2) { q <- rdrobust(y, x, h = h, p = p); cat("p =", p, ": estimate", round(q$coef[1], 2), " se", round(q$se[1], 2), "\n") }

## ---- 4. local linear by bandwidth, conventional estimate + conventional se -------
cat("\n## 4. local linear at several h (triangular, conventional)\n")
tb <- t(sapply(c(5, 10, h, 30, 50), function(hh) {
  q <- rdrobust(y, x, h = hh); c(h = hh, est = q$coef[1], se = q$se[1], n = sum(q$N_h)) }))
print(round(tb, 2))

## ---- 5. the standard error from Var(tau-hat) = 4 s+^2/m+ + 4 s-^2/m- (uniform weights) --------
cat("\n## 5. standard error at h =", round(h, 2), "\n")
sp <- summary(lm(vote ~ margin, data = d, subset = D == 1))$sigma
sm <- summary(lm(vote ~ margin, data = d, subset = D == 0))$sigma
mp <- sum(d$D == 1); mm <- sum(d$D == 0)
cat("sigma+ =", round(sp, 2), " sigma- =", round(sm, 2), " m+ =", mp, " m- =", mm, "\n")
cat("formula se = sqrt(4 s+^2/m+ + 4 s-^2/m-) =", round(sqrt(4 * sp^2 / mp + 4 * sm^2 / mm), 2), "\n")
cat("lm (uniform, separate variances) se of D:",
    round(sqrt(summary(lm(vote ~ margin, data = d, subset = D == 1))$coef[1, 2]^2 +
               summary(lm(vote ~ margin, data = d, subset = D == 0))$coef[1, 2]^2), 2), "\n")
q <- rdrobust(y, x)
cat("rdrobust conventional: estimate", round(q$coef[1], 2), " se", round(q$se[1], 2),
    " CI", round(q$coef[1] - 1.96 * q$se[1], 2), round(q$coef[1] + 1.96 * q$se[1], 2), "\n")
cat("edge factor, uniform u on [0,1]: line", solve(outer(1:2, 1:2, function(i, j) 1 / (i + j - 1)))[1, 1],
    " quadratic", solve(outer(1:3, 1:3, function(i, j) 1 / (i + j - 1)))[1, 1], "\n")
cat("robust se with b = h, formula sqrt(9 s+^2/m+ + 9 s-^2/m-) =", round(sqrt(9 * sp^2 / mp + 9 * sm^2 / mm), 2), "\n")
qb <- rdrobust(y, x, h = h, b = h); qd <- rdrobust(y, x)
cat("rdrobust robust se: b = h", round(qb$se[3], 2), "; default b =", round(qd$bws[2, 1], 2), ":", round(qd$se[3], 2), "\n")
q10 <- rdrobust(y, x, h = 10)
cat("undersmoothed, h = 10, conventional: estimate", round(q10$coef[1], 2), " se", round(q10$se[1], 2),
    " CI", round(q10$coef[1] - 1.96 * q10$se[1], 2), round(q10$coef[1] + 1.96 * q10$se[1], 2), "\n")

## ---- 6. power of the robust test of tau = 0 (rdpower / rdsampsi) --------------------
if (!requireNamespace("rdpower", quietly = TRUE)) install.packages("rdpower", repos = "https://cloud.r-project.org", quiet = TRUE)
suppressMessages(library(rdpower))
cat("\n## 6. power, robust test of tau = 0 at h_MSE\n")
for (t in c(3, 5, 7)) {
  pw <- rdpower(data = cbind(y, x), tau = t)
  cat("tau =", t, ": robust se", round(pw$se.rbc, 2), " units in h", pw$Nh.l + pw$Nh.r,
      " power", round(pw$power.rbc, 2), " by hand", round(pnorm(t / pw$se.rbc - 1.96) + pnorm(-t / pw$se.rbc - 1.96), 2), "\n")
}
ss <- rdsampsi(data = cbind(y, x), tau = 3)
cat("80% power for tau = 3: units in h", ss$sampsi.h.tot, " total n", ss$sampsi.tot, "\n")

## ---- figures -------------------------------------------------------------------------
br <- seq(-100, 100, by = 2.5); mids <- br[-1] - 1.25
bmn <- tapply(y, cut(x, br, right = FALSE), mean)
col_side <- function(m) ifelse(m < 0, "navy", "firebrick")

## (a) means vs lines, zoomed to |margin| < 30
pdf("figs/senate_means_lines.pdf", width = 9, height = 3.3, family = "Times")
par(mfrow = c(1, 2), mar = c(4, 4.2, 1.8, 0.6))
z <- abs(mids) < 30
for (panel in 1:2) {
  plot(mids[z], bmn[z], pch = 19, cex = 0.7, col = col_side(mids[z]), ylim = c(30, 70),
       xlab = "Democratic margin at t", ylab = "vote share at t + 6",
       main = if (panel == 1) "compare means within h" else "fit a line within h", font.main = 1, cex.main = 1)
  abline(v = 0, lty = 2, col = "gray50"); rect(-h, 25, h, 75, border = NA, col = adjustcolor("gray", 0.15))
  if (panel == 1) {
    segments(-h, mb, 0, mb, lwd = 3, col = "navy"); segments(0, ma, h, ma, lwd = 3, col = "firebrick")
    points(c(xb, xa), c(mb, ma), pch = 4, cex = 1.6, lwd = 2)
  } else {
    gl <- seq(-h, 0, length.out = 30); gr <- seq(0, h, length.out = 30)
    lines(gl, coef(ft)[1] + coef(ft)[3] * gl, lwd = 3, col = "navy")
    lines(gr, coef(ft)[1] + coef(ft)[2] + (coef(ft)[3] + coef(ft)[4]) * gr, lwd = 3, col = "firebrick")
    points(c(0, 0), c(coef(ft)[1], coef(ft)[1] + coef(ft)[2]), pch = 19, cex = 1.3)
  }
}
dev.off()

## (b) three bandwidths
pdf("figs/senate_three_h.pdf", width = 7.5, height = 2.6, family = "Times")
par(mfrow = c(1, 3), mar = c(3.6, 3.6, 1.8, 0.4), mgp = c(2.2, 0.7, 0), cex = 0.9)
for (hh in c(5, h, 50)) {
  dd <- subset(s, abs(margin) <= hh); dd$D <- as.integer(dd$margin >= 0); dd$w <- 1 - abs(dd$margin) / hh
  f <- lm(vote ~ D * margin, data = dd, weights = w)
  z <- abs(mids) < 60
  plot(mids[z], bmn[z], pch = 19, cex = 0.6, col = adjustcolor(col_side(mids[z]), 0.5), ylim = c(20, 80),
       xlab = "Democratic margin at t", ylab = "vote share at t + 6",
       main = sprintf("h = %.1f: estimate %.1f", hh, coef(f)[["D"]]), font.main = 1, cex.main = 1.25)
  abline(v = 0, lty = 2, col = "gray50"); rect(-hh, 15, hh, 85, border = NA, col = adjustcolor("gray", 0.15))
  gl <- seq(-hh, 0, length.out = 30); gr <- seq(0, hh, length.out = 30)
  lines(gl, coef(f)[1] + coef(f)[3] * gl, lwd = 3, col = "navy")
  lines(gr, coef(f)[1] + coef(f)[2] + (coef(f)[3] + coef(f)[4]) * gr, lwd = 3, col = "firebrick")
}
dev.off()
