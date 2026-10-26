## Week 9 -- fuzzy RD: local 2SLS by hand and the bandwidth for the ratio.
##   Rscript figs/w9_fuzzy.R > figs/w9_fuzzy.out
## Same simulated data as Lab 9's fuzzy section (true effect 1.5, take-up jump 0.55).
library(rdrobust); library(ivreg)
set.seed(1)
n <- 3000
xx <- runif(n, -1, 1)
takeup <- rbinom(n, 1, ifelse(xx >= 0, 0.75, 0.20))
yy <- 1.5 * takeup + 0.5 * xx + rnorm(n)

## ---- 1. bandwidths ------------------------------------------------------------------
fz <- rdrobust(yy, xx, fuzzy = takeup)
hY <- rdrobust(yy, xx)$bws[1, 1]; hD <- rdrobust(takeup, xx)$bws[1, 1]; h <- fz$bws[1, 1]
cat("## 1. MSE-optimal h: outcome alone", round(hY, 3), "; take-up alone", round(hD, 3),
    "; fuzzy (ratio)", round(h, 3), "\n")

## ---- 2. local 2SLS at the fuzzy h, triangular kernel ------------------------------------
d <- data.frame(y = yy, x = xx, D = takeup)
d <- subset(d, abs(x) <= h); d$Z <- as.integer(d$x >= 0); d$w <- 1 - abs(d$x) / h
fs <- lm(D ~ Z + x + Z:x, data = d, weights = w)          # first stage
rf <- lm(y ~ Z + x + Z:x, data = d, weights = w)          # reduced form
d$Dhat <- fitted(fs)
ss <- lm(y ~ Dhat + x + Z:x, data = d, weights = w)        # second stage
iv <- ivreg(y ~ D + x + Z:x | Z + x + Z:x, data = d, weights = w)
cat("\n## 2. h =", round(h, 3), ", n in window", nrow(d), "\n")
cat("first stage jump", round(coef(fs)["Z"], 3), "; reduced form jump", round(coef(rf)["Z"], 3),
    "; ratio", round(coef(rf)["Z"] / coef(fs)["Z"], 3), "\n")
cat("second stage on Dhat", round(coef(ss)["Dhat"], 3), "; ivreg", round(coef(iv)["D"], 3),
    "; rdrobust conventional", round(fz$coef[1], 3), "\n")
cat("se: naive second stage", round(summary(ss)$coef["Dhat", 2], 3), "; ivreg", round(summary(iv)$coef["D", 2], 3),
    "; rdrobust conventional", round(fz$se[1], 3), "\n")

## ---- 3. Ser Pilo Paga (Lab 9): the two jumps at the fuzzy bandwidth, one figure each ------
##   writes figs/spp_jump_D.pdf and figs/spp_jump_Y.pdf
spp <- read.csv("../Data/spp_colombia.csv")
fzs <- suppressWarnings(rdrobust(spp$Y, spp$X1, fuzzy = spp$D)); hs <- fzs$bws[1, 1]
w <- subset(spp, abs(X1) <= hs); w$Z <- as.integer(w$X1 >= 0); w$k <- 1 - abs(w$X1) / hs
jD <- lm(D ~ Z * X1, data = w, weights = k); jY <- lm(Y ~ Z * X1, data = w, weights = k)
cat("\n## 3. SPP at the fuzzy h =", round(hs, 2), ", n in window", nrow(w), "\n")
cat("jump in D", round(coef(jD)[["Z"]], 3), "; jump in Y", round(coef(jY)[["Z"]], 3),
    "; ratio", round(coef(jY)[["Z"]] / coef(jD)[["Z"]], 3), "; rdrobust", round(fzs$coef[1], 3), "\n")
ivs <- ivreg(Y ~ D + X1 + Z:X1 | Z + X1 + Z:X1, data = w, weights = k)
cat("local 2SLS (ivreg, same h and kernel)", round(coef(ivs)[["D"]], 3), "\n")
br <- seq(-30, 30, by = 1.5); mids <- br[-1] - 0.75
jumpfig <- function(file, v, fit, ylab) {
  pdf(file, width = 7, height = 3, family = "Times"); par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.25, cex.axis = 1.15)
  bm <- tapply(spp[[v]], cut(spp$X1, br, right = FALSE), mean)
  plot(mids, bm, pch = 19, col = ifelse(mids < 0, "navy", "firebrick"), cex = 0.8, ylim = c(0, 1),
       xlab = "SISBEN distance to cutoff (positive = eligible)", ylab = ylab)
  abline(v = 0, lty = 2, col = "gray50"); abline(v = c(-hs, hs), lty = 3, col = "gray70")
  b <- coef(fit); gl <- seq(-hs, 0, length.out = 50); gr <- seq(0, hs, length.out = 50)
  lines(gl, b[1] + b[3] * gl, lwd = 3, col = "navy")
  lines(gr, b[1] + b[2] + (b[3] + b[4]) * gr, lwd = 3, col = "firebrick")
  segments(0, b[1], 0, b[1] + b[2], lwd = 3, col = "darkorange")
  text(0.6, b[1] + b[2] / 2, sprintf("jump = %.2f", b[2]), pos = 4, col = "darkorange", cex = 1.3)
  dev.off()
}
jumpfig("figs/spp_jump_D.pdf", "D", jD, "share receiving SPP")
jumpfig("figs/spp_jump_Y.pdf", "Y", jY, "share enrolled in college")
