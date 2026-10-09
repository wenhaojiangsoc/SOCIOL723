## Week 9 -- simulations behind three claims on the slides.
##   1. at the MSE-optimal bandwidth the conventional interval undercovers; the robust one does not
##   2. RD needs about 4 times (uniform X) or 2.75 times (normal X, common slope) the sample of an experiment
##   3. what sorting does to the density and to the estimate (figs/manip_density.pdf)
##   Rscript figs/w9_sims.R > figs/w9_sims.out
set.seed(723)
library(rdrobust)

## ---- 1. coverage, the Lee (2008) design used by Calonico, Cattaneo, and Titiunik (2014) ----
m_lee <- function(x) ifelse(x < 0,
  0.48 + 1.27 * x + 7.18 * x^2 + 20.21 * x^3 + 21.54 * x^4 + 7.33 * x^5,
  0.52 + 0.84 * x - 3.00 * x^2 + 7.99 * x^3 - 9.01 * x^4 + 3.56 * x^5)
tau <- 0.04; R <- 1000
for (n in c(500, 5000)) {
res <- t(replicate(R, {
  x <- 2 * rbeta(n, 2, 4) - 1; y <- m_lee(x) + rnorm(n, 0, 0.1295)
  r <- rdrobust(y, x)
  c(est = r$coef[1], se = r$se[1], h = r$bws[1, 1],
    conv = as.numeric(r$ci[1, 1] <= tau & tau <= r$ci[1, 2]),
    rob = as.numeric(r$ci[3, 1] <= tau & tau <= r$ci[3, 2]),
    len_conv = unname(diff(r$ci[1, ])), len_rob = unname(diff(r$ci[3, ])),
    rob_cer = { rc <- rdrobust(y, x, bwselect = "cerrd"); as.numeric(rc$ci[3, 1] <= tau & tau <= rc$ci[3, 2]) })
}))
cat("## 1. Lee design, n =", n, ", R =", R, "\n")
cat("bias of conventional estimate:", round(mean(res[, "est"]) - tau, 4),
    "; sd:", round(sd(res[, "est"]), 4), "; bias/sd:", round((mean(res[, "est"]) - tau) / sd(res[, "est"]), 2), "\n")
cat("coverage of nominal 95%: conventional", round(mean(res[, "conv"]), 3), "; robust", round(mean(res[, "rob"]), 3), "\n")
cat("robust CI at the CER-optimal h:", round(mean(res[, "rob_cer"]), 3), "\n")
cat("median length: conventional", round(median(res[, "len_conv"]), 4), "; robust", round(median(res[, "len_rob"]), 4), "\n")
cat("median h:", round(median(res[, "h"]), 3), "\n")
}
## coverage of est +- 1.96 se when the bias is half a standard error
cat("theory: Pr(|N(0.5,1)| <= 1.96) =", round(pnorm(1.96 - 0.5) - pnorm(-1.96 - 0.5), 4), "\n")

## ---- 2. the sample-size price of RD ------------------------------------------------
cat("\n## 2. variance of the RD estimate / variance of a difference in means (same n, same sigma)\n")
n <- 1000; R <- 20000
ratio <- function(draw_x, interact) {
  v <- t(replicate(R, {
    x <- draw_x(n); e <- rnorm(n)
    D <- as.integer(x >= 0); y <- x + e
    M <- if (interact) cbind(1, D, x, D * x) else cbind(1, D, x)
    rd <- .lm.fit(M, y)$coefficients[2]
    Dr <- rbinom(n, 1, 0.5); yr <- x + e       # experiment: same outcome model, random D
    c(rd = rd, rct = mean(yr[Dr == 1]) - mean(yr[Dr == 0]) - (mean(x[Dr == 1]) - mean(x[Dr == 0])))
  }))
  var(v[, "rd"]) / var(v[, "rct"])
}
cat("uniform X, separate slopes:", round(ratio(function(n) runif(n, -1, 1), TRUE), 2), " (theory 4)\n")
cat("uniform X, common slope:   ", round(ratio(function(n) runif(n, -1, 1), FALSE), 2), " (theory 1/(1 - 3/4) = 4)\n")
cat("normal X, common slope:    ", round(ratio(rnorm, FALSE), 2), " (theory 1/(1 - 2/pi) =", round(1 / (1 - 2 / pi), 2), ")\n")
cat("normal X, separate slopes: ", round(ratio(rnorm, TRUE), 2), "\n")

## ---- 3. sorting: 30% of units landing in [-2, 0) push themselves to [0, 2) ----------
cat("\n## 3. sorting\n")
n <- 5000
x <- rnorm(n, 0, 15); u <- rnorm(n)
mover <- x >= -2 & x < 0 & u > quantile(u, 0.7)   # the movers are the high-u units
x2 <- ifelse(mover, -x, x)
y2 <- 45 + 0.2 * x2 + 3 * u + rnorm(n)            # true effect of crossing = 0
cat("movers:", sum(mover), "\n")
cat("estimate, no sorting:", round(rdrobust(45 + 0.2 * x + 3 * u + rnorm(n), x)$coef[1], 2),
    "; with sorting:", round(rdrobust(y2, x2)$coef[1], 2), "\n")
suppressMessages(library(rddensity))
d1 <- rddensity(x); d2 <- rddensity(x2)
cat("density test p: no sorting", round(d1$test$p_jk, 3), "; sorting", signif(d2$test$p_jk, 3), "\n")
pdf("figs/manip_density.pdf", width = 9, height = 3, family = "Times")
par(mfrow = c(1, 2), mar = c(4, 4, 1.6, 1))
br <- seq(-60, 60, by = 2)
hist(x[abs(x) < 60], breaks = br, col = ifelse(br[-1] <= 0, "navy", "firebrick"), border = "white",
     main = "A. no sorting", xlab = "running variable", ylab = "count", cex.main = 1)
abline(v = 0, lty = 2)
hist(x2[abs(x2) < 60], breaks = br, col = ifelse(br[-1] <= 0, "navy", "firebrick"), border = "white",
     main = "B. high-u units just below move just above", xlab = "running variable", ylab = "count", cex.main = 1)
abline(v = 0, lty = 2)
dev.off()
