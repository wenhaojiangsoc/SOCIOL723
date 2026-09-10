## Week 3 slides, Testing section: simulations and figures.
## Run from the week folder:  Rscript figs/make_testing_figs.R
set.seed(723)
dukeblue <- rgb(1, 33, 105, maxColorValue = 255)
accent   <- rgb(200, 78, 0, maxColorValue = 255)

## ---------- 1. Poisson, n = 200, theta0 = 2: three rulers ----------------
n <- 200; th0 <- 2; R <- 20000
ybar <- replicate(R, mean(rpois(n, th0)))
W  <- n * (ybar - th0)^2 / ybar                     # observed info at ybar: n/ybar
LM <- n * (ybar - th0)^2 / th0                      # expected info at theta0: n/theta0
LR <- 2 * n * (ybar * log(ybar / th0) - (ybar - th0))
cat("Poisson n=200: reject rates  W", mean(W > 3.84), " LR", mean(LR > 3.84),
    " LM", mean(LM > 3.84), "\n")
cat("max |LR - W| =", max(abs(LR - W)), "; max |LM - W| =", max(abs(LM - W)), "\n")
cat("share of samples where the three verdicts differ:",
    mean((W > 3.84) != (LR > 3.84) | (W > 3.84) != (LM > 3.84)), "\n")

pdf("figs/rulers_large.pdf", width = 6.4, height = 3.2)
par(mfrow = c(1, 2), mar = c(3.6, 3.6, 1.6, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.85)
idx <- sample(R, 2000)
plot(W[idx], LR[idx], pch = 16, cex = 0.35, col = adjustcolor(dukeblue, 0.5),
     xlab = expression(W == z^2), ylab = "LR", main = "LR against Wald", font.main = 1,
     xlim = c(0, 12), ylim = c(0, 12))
abline(0, 1, col = "gray50"); abline(h = 3.84, v = 3.84, lty = 3, col = accent)
plot(W[idx], LM[idx], pch = 16, cex = 0.35, col = adjustcolor(dukeblue, 0.5),
     xlab = expression(W == z^2), ylab = "LM", main = "Score against Wald", font.main = 1,
     xlim = c(0, 12), ylim = c(0, 12))
abline(0, 1, col = "gray50"); abline(h = 3.84, v = 3.84, lty = 3, col = accent)
dev.off()

## worked numbers for one sample, ybar = 2.3
yb <- 2.3
cat(sprintf("ybar=2.3: W=%.2f LR=%.2f LM=%.2f\n", n*(yb-th0)^2/yb,
            2*n*(yb*log(yb/th0)-(yb-th0)), n*(yb-th0)^2/th0))

## ---------- 2. Bernoulli, theta0 = 0.1: exact enumeration ----------------
th0 <- 0.1
rulers <- function(n, s) {
  th <- s / n
  se <- sqrt(th * (1 - th) / n)
  z  <- (th - th0) / se
  W  <- z^2
  l  <- function(t) ifelse(s == 0, 0, s * log(t)) + ifelse(s == n, 0, (n - s) * log(1 - t))
  LR <- 2 * (l(th) - l(th0))
  S  <- s / th0 - (n - s) / (1 - th0)
  LM <- S^2 / (n / (th0 * (1 - th0)))
  data.frame(s = s, theta_hat = th, se = se, z = z, W = W, LR = LR, LM = LM)
}
cat("\nBernoulli n=20, s = 0..4:\n")
print(round(rulers(20, 0:4), 3))
cat("\nExact rejection rates at 3.84, theta0 = 0.1:\n")
for (n in c(20, 50, 100, 500)) {
  s <- 0:n; p <- dbinom(s, n, th0); r <- rulers(n, s)
  undef <- is.na(r$W) | !is.finite(r$W)
  cat(sprintf("n=%4d  W undefined %5.1f%%  W rejects %5.1f%%  LR rejects %5.1f%%  LM rejects %5.1f%%\n",
      n, 100*sum(p[undef]), 100*sum(p[!undef & r$W > 3.84]),
      100*sum(p[r$LR > 3.84]), 100*sum(p[r$LM > 3.84])))
}

## ---------- 3. check the parabola_n table: drop at 2 SE below the peak ----
th <- 0.1
for (n in c(20, 100, 500, 2000)) {
  se <- sqrt(th * (1 - th) / n); t2 <- th - 2 * se
  drop <- if (t2 <= 0) NA else n * (th * log(th / t2) + (1 - th) * log((1 - th) / (1 - t2)))
  cat(sprintf("n=%4d  theta at -2SE = %.3f  drop = %.2f\n", n, t2, drop))
}
