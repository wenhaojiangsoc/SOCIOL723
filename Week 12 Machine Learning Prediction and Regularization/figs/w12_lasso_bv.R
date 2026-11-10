## Week 12 -- bias, variance, and test error of the lasso as the penalty lambda moves.
## The 690S sparse design (as in figs/w12_sparsity.R): n = 100, p = 400, X ~ U(-.5, .5), beta_j = 7 / j^2, sigma = 1.
##   Rscript figs/w12_lasso_bv.R > figs/w12_lasso_bv.out   (run from the week folder)
## writes figs/lasso_bv_curve.pdf
suppressMessages(library(glmnet))
set.seed(723)
n <- 100; p <- 400; sig <- 1; beta <- 7 / (1:p)^2
x0 <- matrix(runif(200 * p, -0.5, 0.5), 200, p); f0 <- as.vector(x0 %*% beta)   # 200 fixed new people
lam <- exp(seq(log(1), log(0.003), length.out = 30)); R <- 300
pred <- array(NA, c(R, length(lam), nrow(x0))); tr <- matrix(NA, R, length(lam)); nz <- matrix(NA, R, length(lam))
for (r in 1:R) {
  X <- matrix(runif(n * p, -0.5, 0.5), n, p); y <- as.vector(X %*% beta + rnorm(n, 0, sig))
  m <- glmnet(X, y, alpha = 1, lambda = lam)
  pred[r, , ] <- t(predict(m, x0))
  tr[r, ] <- colMeans((y - predict(m, X))^2)
  nz[r, ] <- m$df
}
bias2 <- sapply(seq_along(lam), function(j) mean((colMeans(pred[, j, ]) - f0)^2))
vari  <- sapply(seq_along(lam), function(j) mean(apply(pred[, j, ], 2, var)))
test  <- sig^2 + bias2 + vari
out <- data.frame(lambda = lam, terms = colMeans(nz), bias2, variance = vari, test, train = colMeans(tr))
cat("lasso, n = 100, p = 400,", R, "samples; errors averaged over 200 new people; sigma^2 = 1\n")
print(round(out, 3))
jb <- which.min(test)
cat("best lambda", signif(lam[jb], 2), ": test", round(test[jb], 3), "= 1 +", round(bias2[jb], 3), "(bias2) +",
    round(vari[jb], 3), "(variance); terms kept", round(mean(nz[, jb]), 1), "\n")
cat("largest lambda", signif(lam[1], 2), ": bias2", round(bias2[1], 3), "variance", round(vari[1], 3), "terms", mean(nz[, 1]), "\n")
cat("smallest lambda", signif(lam[length(lam)], 2), ": bias2", round(bias2[length(lam)], 3), "variance", round(vari[length(lam)], 3),
    "terms", round(mean(nz[, length(lam)]), 1), "train", round(mean(tr[, length(lam)]), 3), "\n")

pdf("figs/lasso_bv_curve.pdf", width = 5.4, height = 3.1, family = "Times")
par(mar = c(3.3, 3.6, 0.6, 0.6), mgp = c(2.1, 0.6, 0))
xx <- -log(lam)                                     # left = big penalty (rigid), right = small penalty (flexible)
yl <- c(0, max(test) * 1.05)
plot(xx, test, type = "l", lwd = 2.5, ylim = yl, las = 1, xaxt = "n",
     xlab = expression("penalty " * lambda * " (smaller = more flexible)"), ylab = "average squared error")
at <- c(1, 0.3, 0.1, 0.03, 0.01, 0.003); axis(1, at = -log(at), labels = at)
lines(xx, bias2, col = "navy", lwd = 2); lines(xx, vari, col = "firebrick", lwd = 2)
lines(xx, out$train, col = "darkgreen", lwd = 2, lty = 2); abline(h = sig^2, col = "gray55", lty = 3)
points(xx[jb], test[jb], pch = 19, cex = 1.1)
text(xx[13], test[13] + 0.13, "test error", cex = 0.85)
text(xx[5] + 0.15, bias2[5] + 0.05, expression(bias^2), col = "navy", cex = 0.85, adj = 0)
text(xx[25], vari[25] + 0.12, "variance", col = "firebrick", cex = 0.85)
text(xx[10] + 0.1, out$train[10] + 0.05, "training error", col = "darkgreen", cex = 0.85, adj = 0)
text(xx[30], sig^2 + 0.08, expression("noise " * sigma^2), col = "gray45", cex = 0.85, adj = 1)
dev.off()
