## Week 12 -- the 690S Week 2 sparsity / lasso / post-lasso / dense pictures, redrawn in the
## deck's style (base R, Times, navy = truth, firebrick = lasso, darkgreen = post-lasso).
## Same DGP, seed, and CV call as Lab 12 section 2, so the coefficients match the lab.
##   Rscript figs/w12_sparsity.R > figs/w12_sparsity.out   (run from the week folder)
## writes figs/sparse_beta.pdf, figs/lasso_sim.pdf, figs/postlasso_sim.pdf, figs/dense_beta.pdf
suppressMessages(library(glmnet))
gen_data <- function(n, p, regime = "sparse", npop = 20000) {      # identical to Lab 12
  if (regime == "sparse") {
    beta <- (1 / seq(1:p)^2) * 7
  } else if (regime == "dense") {
    beta <- rnorm(p) * 0.2
  } else if (regime == "sparsedense") {
    beta <- (1 / seq(1:p)^2) * 6.5 + rnorm(p, 0, 0.5) * 0.7
  }
  X <- matrix(runif(n * p, -0.5, 0.5), n, p); y <- X %*% beta + rnorm(n)
  Xpop <- matrix(runif(npop * p, -0.5, 0.5), npop, p); ypop <- Xpop %*% beta + rnorm(npop)
  list(X = X, y = as.vector(y), beta = beta)
}
set.seed(1)
res <- gen_data(100, 400, "sparse"); X <- res$X; y <- res$y; beta <- res$beta
cv <- cv.glmnet(X, y, family = "gaussian", alpha = 1, nfolds = 5)
b_lasso <- as.numeric(coef(cv, s = "lambda.min"))[-1]
kept <- which(b_lasso != 0)
b_post <- rep(0, 400); b_post[kept] <- coef(lm(y ~ X[, kept]))[-1]
cat("lasso keeps", length(kept), "regressors:", kept, "\n")
print(round(rbind(truth = beta[kept], lasso = b_lasso[kept], post = b_post[kept]), 2))

top <- function(b) sort(abs(b), decreasing = TRUE)[1:20]
panel <- function(main = "", ylim = NULL) {
  plot(NA, xlim = c(1, 20), ylim = ylim, xlab = "rank j (largest first)", ylab = "size of the j-th largest coefficient",
       main = main, cex.main = 1.0, font.main = 1, las = 1, bty = "l", tcl = -0.3)
  abline(h = pretty(ylim), col = "gray92", lwd = 0.8)
}
add <- function(b, col, pch = 21) {
  lines(1:20, top(b), col = col, lwd = 1.6)
  points(1:20, top(b), pch = 21, bg = adjustcolor(col, 0.75), col = "white", cex = 1.1, lwd = 0.6)
}
mar <- c(3.4, 4.2, 1.6, 1)

pdf("figs/sparse_beta.pdf", width = 5, height = 2.8, family = "Times")
par(mar = mar, mgp = c(2.2, 0.7, 0)); panel(ylim = c(0, 7)); add(beta, "navy", 19)
dev.off()

## figure: a penalty large enough to keep only the regressors that matter (lambda = 0.4, glmnet scale)
b_two <- as.numeric(coef(cv$glmnet.fit, s = 0.4))[-1]; k2 <- which(b_two != 0)
cat("lambda = 0.4 keeps", k2, ":", round(b_two[k2], 2), "(true", beta[k2], "); CV lambda.min =", round(cv$lambda.min, 3), "\n")
pdf("figs/lasso_sim.pdf", width = 6.4, height = 3.0, family = "Times")
par(mar = c(3.3, 3.8, 0.8, 0.8), mgp = c(2.1, 0.55, 0), tcl = -0.3)
plot(1:400, b_two, log = "x", type = "n", ylim = c(-0.3, 7.4), las = 1, bty = "l",
     xlab = "regressor j (log scale)", ylab = "coefficient")
abline(h = 0, col = "gray75")
lines(1:400, beta, col = "navy", lwd = 1.6)
z <- setdiff(1:400, k2)
points(z, b_two[z], pch = 19, cex = 0.35, col = "gray65")
points(k2, beta[k2], pch = 21, bg = "navy", col = "white", cex = 1.2)
points(k2, b_two[k2], pch = 21, bg = "firebrick", col = "white", cex = 1.6, lwd = 0.8)
lab2 <- sprintf("lasso %.2f (true %.2f)", b_two[k2], beta[k2])
text(1.4, b_two[1], lab2[1], adj = 0, cex = 0.8, col = "firebrick")
segments(2.15, 0.75, 3.3, 1.75, col = "gray55"); text(3.4, 1.9, lab2[2], adj = 0, cex = 0.8, col = "firebrick")
text(60, 0.45, "398 others: exactly 0", col = "gray40", cex = 0.85)
legend("topright", c("true coefficient", "lasso: kept", "lasso: set to 0"), col = c("navy", "firebrick", "gray65"),
       lwd = c(1.6, NA, NA), pch = c(NA, 19, 19), pt.cex = c(NA, 1.2, 0.6), bty = "n", cex = 0.8)
dev.off()

pdf("figs/postlasso_sim.pdf", width = 5.2, height = 3.4, family = "Times")
par(mar = mar, mgp = c(2.2, 0.7, 0)); panel(ylim = c(0, 7)); add(beta, "navy", 19); add(b_lasso, "firebrick", 17)
add(b_post, "darkgreen", 15)
legend("topright", c("truth", "lasso", "post-lasso"), col = c("navy", "firebrick", "darkgreen"),
       pch = 19, lwd = 1.6, bty = "n")
dev.off()

set.seed(1); bd <- gen_data(100, 400, "dense", npop = 10)$beta
set.seed(1); bs <- gen_data(100, 400, "sparsedense", npop = 10)$beta
pdf("figs/dense_beta.pdf", width = 9, height = 3.2, family = "Times")
par(mfrow = c(1, 2), mar = mar, mgp = c(2.2, 0.7, 0))
panel("dense", ylim = c(0, max(top(bd)))); add(bd, "navy", 19)
panel("sparse and dense", ylim = c(0, max(top(bs)))); add(bs, "navy", 19)
dev.off()
cat("dense: largest", round(max(abs(bd)), 2), "; sparse and dense: largest", round(max(abs(bs)), 2), "\n")
