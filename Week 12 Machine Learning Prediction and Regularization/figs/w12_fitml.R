## Week 12 -- 690S Week 3 'fitML' illustration: fit the noiseless curve Y = exp(4X) with
## a shallow tree, a deep tree, a random forest, boosted trees, and a one-layer network.
##   Rscript figs/w12_fitml.R > figs/w12_fitml.out   (run from the week folder)
## writes figs/fitml_trees.pdf (4 panels) and figs/fitml_nn.pdf (2 panels)
suppressMessages({library(rpart); library(randomForest); library(gbm); library(nnet)})
set.seed(1)
x <- runif(1000); y <- exp(4 * x)                       # noiseless, as in 690S
d <- data.frame(x = x, y = y)
shallow <- rpart(y ~ x, d, cp = 0.01)                   # cp = complexity penalty
deep    <- rpart(y ~ x, d, cp = 0.0005)
rf      <- randomForest(y ~ x, d, ntree = 500, nodesize = 5)
bt      <- gbm(y ~ x, data = d, distribution = "gaussian", n.trees = 100, shrinkage = 0.05, interaction.depth = 3)
leaves <- function(t) sum(t$frame$var == "<leaf>")
fits <- list(`shallow tree (cp = .01)` = predict(shallow, d), `deep tree (cp = .0005)` = predict(deep, d),
             `random forest (500 trees)` = predict(rf, d), `boosting (100 steps, rate .05)` = predict(bt, d, n.trees = 100))
cat("leaves: shallow", leaves(shallow), "; deep", leaves(deep), "\n")
print(round(sapply(fits, function(f) sqrt(mean((y - f)^2))), 3))   # RMSE against the true curve
o <- order(x)
pdf("figs/fitml_trees.pdf", width = 9, height = 5.2, family = "Times")
par(mfrow = c(2, 2), mar = c(3.6, 3.8, 1.8, 0.6), mgp = c(2.2, 0.7, 0))
for (nm in names(fits)) {
  plot(x[o], y[o], type = "l", lwd = 2, col = "navy", xlab = "X", ylab = "g(X)", main = nm, cex.main = 0.95, font.main = 1)
  points(x, fits[[nm]], pch = 16, cex = 0.35, col = adjustcolor("firebrick", 0.6))
}
dev.off()
## one hidden layer (nnet), 10 units: stop the optimizer after 5 iterations vs. let it run
ys <- (y - mean(y)) / sd(y)
X1 <- matrix(x)
n5   <- nnet(X1, ys, size = 10, linout = TRUE, maxit = 5,   trace = FALSE)
n500 <- nnet(X1, ys, size = 10, linout = TRUE, maxit = 500, trace = FALSE)
nn <- list(`network, 5 iterations` = mean(y) + sd(y) * as.vector(predict(n5, X1)), `network, 500 iterations` = mean(y) + sd(y) * as.vector(predict(n500, X1)))
print(round(sapply(nn, function(f) sqrt(mean((y - f)^2))), 3))
pdf("figs/fitml_nn.pdf", width = 9, height = 2.9, family = "Times")
par(mfrow = c(1, 2), mar = c(3.6, 3.8, 1.8, 0.6), mgp = c(2.2, 0.7, 0))
for (nm in names(nn)) {
  plot(x[o], y[o], type = "l", lwd = 2, col = "navy", xlab = "X", ylab = "g(X)", main = nm, cex.main = 0.95, font.main = 1)
  points(x, nn[[nm]], pch = 16, cex = 0.35, col = adjustcolor("firebrick", 0.6))
}
dev.off()

## ---- how the 500-iteration network builds its curve: 10 hidden S-curves, weighted and added ----
## nnet weights, size 10, one input: unit k has (bias w_k0, weight w_k1); then output bias b0 and b1..b10
cat("\nfirst three training pairs (X_i, Y_i):\n"); print(round(head(d, 3), 3))
cat("parameters: 10 x 2 + 11 =", length(n500$wts), "\n")
## by hand: K = 4 hidden units with chosen centres and steepness, A_k(x) = sigmoid(w_k0 + w_k1 x),
## then the output layer = OLS of Y on A_1..A_4 (training would also move w_k0 and w_k1)
w1 <- 15; ctr <- c(0.3, 0.55, 0.75, 0.9); w0 <- -w1 * ctr
Ad <- sapply(1:4, function(k) plogis(w0[k] + w1 * d$x))
ols <- lm(d$y ~ Ad); bet <- coef(ols)
cat("hand network: w_k1 =", w1, "; centres", ctr, "; w_k0 =", w0, "\n")
cat("output layer by OLS: beta_0 .. beta_4 =", round(bet, 2), "; RMSE", round(sqrt(mean(resid(ols)^2)), 3), "\n")
grid <- seq(0, 1, length.out = 400)
A <- sapply(1:4, function(k) plogis(w0[k] + w1 * grid))
pieces <- sweep(A, 2, bet[-1], "*"); fsum <- bet[1] + rowSums(pieces)
xi <- 0.8; Ai <- plogis(w0 + w1 * xi)
cat("worked case X = 0.8: A =", round(Ai, 3), "; f =", round(bet[1], 2), "+", paste(round(bet[-1], 2), "x", round(Ai, 3), collapse = " + "),
    "=", round(bet[1] + sum(bet[-1] * Ai), 2), "; truth", round(exp(4 * xi), 2), "\n")
cols <- c("navy", "darkgreen", "darkorange3", "purple4")
pdf("figs/nn_pieces.pdf", width = 7.2, height = 2.6, family = "Times")
par(mfrow = c(1, 2), mar = c(3.0, 3.4, 1.6, 0.6), mgp = c(1.9, 0.5, 0))
plot(grid, A[, 1], type = "n", ylim = c(0, 1), las = 1, xlab = "X", ylab = "hidden unit value",
     main = "4 hidden units: S-curves in X", font.main = 1, cex.main = 1.05)
for (k in 1:4) lines(grid, A[, k], col = cols[k], lwd = 2)
text(ctr, 0.5, paste0("A", 1:4), pos = 4, cex = 0.8, col = cols)
plot(grid, exp(4 * grid), type = "n", ylim = range(c(pieces, fsum, exp(4 * grid))), las = 1, xlab = "X", ylab = "Y",
     main = "output: weighted sum", font.main = 1, cex.main = 1.05)
for (k in 1:4) lines(grid, pieces[, k], col = adjustcolor(cols[k], 0.6), lwd = 1.2, lty = 3)
lines(grid, exp(4 * grid), col = "navy", lwd = 3)
lines(grid, fsum, col = "firebrick", lwd = 1.6, lty = 2)
legend("topleft", c("truth", "network", expression(beta[k] * A[k])), col = c("navy", "firebrick", "gray40"),
       lwd = c(3, 1.6, 1.2), lty = c(1, 2, 3), bty = "n", cex = 0.8)
dev.off()
