## Week 12 -- training / validation / test and K-fold cross-validation on the k-NN example of figs/w12_intro.R
## (f(x) = sin(2 pi x), sigma = 0.4). Every number on the 'Training, Validation, Test' frames comes from here.
##   Rscript figs/w12_split.R > figs/w12_split.out   (run from the week folder)
## writes figs/cv_knn.pdf
set.seed(723)
f <- function(x) sin(2 * pi * x); sig <- 0.4
knn <- function(x, y, x0, k) sapply(x0, function(z) mean(y[order(abs(x - z))[1:k]]))
K <- c(1, 2, 3, 4, 5, 7, 10, 15, 20, 30, 40, 60)
ntr <- 100; nte <- 50; nf <- 5
xg <- seq(0.0025, 0.9975, by = 0.005)                     # grid for the true error of a fitted rule
true_err <- function(x, y, k) sig^2 + mean((knn(x, y, xg, k) - f(xg))^2)

one_run <- function() {
  x <- runif(ntr); y <- f(x) + rnorm(ntr, 0, sig)            # training set
  xt <- runif(nte); yt <- f(xt) + rnorm(nte, 0, sig)         # test set, never used to choose k
  fold <- sample(rep(1:nf, length.out = ntr))
  tab <- t(sapply(K, function(k) {
    cv <- mean(sapply(1:nf, function(b) {
      i <- fold == b; mean((y[i] - knn(x[!i], y[!i], x[i], k))^2) }))
    c(k = k, train = mean((y - knn(x, y, x, k))^2), cv = cv,
      test = mean((yt - knn(x, y, xt, k))^2), true = true_err(x, y, k))
  }))
  as.data.frame(tab)
}

## ---- A. one sample, shown on the slide ---------------------------------------------
tab <- one_run()
cat("A. one sample: 100 training cases (5 folds of 20), 50 test cases\n"); print(round(tab, 3))
kc <- tab$k[which.min(tab$cv)]
cat("k chosen by training error:", tab$k[which.min(tab$train)], "; by 5-fold CV:", kc,
    "; refit on all 100, test MSE", round(tab$test[tab$k == kc], 3), "; true error", round(tab$true[tab$k == kc], 3), "\n")
cat("best possible k (true error):", tab$k[which.min(tab$true)], round(min(tab$true), 3), "\n")

pdf("figs/cv_knn.pdf", width = 5.4, height = 3.0, family = "Times")
par(mar = c(3.3, 3.6, 0.6, 0.6), mgp = c(2.1, 0.6, 0))
xx <- 1 / tab$k
plot(xx, tab$cv, log = "x", type = "b", pch = 19, col = "navy", lwd = 2, ylim = c(0, 0.6), las = 1, xaxt = "n",
     xlab = "k neighbours averaged (fewer = more flexible)", ylab = "mean squared error")
axis(1, at = 1 / c(60, 20, 10, 5, 2, 1), labels = c(60, 20, 10, 5, 2, 1))
lines(xx, tab$test, type = "b", pch = 17, col = "firebrick", lwd = 2, lty = 2)
lines(xx, tab$train, type = "b", pch = 15, col = "darkgreen", lwd = 2)
abline(h = sig^2, col = "gray55", lty = 3)
points(1 / kc, min(tab$cv), pch = 1, cex = 2.2, lwd = 2)
legend("bottomleft", c("5-fold CV (training set only)", "test set (50 held-out cases)", "training error"),
       col = c("navy", "firebrick", "darkgreen"), pch = c(19, 17, 15), lty = c(1, 2, 1), lwd = 2, bty = "n", cex = 0.8)
dev.off()

## ---- B. 500 samples: what each way of choosing k reports, vs the truth ------------------
R <- 500
res <- t(replicate(R, {
  t <- one_run()
  jc <- which.min(t$cv); jt <- which.min(t$test)
  c(cv_k = t$k[jc], cv_reported_test = t$test[jc], cv_true = t$true[jc],
    test_k = t$k[jt], test_reported = t$test[jt], test_true = t$true[jt],
    train_k = t$k[which.min(t$train)], train_reported = min(t$train))
}))
cat("\nB.", R, "samples; averages:\n"); print(round(colMeans(res), 3))
cat("tune by CV, then report test: reported", round(mean(res[, "cv_reported_test"]), 3),
    "vs true", round(mean(res[, "cv_true"]), 3), "\n")
cat("tune on the test set and report that test error: reported", round(mean(res[, "test_reported"]), 3),
    "vs true", round(mean(res[, "test_true"]), 3), "\n")
cat("tune on training error: k = 1 in", mean(res[, "train_k"] == 1), "of samples; reported", round(mean(res[, "train_reported"]), 3), "\n")
