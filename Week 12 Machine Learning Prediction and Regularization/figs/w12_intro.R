## Week 12 -- opening section (why ML, the CEF) and the standard bias-variance pictures.
## Every number on the 'Why Machine Learning' and bias-variance picture frames comes from here.
##   Rscript figs/w12_intro.R > figs/w12_intro.out   (run from the week folder)
## writes figs/cef_gss.pdf, figs/bv_fits.pdf, figs/bv_curve.pdf
set.seed(723)
d <- readRDS("../Data/gss_earnings.rds")
d <- d[complete.cases(d[, c("lnearn", "educ", "age", "female", "black", "otherace", "wordsum", "pareduc",
                            "hours", "childs", "evermar", "region", "year")]), ]
cat("n:", nrow(d), "\n")

## ---- A. The CEF of log earnings: cell means vs the OLS line ------------------
cm <- function(x, min_n = 20) {
  t <- data.frame(x = sort(unique(x)), m = as.vector(tapply(d$lnearn, x, mean)),
                  n = as.vector(table(x)))
  t[t$n >= min_n, ]
}
ce <- cm(d$educ); ca <- cm(d$age)
fe <- lm(lnearn ~ educ, d); fa <- lm(lnearn ~ age, d)
cat("\nA. OLS slope educ:", round(coef(fe)[2], 3), "; OLS slope age:", round(coef(fa)[2], 4), "\n")
cat("cell means by educ (n >= 20):\n"); print(round(ce, 2))
cat("cell means by age, selected:\n"); print(round(ca[ca$x %in% c(20, 25, 30, 40, 50, 60, 65), ], 2))
cat("OLS fitted at age 25, 45, 65:", round(predict(fa, data.frame(age = c(25, 45, 65))), 2), "\n")

pdf("figs/cef_gss.pdf", width = 7.2, height = 2.7, family = "Times")
par(mfrow = c(1, 2), mar = c(3.4, 3.7, 1.6, 0.8), mgp = c(2.2, 0.55, 0), tcl = -0.3)
panel <- function(t, fit, xlab, main) {
  yl <- range(c(t$m, fitted(fit))) + c(-0.08, 0.08)
  plot(t$x, t$m, type = "n", las = 1, bty = "l", xlab = xlab, ylab = "log earnings", ylim = yl,
       main = main, font.main = 1, cex.main = 1.05)
  abline(h = pretty(yl), col = "gray92", lwd = 0.8)
  abline(fit, col = "firebrick", lwd = 2.2)
  points(t$x, t$m, pch = 21, bg = adjustcolor("navy", 0.55), col = "white", lwd = 0.6, cex = 0.7 + sqrt(t$n) / 14)
}
panel(ce, fe, "years of schooling", "schooling")
panel(ca, fa, "age", "age")
m25 <- ca$m[ca$x == 25]; o25 <- predict(fa, data.frame(age = 25))
arrows(25, o25 - 0.03, 25, m25 + 0.04, length = 0.06, col = "gray30", lwd = 1.2)
text(26.5, (m25 + o25) / 2 - 0.05, sprintf("age 25: line %.2f,\naverage %.2f", o25, m25), adj = 0, cex = 0.78, col = "gray20")
dev.off()

## ---- B. Cells run out as covariates are added ------------------------------------
add <- list("schooling" = "educ", "+ age" = "age", "+ female" = "female",
            "+ race" = c("black", "otherace"), "+ region" = "region", "+ parents' educ" = "pareduc",
            "+ vocabulary" = "wordsum")
vars <- c(); cat("\nB. cells as covariates are added\n")
for (k in names(add)) {
  vars <- c(vars, add[[k]])
  key <- do.call(paste, c(d[vars], sep = "|"))
  tab <- table(key)
  cat(sprintf("%-16s cells occupied %5d ; people per cell %6.1f ; share of people alone in their cell %.2f\n",
              k, length(tab), nrow(d) / length(tab), mean(tab[key] == 1)))
}

## ---- C. Underfit / overfit: k-nearest-neighbour averages on a simulated curve ----
## fhat(x) = mean of Y among the k training cases with X closest to x (a local cell mean)
f <- function(x) sin(2 * pi * x)
n <- 50; sig <- 0.4
knn <- function(x, y, x0, k) sapply(x0, function(z) mean(y[order(abs(x - z))[1:k]]))
x <- runif(n); y <- f(x) + rnorm(n, 0, sig)
grid <- seq(0, 1, length.out = 400)
ks <- c(25, 7, 1)
lab <- c("k = 25: too rigid", "k = 7: about right", "k = 1: too flexible")
pdf("figs/bv_fits.pdf", width = 7.2, height = 2.4, family = "Times")
par(mfrow = c(1, 3), mar = c(2.6, 2.6, 1.6, 0.4), mgp = c(1.5, 0.5, 0))
for (j in 1:3) {
  plot(x, y, pch = 19, cex = 0.6, col = "gray40", ylim = c(-2, 2), xlab = "X", ylab = "Y", las = 1,
       main = lab[j], font.main = 1, cex.main = 1.05)
  lines(grid, f(grid), col = "navy", lwd = 2)
  lines(grid, knn(x, y, grid, ks[j]), col = "firebrick", lwd = 1.8, type = "s")
}
dev.off()

## repeated samples: bias^2 and variance of fhat(x), averaged over x
K <- c(50, 40, 30, 25, 20, 15, 12, 10, 8, 7, 6, 5, 4, 3, 2, 1); R <- 1000
xs <- seq(0.05, 0.95, length.out = 50)
pred <- array(NA, c(R, length(K), length(xs))); tr <- matrix(NA, R, length(K))
for (r in 1:R) {
  x <- runif(n); y <- f(x) + rnorm(n, 0, sig)
  for (j in seq_along(K)) {
    pred[r, j, ] <- knn(x, y, xs, K[j])
    tr[r, j] <- mean((y - knn(x, y, x, K[j]))^2)
  }
}
bias2 <- sapply(seq_along(K), function(j) mean((colMeans(pred[, j, ]) - f(xs))^2))
vari <- sapply(seq_along(K), function(j) mean(apply(pred[, j, ], 2, var)))
test <- bias2 + vari + sig^2
out <- data.frame(k = K, bias2 = bias2, variance = vari, sig2_over_k = sig^2 / K, test = test, train = colMeans(tr))
cat("\nC. k-NN, averaged over x and", R, "samples of n =", n, "; sigma^2 =", sig^2, "\n")
print(round(out, 3))
cat("best k:", K[which.min(test)], "\n")

pdf("figs/bv_curve.pdf", width = 5.4, height = 3.1, family = "Times")
par(mar = c(3.3, 3.6, 0.6, 0.6), mgp = c(2.1, 0.6, 0))
xx <- 1 / K
plot(xx, test, log = "x", type = "l", col = "black", lwd = 2.5, ylim = c(0, 0.75), las = 1, xaxt = "n",
     xlab = "flexibility: k neighbours averaged (fewer = more flexible)", ylab = "average squared error")
axis(1, at = 1 / c(50, 20, 10, 5, 2, 1), labels = c(50, 20, 10, 5, 2, 1))
lines(xx, bias2, col = "navy", lwd = 2)
lines(xx, vari, col = "firebrick", lwd = 2)
lines(xx, out$train, col = "darkgreen", lwd = 2, lty = 2)
abline(h = sig^2, col = "gray55", lty = 3)
points(xx[which.min(test)], min(test), pch = 19, cex = 1.1)
text(1 / 4, 0.27, "test error", adj = 0.5, cex = 0.8)
text(1 / 14, 0.06, expression(bias^2), col = "navy", adj = 0.5, cex = 0.8)
text(1 / 1.3, 0.2, "variance", col = "firebrick", adj = 1, cex = 0.8)
text(1 / 1.6, 0.03, "training error", col = "darkgreen", adj = 1, cex = 0.8)
text(1 / 48, 0.185, expression("noise " * sigma^2), col = "gray45", adj = 0, cex = 0.8)
dev.off()
