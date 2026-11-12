## Week 12 -- boosting by hand, drawn, and what the learning rate does.
## Six people (as in figs/w12_690s.R, H), one-split trees (stumps). Every number on the boosting
## picture frames comes from here.
##   Rscript figs/w12_boost_steps.R > figs/w12_boost_steps.out   (run from the week folder)
## writes figs/boost_step1.pdf (from 0, as the algorithm is written), figs/boost_lr.pdf, figs/boost_lr_test.pdf
x <- c(10, 12, 12, 14, 16, 18); y <- c(9.6, 9.8, 10.0, 10.4, 10.8, 11.0)
sse <- function(v) sum((v - mean(v))^2)
stump <- function(x, r) {                       # best single split of r on x
  cuts <- sort(unique(x)); cuts <- (head(cuts, -1) + tail(cuts, -1)) / 2
  s <- cuts[which.min(sapply(cuts, function(c) sse(r[x <= c]) + sse(r[x > c])))]
  list(cut = s, lo = mean(r[x <= s]), hi = mean(r[x > s]))
}
xs <- seq(9, 19, by = 0.01)
run <- function(lr, J = 4) {                    # returns the stumps, so the fit can be drawn on a grid
  st <- list(); f <- rep(mean(y), 6)
  for (j in 1:J) { s <- stump(x, y - f); s$lr <- lr; st[[j]] <- s
    f <- f + lr * ifelse(x <= s$cut, s$lo, s$hi) }
  st
}
pred <- function(xx, st) mean(y) + Reduce(`+`, lapply(st, function(s) s$lr * ifelse(xx <= s$cut, s$lo, s$hi)), 0)
A <- run(0.5); B <- run(1)
cat("## A. six people, four steps\n")
for (j in 1:4) cat(sprintf("step %d | lr 0.5: cut %.0f, prediction %s, SSR %.3f | lr 1: cut %.0f, prediction %s, SSR %.3f\n",
  j, A[[j]]$cut, paste(sprintf("%.2f", pred(x, A[1:j])), collapse = " "), sum((y - pred(x, A[1:j]))^2),
  B[[j]]$cut, paste(sprintf("%.2f", pred(x, B[1:j])), collapse = " "), sum((y - pred(x, B[1:j]))^2)))
s1 <- A[[1]]
cat("step 1 stump on the residuals:", round(s1$lo, 3), "/", round(s1$hi, 3), "; person with 10 years: residual",
    round(y[1] - mean(y), 2), "-> lr 0.5:", round(y[1] - mean(y) - 0.5 * s1$lo, 2), "; lr 1:", round(y[1] - mean(y) - s1$lo, 2), "\n")

base_plot <- function(ylim, ylab, main) {
  plot(NA, xlim = c(9, 19), ylim = ylim, las = 1, cex.axis = 0.95, xlab = "years of schooling", ylab = ylab,
       main = main, font.main = 1, cex.main = 1.05)
}
## ---- figure 1: the algorithm as written, from a guess of 0: four updates -----------------
cat("\n## B. start from 0 (R_i = Y_i), learning rate 0.5 (and 1 for comparison)\n")
run0 <- function(lr, J = 4) { st <- list(); f <- rep(0, 6)
  for (j in 1:J) { s <- stump(x, y - f); s$lr <- lr; st[[j]] <- s; f <- f + lr * ifelse(x <= s$cut, s$lo, s$hi) }
  st }
pred0 <- function(xx, st) Reduce(`+`, lapply(st, function(s) s$lr * ifelse(xx <= s$cut, s$lo, s$hi)), 0)
Z <- run0(0.5); Z1 <- run0(1)
for (j in 1:4) cat(sprintf("step %d: residuals %s; stump cut %.0f: %.2f / %.2f; prediction %s; lr 1 prediction %s\n", j,
  paste(sprintf("%.2f", y - pred0(x, Z[seq_len(j - 1)])), collapse = " "), Z[[j]]$cut, Z[[j]]$lo, Z[[j]]$hi,
  paste(sprintf("%.2f", pred0(x, Z[1:j])), collapse = " "), paste(sprintf("%.2f", pred0(x, Z1[1:j])), collapse = " ")))
jit <- c(0, -0.15, 0.15, 0, 0, 0)
pdf("figs/boost_step1.pdf", width = 6.4, height = 2.75, family = "Times")
par(mfrow = c(2, 4), mar = c(2.8, 2.6, 1.5, 0.3), mgp = c(1.9, 0.5, 0), cex.lab = 1.05)
for (j in 1:4) {                               # top: residuals entering step j, and the stump
  r <- y - pred0(x, Z[seq_len(j - 1)])
  plot(x + jit, r, pch = 19, xlim = c(9, 19), ylim = c(0, 11.5), las = 1, cex.axis = 0.95,
       xlab = "", ylab = if (j == 1) "residual" else "",
       main = if (j == 1) "step 1: residual = Y" else sprintf("step %d: what is left", j), font.main = 1, cex.main = 1.05)
  lines(xs, ifelse(xs <= Z[[j]]$cut, Z[[j]]$lo, Z[[j]]$hi), type = "s", col = "firebrick", lwd = 2)
}
for (j in 1:4) {                               # bottom: prediction after step j
  plot(x + jit, y, pch = 19, xlim = c(9, 19), ylim = c(0, 11.5), las = 1, cex.axis = 0.95,
       xlab = "years of schooling", ylab = if (j == 1) "prediction" else "",
       main = sprintf("prediction after step %d", j), font.main = 1, cex.main = 1.05)
  if (j > 1) lines(xs, pred0(xs, Z[1:(j - 1)]), type = "s", col = "gray70", lwd = 1.5)
  lines(xs, pred0(xs, Z[1:j]), type = "s", col = "navy", lwd = 2.2)
  if (j == 1) { abline(h = 0, col = "gray70", lwd = 1.5)
    legend("bottomright", c("before this step", "after: + 0.5 x stump"), col = c("gray70", "navy"),
           lwd = 2, bty = "n", cex = 0.85) }
}
dev.off()

## ---- figure 2: four steps, rate 1 vs rate 0.5 ---------------------------------------
pdf("figs/boost_lr.pdf", width = 7.6, height = 2.5, family = "Times")
par(mfrow = c(1, 4), mar = c(3, 3.2, 2.6, 0.4), mgp = c(1.9, 0.5, 0), cex.lab = 1.05)
for (j in 1:4) {
  base_plot(c(9.5, 11.1), if (j == 1) "log earnings" else "", "")
  title(sprintf("after step %d\nSSR: rate 1 %.3f, rate 0.5 %.3f", j, sum((y - pred(x, B[1:j]))^2),
                sum((y - pred(x, A[1:j]))^2)), font.main = 1, cex.main = 0.95, line = 0.4)
  points(x + c(0, -0.12, 0.12, 0, 0, 0), y, pch = 19)
  lines(xs, pred(xs, B[1:j]), type = "s", col = "firebrick", lwd = 1.6, lty = 2)
  lines(xs, pred(xs, A[1:j]), type = "s", col = "navy", lwd = 2.2)
}
dev.off()

## ---- C. 100 noisy cases from a curve: test error by step, three learning rates ---------
cat("\n## C. Y = exp(4X) + N(0, 4^2), n = 100 training, 5,000 test cases; stumps\n")
set.seed(723)
n <- 100; f0 <- function(x) exp(4 * x)
xc <- sort(runif(n)); yc <- f0(xc) + rnorm(n, 0, 4)
xt <- runif(5000); yt <- f0(xt) + rnorm(5000, 0, 4)
st2 <- function(x, r) { cuts <- (head(x, -1) + tail(x, -1)) / 2
  ss <- sapply(cuts, function(c) sse(r[x <= c]) + sse(r[x > c])); c0 <- cuts[which.min(ss)]
  list(c = c0, lo = mean(r[x <= c0]), hi = mean(r[x > c0])) }
J <- 300; rates <- c(1, 0.3, 0.1)
curves <- sapply(rates, function(lr) {
  f <- rep(mean(yc), n); ft <- rep(mean(yc), 5000); te <- numeric(J)
  for (j in 1:J) { s <- st2(xc, yc - f)
    f <- f + lr * ifelse(xc <= s$c, s$lo, s$hi); ft <- ft + lr * ifelse(xt <= s$c, s$lo, s$hi)
    te[j] <- mean((yt - ft)^2) }
  cat(sprintf("rate %.1f: lowest test MSE %.1f at step %d; at step 300 %.1f\n", lr, min(te), which.min(te), te[J]))
  te })
cat("noise variance 16\n")
pdf("figs/boost_lr_test.pdf", width = 5.4, height = 2.9, family = "Times")
par(mar = c(3.2, 3.6, 0.8, 0.6), mgp = c(2.1, 0.55, 0))
cols <- c("firebrick", "darkgreen", "navy")
matplot(1:J, curves, type = "l", lty = 1, lwd = 2, col = cols, log = "x", ylim = c(15, 45), las = 1,
        xlab = "number of steps (log scale)", ylab = "test MSE")
abline(h = 16, col = "gray55", lty = 3)
points(apply(curves, 2, which.min), apply(curves, 2, min), pch = 19, col = cols)
legend("topright", paste("learning rate", rates), col = cols, lwd = 2, bty = "n", cex = 0.85)
text(1.6, 17.3, "noise variance", col = "gray45", cex = 0.8)
dev.off()
