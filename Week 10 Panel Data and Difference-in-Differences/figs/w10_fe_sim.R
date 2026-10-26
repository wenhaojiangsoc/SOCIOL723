## Week 10 -- fixed vs random intercepts, without time: a simulation.
##   Rscript figs/w10_fe_sim.R > figs/w10_fe_sim.out ; writes figs/fere_worlds.pdf, figs/fere_sims.pdf
## Y_ij = alpha_j + beta X_ij + e_ij, beta = 1; X_ij = m_j + u_ij.
## World A: alpha_j unrelated to X (RE assumption holds). World B: alpha_j = 1.5 m_j + noise.
## Var(alpha_j) is the same in both worlds; only its covariance with X differs.
suppressMessages({library(fixest); library(plm)}); setFixest_notes(FALSE)
set.seed(1010)
beta <- 1
draw <- function(J, n, world) {
  m <- rnorm(J); v <- rnorm(J)
  k <- if (world == "A") 0 else 0.8
  a <- 2 * (k * m + sqrt(1 - k^2) * v)                 # sd(alpha) = 2 in both worlds
  g <- rep(seq_len(J), each = n); x <- m[g] + rnorm(J * n)
  data.frame(g = g, x = x, y = a[g] + beta * x + rnorm(J * n, sd = 1.5))
}

## ---- 1. one sample of 6 groups per world: the picture ---------------------------------
pal <- c("navy", "firebrick", "darkgreen", "darkorange", "purple", "gray35")
pdf("figs/fere_worlds.pdf", width = 9, height = 3.4, family = "Times")
par(mfrow = c(1, 2), mar = c(4, 4.2, 2, 0.6), cex.lab = 1.15, cex.axis = 1.05, cex.main = 1.2)
for (w in c("A", "B")) {
  set.seed(if (w == "A") 184 else 20); d <- draw(6, 25, w)   # seeds chosen so each picture is typical of its world
  fe <- coef(feols(y ~ x | g, d))[["x"]]; po <- coef(lm(y ~ x, d))
  plot(d$x, d$y, pch = 19, cex = 0.55, col = adjustcolor(pal[d$g], 0.6), xlab = "X", ylab = "Y",
       main = if (w == "A") "A: intercepts unrelated to X" else "B: intercepts rise with X",
       xlim = c(-3.5, 3.5), ylim = c(-9, 9))
  for (j in 1:6) { s <- d[d$g == j, ]; xs <- range(s$x)
    lines(xs, mean(s$y) + fe * (xs - mean(s$x)), col = pal[j], lwd = 2.5) }
  abline(po, lwd = 2.5, lty = 2)
  legend("bottomright", c(sprintf("within groups: slope %.2f", fe), sprintf("pooled: slope %.2f", po[2])),
         lwd = 2.5, lty = c(1, 2), col = c("gray35", "black"), bty = "n", cex = 0.95)
  cat("picture, world", w, ": FE slope", round(fe, 2), "; pooled slope", round(po[2], 2), "\n")
}
dev.off()

## ---- 2. 1,000 samples per world: J = 100 groups of n = 4 ---------------------------------
R <- 1000
one <- function(world) {
  d <- draw(100, 4, world); d$xbar <- ave(d$x, d$g)
  pd <- pdata.frame(d, index = c("g")); pd$id2 <- NULL
  fe <- plm(y ~ x, pd, model = "within"); re <- plm(y ~ x, pd, model = "random")
  ht <- phtest(fe, re)
  mu <- feols(y ~ x + xbar, d, cluster = ~g)
  c(pooled = coef(lm(y ~ x, d))[["x"]], RE = coef(re)[["x"]], FE = coef(fe)[["x"]],
    se_RE = sqrt(vcov(re)["x", "x"]), se_FE = sqrt(vcov(fe)["x", "x"]),
    hausman_p = ht$p.value, mundlak_p = pvalue(mu)[["xbar"]])
}
res <- lapply(c(A = "A", B = "B"), function(w) t(replicate(R, one(w))))
for (w in c("A", "B")) {
  r <- res[[w]]
  cat("\n## world", w, ": mean and sd over", R, "samples\n")
  print(round(rbind(mean = colMeans(r[, 1:3]), sd = apply(r[, 1:3], 2, sd)), 3))
  cat("rejection rate at 5%: Hausman", mean(r[, "hausman_p"] < .05), "; Mundlak (clustered)",
      mean(r[, "mundlak_p"] < .05), "\n")
}

## ---- 3. sampling distributions -----------------------------------------------------------
pdf("figs/fere_sims.pdf", width = 9, height = 3.2, family = "Times")
par(mfrow = c(1, 2), mar = c(4, 4.2, 2, 0.6), cex.lab = 1.15, cex.axis = 1.05, cex.main = 1.2)
cols <- c(pooled = "gray50", RE = "firebrick", FE = "navy")
for (w in c("A", "B")) {
  r <- res[[w]]; ds <- lapply(c("pooled", "RE", "FE"), function(k) density(r[, k]))
  plot(NA, xlim = c(0.7, 2.1), ylim = c(0, max(sapply(ds, function(z) max(z$y)))),
       xlab = expression(hat(beta)), ylab = "density",
       main = if (w == "A") "A: intercepts unrelated to X" else "B: intercepts rise with X")
  for (k in 1:3) lines(ds[[k]], col = cols[k], lwd = 2.5)
  abline(v = beta, lty = 2)
  legend("topright", c("pooled OLS", "random effects", "fixed effects"), col = cols, lwd = 2.5, bty = "n", cex = 0.95)
}
dev.off()
