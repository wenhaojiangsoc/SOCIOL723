## Week 7 -- shift-share (Bartik) instruments by simulation.
## Every number on the shift-share simulation frames comes from here.
##   Rscript figs/w7_shiftshare.R > figs/w7_shiftshare.out ; writes figs/shiftshare_sim.pdf
set.seed(723)
beta <- 1               # true effect of the local shock x on the local outcome y
L <- 300; K <- 80       # regions, industries
R <- 1000

## shares: fixed across reps so that "shares" and "shocks" stories can be separated
S <- matrix(rgamma(L * K, shape = 0.3), L, K); S <- S / rowSums(S)   # rows sum to one; regions specialise
sbar <- colMeans(S)                                      # average exposure of each industry
cat(sprintf("effective number of shocks 1/sum(sbar^2) = %.1f (K = %d)\n", 1 / sum(sbar^2), K))

ssiv <- function(y, x, z) sum((z - mean(z)) * y) / sum((z - mean(z)) * x)
hc_se <- function(y, x, z, b) {           # robust SE of the just-identified IV estimate
  e <- y - mean(y) - b * (x - mean(x)); zc <- z - mean(z)
  sqrt(sum(zc^2 * e^2)) / abs(sum(zc * (x - mean(x))))
}
## BHJ shock-level regression: aggregate y and x to industries with exposure weights,
## instrument with the shocks themselves, robust SE clustered by industry (= one obs each)
bhj <- function(y, x, g) {
  yc <- y - mean(y); xc <- x - mean(x)
  sk <- colSums(S); ybar <- colSums(S * yc) / sk; xbar <- colSums(S * xc) / sk
  gc <- g - sum(sk * g) / sum(sk)
  b  <- sum(sk * gc * ybar) / sum(sk * gc * xbar)
  e  <- ybar - b * xbar
  se <- sqrt(sum((sk * gc)^2 * e^2)) / abs(sum(sk * gc * xbar))
  c(b = unname(b), se = unname(se))
}

scenario <- function(name, shocks_ok = TRUE, shares_endog = FALSE, industry_err = FALSE) {
  res <- t(replicate(R, {
    g   <- rnorm(K, 0, 2.5)                           # national industry shocks
    nu  <- rnorm(K)                                   # industry-level unobservable
    if (!shocks_ok) g <- g + 1.5 * nu                 # shocks track the unobservable
    eta <- rnorm(L)                                   # local supply shock (confounder)
    z   <- as.vector(S %*% g)                         # the shift-share instrument
    x   <- z + eta
    e   <- 0.8 * eta + rnorm(L, 0, 0.5)               # OLS confounding through eta
    if (shares_endog) e <- e + 2 * (S[, 1] - mean(S[, 1]))   # share of industry 1 shifts y
    if (industry_err || !shocks_ok) e <- e + 2 * as.vector(S %*% nu)   # exposure-weighted industry error
    y   <- beta * x + e
    b_ols <- cov(x, y) / var(x); b_iv <- ssiv(y, x, z)
    se_c  <- hc_se(y, x, z, b_iv); bh <- bhj(y, x, g)
    c(ols = b_ols, ssiv = b_iv, r2 = cor(x, z)^2,
      cover_conv = as.numeric(abs(b_iv - beta) <= 1.96 * se_c),
      cover_bhj  = as.numeric(abs(bh[["b"]] - beta) <= 1.96 * bh[["se"]]))
  }))
  cat(sprintf("\n==== %s ====\n", name))
  cat(sprintf("first-stage R2, mean over reps %.2f\n", mean(res[, "r2"])))
  cat(sprintf("OLS   mean %.3f  sd %.3f\n", mean(res[, "ols"]), sd(res[, "ols"])))
  cat(sprintf("SSIV  mean %.3f  sd %.3f\n", mean(res[, "ssiv"]), sd(res[, "ssiv"])))
  cat(sprintf("95%% CI coverage: conventional robust %.3f   BHJ shock-level %.3f\n",
              mean(res[, "cover_conv"]), mean(res[, "cover_bhj"])))
  invisible(res)
}

r1 <- scenario("A. shocks random, shares exogenous (both stories hold)")
r2 <- scenario("B. shares predict the outcome directly, shocks random (shocks story only)", shares_endog = TRUE)
r3 <- scenario("C. shocks correlated with an industry unobservable (shocks story fails)", shocks_ok = FALSE)
r4 <- scenario("D. valid instrument, exposure-weighted industry error (inference problem)", industry_err = TRUE)

pdf("figs/shiftshare_sim.pdf", width = 7.5, height = 3.6, family = "Times")
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), cex = 1.15)
d_ols <- density(r1[, "ols"]); d_iv <- density(r1[, "ssiv"])
plot(d_iv, main = "A. valid design", xlab = expression(hat(beta)), ylab = "density",
     xlim = range(c(d_ols$x, d_iv$x)), ylim = c(0, max(d_ols$y, d_iv$y)), col = "navy", lwd = 2)
lines(d_ols, col = "firebrick", lwd = 2); abline(v = beta, lty = 3)
legend("topright", c("OLS", "shift-share IV"), col = c("firebrick", "navy"), lwd = 2, bty = "n")
d_ols <- density(r3[, "ols"]); d_iv <- density(r3[, "ssiv"])
plot(d_iv, main = "C. shocks not random", xlab = expression(hat(beta)), ylab = "density",
     xlim = range(c(d_ols$x, d_iv$x)), ylim = c(0, max(d_ols$y, d_iv$y)), col = "navy", lwd = 2)
lines(d_ols, col = "firebrick", lwd = 2); abline(v = beta, lty = 3)
dev.off()
