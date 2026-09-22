## Week 5, potential outcomes: numbers for the Neyman, simulation, and bounds frames.
## Run from the week folder. Writes figs/po_sims.pdf.
set.seed(723)
n <- 2000
## fixed population: X = parents went to college; Y(0) higher for X = 1;
## tau_i LOWER for X = 1 (negative selection, as in Brand and Xie 2010)
X   <- rbinom(n, 1, 0.5)
Y0  <- 10 + 4 * X + rnorm(n, 0, 2)
tau <- 6 - 4 * X + rnorm(n, 0, 1)
Y1  <- Y0 + tau
ATE <- mean(tau)
p_sel <- ifelse(X == 1, 0.7, 0.3)          # selection: X = 1 treated more often
R <- 2000
est <- matrix(NA, R, 3, dimnames = list(NULL, c("naive_selection", "naive_random", "adjusted_selection")))
att <- atu <- base <- neyman_se <- hc2_se <- numeric(R)
suppressPackageStartupMessages(library(sandwich))
for (r in 1:R) {
  D <- rbinom(n, 1, p_sel); Y <- D * Y1 + (1 - D) * Y0
  est[r, 1] <- mean(Y[D == 1]) - mean(Y[D == 0])
  att[r] <- mean(tau[D == 1]); atu[r] <- mean(tau[D == 0]); base[r] <- mean(Y0[D == 1]) - mean(Y0[D == 0])
  ## the identification formula: within-X differences, averaged with the X shares
  cell <- sapply(0:1, function(x) mean(Y[D == 1 & X == x]) - mean(Y[D == 0 & X == x]))
  est[r, 3] <- sum(cell * table(X) / n)
  Dr <- rbinom(n, 1, 0.5); Yr <- Dr * Y1 + (1 - Dr) * Y0
  est[r, 2] <- mean(Yr[Dr == 1]) - mean(Yr[Dr == 0])
  n1 <- sum(Dr); n0 <- n - n1
  neyman_se[r] <- sqrt(var(Yr[Dr == 1]) / n1 + var(Yr[Dr == 0]) / n0)
  hc2_se[r] <- sqrt(vcovHC(lm(Yr ~ Dr), type = "HC2")[2, 2])
}
cat("ATE", round(ATE, 2), " ATT (avg over draws)", round(mean(att), 2), " ATU", round(mean(atu), 2), "\n")
cat("baseline selection E[Y0|D=1]-E[Y0|D=0] (avg over draws):", round(mean(base), 2), " ATT + baseline:", round(mean(att) + mean(base), 2), "\n")
cat("means of estimates:", round(colMeans(est), 2), "\n")
cat("SD of estimates:", round(apply(est, 2, sd), 3), "\n")
cat("randomized: SD of estimate", round(sd(est[, 2]), 3), " mean Neyman SE", round(mean(neyman_se), 3),
    " mean HC2 SE", round(mean(hc2_se), 3), " max |Neyman - HC2|", signif(max(abs(neyman_se - hc2_se)), 2), "\n")
## one draw for the Neyman frame
set.seed(1); Dr <- rbinom(n, 1, 0.5); Yr <- Dr * Y1 + (1 - Dr) * Y0
cat("one experiment: n1", sum(Dr), " ybar1", round(mean(Yr[Dr == 1]), 2), " ybar0", round(mean(Yr[Dr == 0]), 2),
    " diff", round(mean(Yr[Dr == 1]) - mean(Yr[Dr == 0]), 2), " s1^2", round(var(Yr[Dr == 1]), 2), " s0^2", round(var(Yr[Dr == 0]), 2),
    " SE", round(sqrt(var(Yr[Dr == 1]) / sum(Dr) + var(Yr[Dr == 0]) / (n - sum(Dr))), 3), "\n")
## Manski bounds on GSS: D = BA, Y = earnings above the sample median
gss <- readRDS("../Data/gss_earnings.rds")
Y <- as.integer(gss$realrinc > median(gss$realrinc)); D <- gss$ba
p1 <- mean(D); y1 <- mean(Y[D == 1]); y0 <- mean(Y[D == 0])
cat("GSS: P(D=1)", round(p1, 3), " P(Y=1|D=1)", round(y1, 3), " P(Y=1|D=0)", round(y0, 3), " naive", round(y1 - y0, 3), "\n")
EY1 <- c(y1 * p1, y1 * p1 + (1 - p1)); EY0 <- c(y0 * (1 - p1), y0 * (1 - p1) + p1)
cat("E[Y(1)] in [", round(EY1, 3), "]  E[Y(0)] in [", round(EY0, 3), "]  ATE in [", round(EY1[1] - EY0[2], 3), ",", round(EY1[2] - EY0[1], 3), "]\n")

dukeblue <- rgb(0, 83, 155, maxColorValue = 255); accent <- rgb(200, 78, 0, maxColorValue = 255)
pdf("figs/po_sims.pdf", width = 8.4, height = 2.1)
par(mfrow = c(1, 3), mar = c(3.2, 1.2, 1.8, 0.6), mgp = c(1.9, 0.6, 0), cex = 0.8)
ttl <- c("Selection on X: naive difference", "Randomized: naive difference", "Selection on X: within-X differences")
br <- seq(3.3, 5.3, by = 0.04)
for (k in 1:3) {
  hist(est[, k], breaks = br, col = "grey80", border = "grey80", main = ttl[k], xlab = "estimate of the ATE",
       ylab = "", yaxt = "n", font.main = 1, cex.main = 0.95)
  abline(v = ATE, col = accent, lwd = 2.5)
  mtext(sprintf("mean %.2f", mean(est[, k])), side = 3, line = -1.2, adj = ifelse(k == 1, 0.05, 0.95), col = dukeblue, cex = 0.75)
}
dev.off()
