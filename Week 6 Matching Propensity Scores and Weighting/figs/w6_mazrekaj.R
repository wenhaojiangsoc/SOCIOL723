## Week 6 -- figure for the Mazrekaj, De Witte and Cabus (2020, ASR) frames.
## Numbers transcribed from their Table 2 (test scores, end of primary school)
## and Table 4 (Oster bounds). Run from the week folder: Rscript figs/w6_mazrekaj.R
est <- c(0.106, 0.054, 0.194, 0.139, 0.147)          # Table 2, columns 1-5
se  <- c(0.019, 0.018, 0.024, 0.023, 0.041)
lab <- c("all, no controls", "all, controls", "from birth, no controls",
         "from birth, controls", "from birth, CEM")
delta <- c(0, 1, 1.5, 2, 3.19)                        # 0 = the controlled OLS estimate
bnd   <- c(0.139, 0.119, 0.093, 0.070, 0.044)         # Table 4, columns 1-4
bse   <- c(0.023, 0.015, 0.016, 0.016, 0.038)

pdf("figs/mazrekaj.pdf", width = 8.4, height = 2.5)
par(mfrow = c(1, 2), mar = c(3.4, 9.5, 1.6, 0.8), mgp = c(2, 0.6, 0), cex = 0.8)
## (a) Table 2
y <- 5:1
plot(est, y, xlim = c(0, 0.26), ylim = c(0.5, 5.5), pch = 19, col = "#012169",
     yaxt = "n", ylab = "", xlab = "test-score gap, SD units", main = "Table 2: same-sex parents")
segments(est - 1.96 * se, y, est + 1.96 * se, y, col = "#012169", lwd = 2)
axis(2, at = y, labels = lab, las = 1)
abline(v = 0, lty = 3, col = "gray50")
## (b) Table 4
par(mar = c(3.4, 3.6, 1.6, 0.8))
plot(delta, bnd, type = "b", pch = 19, col = "#C84E00", lwd = 2, ylim = c(-0.05, 0.2),
     xlab = "selection ratio  (unobservables / observables)", ylab = "bounded effect",
     main = "Table 4: Oster bounds")
segments(delta, bnd - 1.96 * bse, delta, bnd + 1.96 * bse, col = "#C84E00", lwd = 2)
abline(h = 0, lty = 3, col = "gray50")
text(3.19, bnd[5], "3.19: not significant", pos = 2, cex = 0.85, col = "#C84E00", offset = 0.8)
dev.off()
cat("wrote figs/mazrekaj.pdf\n")
