## Week 10 -- Card and Krueger (1994) 2x2 as a picture: FTE employment per store, their Table 3
## (PA 79 stores, NJ 331 stores, February and November 1992). Dashed = NJ had it moved like PA.
##   Rscript figs/w10_ck_parallel.R   (from the week folder; writes figs/ck_parallel.pdf)
pa <- c(23.33, 21.17); nj <- c(20.44, 21.03)
cf <- nj[1] + diff(pa)                                  ## NJ without the law, under parallel trends
cat("NJ counterfactual November:", cf, "; DiD:", nj[2] - cf, "\n")
x <- c(1, 2)
pdf("figs/ck_parallel.pdf", width = 6.4, height = 4.4, family = "Times")
par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.2, cex.axis = 1.1)
plot(x, pa, type = "b", pch = 19, lwd = 2.5, col = "navy", xlim = c(0.6, 2.9), ylim = c(17.5, 24.5),
     xaxt = "n", xlab = "", ylab = "FTE employment per store")
axis(1, at = x, labels = c("February 1992\n(before)", "November 1992\n(after)"), padj = 0.5)
lines(x, nj, type = "b", pch = 19, lwd = 2.5, col = "firebrick")
lines(x, c(nj[1], cf), lty = 2, lwd = 2, col = "firebrick")
arrows(2.08, cf, 2.08, nj[2], code = 3, length = 0.06, lwd = 1.5)
## their Table 3 reports 2.76 from unrounded means; the rounded cell means give 2.75
text(2.1, (cf + nj[2]) / 2, "effect = 2.76", pos = 4, cex = 1.0)
text(2.05, pa[2] + 0.35, sprintf("PA %.2f", pa[2]), pos = 4, cex = 0.95, col = "navy")
text(2.05, nj[2] - 0.35, sprintf("NJ %.2f", nj[2]), pos = 4, cex = 0.95, col = "firebrick")
text(2.05, cf - 0.25, sprintf("NJ without the law %.2f", cf), pos = 4, cex = 0.95, col = "firebrick")
text(0.95, pa[1], sprintf("PA %.2f", pa[1]), pos = 2, cex = 0.95, col = "navy")
text(0.95, nj[1], sprintf("NJ %.2f", nj[1]), pos = 2, cex = 0.95, col = "firebrick")
dev.off()
