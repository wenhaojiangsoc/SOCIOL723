## =============================================================================
## w13_surfaces.R -- the two moment surfaces on the orthogonality slide
## Exact population moments at the true tau when the nuisance predictions are off
## by constants: outcome error a (in l-hat or g-hat), treatment error b (in e-hat).
##   orthogonal score  [(Y - l) - tau (D - e)](D - e):  E = (a - tau b) b
##   naive score       (Y - tau D - g) D:               E = -a E[D]
## (derivations: "Where the Product Comes From" and "Orthogonality, Derived").
## Run from the Week 13 folder: Rscript figs/w13_surfaces.R
## =============================================================================
tau <- 0.5; ED <- 0.5                        # tau of the w13_sims.R design; E[D] = .5
a <- b <- seq(-.3, .3, length.out = 41)
M_orth  <- outer(a, b, function(a, b) (a - tau * b) * b)
M_naive <- outer(a, b, function(a, b) -a * ED + 0 * b)
cat("orthogonal moment at (a,b) = (.1,.1):", (0.1 - tau * 0.1) * 0.1,
    "  naive at a = .1:", -0.1 * ED, "\n")
cat("orthogonal at (.3,.3):", (0.3 - tau * 0.3) * 0.3, "  naive at a = .3:", -0.3 * ED, "\n")

navy <- rgb(1, 33, 105, maxColorValue = 255); fire <- "firebrick"
shade <- function(M, col) {                 # facet colour by height
  z <- (M[-1, -1] + M[-1, -41] + M[-41, -1] + M[-41, -41]) / 4
  ramp <- colorRampPalette(c(navy, "gray90", fire))(100)
  ramp[cut(z, seq(-.16, .16, length.out = 101), include.lowest = TRUE)]
}
pdf("figs/orth_surfaces.pdf", width = 8, height = 3.6)
par(mfrow = c(1, 2), mar = c(1, 1, 2, 1))
for (k in 1:2) {
  M <- if (k == 1) M_orth else M_naive
  p <- persp(a, b, M, zlim = c(-.16, .16), theta = 35, phi = 25, expand = .7,
             col = shade(M), border = adjustcolor("gray40", .25), ticktype = "detailed",
             nticks = 3, cex.axis = .6, cex.lab = .75,
             xlab = "error in outcome model", ylab = "error in e(X)", zlab = "moment",
             main = if (k == 1) "Orthogonal score: flat at the truth" else "Naive score: tilted at the truth",
             font.main = 1, cex.main = .9)
  lines(trans3d(a, 0, if (k == 1) 0 * a else -a * ED, p), lwd = 2, col = "black")
  points(trans3d(0, 0, 0, p), pch = 19, cex = 1)
}
dev.off()
