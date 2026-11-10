## Week 12 -- gradient descent on a non-convex R(theta) with a local and a global minimum,
## drawn in the deck's style (replaces the 690S gradient.pdf). Hypothetical function.
##   Rscript figs/w12_gradient.R > figs/w12_gradient.out   (run from the week folder)
## writes figs/gradient_descent.pdf
R  <- function(t) 0.05 * t^4 - 0.55 * t^3 + 1.8 * t^2 - 2.0 * t + 4   # two minima
dR <- function(t) 0.2 * t^3 - 1.65 * t^2 + 3.6 * t - 2.0
descend <- function(t0, rho = 0.15, steps = 25) { th <- t0; for (s in 1:steps) th <- c(th, tail(th, 1) - rho * dR(tail(th, 1))); th }
left  <- descend(0.0)     # starts left of the hump: stops in the local minimum
right <- descend(3.7)     # starts right of the hump: reaches the global minimum
cat("start 0.0 ends at", round(tail(left, 1), 2), "R =", round(R(tail(left, 1)), 2), "\n")
cat("start 3.7 ends at", round(tail(right, 1), 2), "R =", round(R(tail(right, 1)), 2), "\n")
pdf("figs/gradient_descent.pdf", width = 6.4, height = 3.4, family = "Times")
par(mar = c(4, 4.2, 1, 1))
curve(R, -0.3, 6.6, lwd = 2, col = "grey40", xlab = expression(theta), ylab = expression(R(theta)), las = 1, ylim = c(0.9, 6.5))
points(left[1:8],  R(left[1:8]),  pch = 19, col = "firebrick", cex = 0.9)
points(right[1:8], R(right[1:8]), pch = 17, col = "navy", cex = 0.9)
text(left[1], R(left[1]), "start A", pos = 3, cex = 0.85, col = "firebrick")
text(right[1], R(right[1]), "start B", pos = 3, cex = 0.85, col = "navy")
text(tail(left, 1), R(tail(left, 1)), "local minimum", pos = 1, cex = 0.85)
text(tail(right, 1), R(tail(right, 1)), "global minimum", pos = 1, cex = 0.85)
dev.off()
