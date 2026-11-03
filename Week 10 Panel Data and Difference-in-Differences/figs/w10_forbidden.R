## Week 10 -- the forbidden comparison as a picture, on the staggered toy example (figs/w10_toy_staggered.R):
## untreated outcome 0; early treated from t = 2 (effects 1, 2, 3); late treated at t = 4 (effect 1); never 0.
## The 2x2 drawn: late (treated) vs early (control), before = t = 2, after = t = 4.
##   Rscript figs/w10_forbidden.R   (from the week folder; writes figs/forbidden.pdf)
t <- 1:4
early <- c(0, 1, 2, 3); late <- c(0, 0, 0, 1); never <- c(0, 0, 0, 0)
late_change  <- late[4]  - late[2]
early_change <- early[4] - early[2]
cat("t = 2 -> 4: late change", late_change, "; early change", early_change,
    "; 2x2", late_change - early_change, "\n")
blue <- rgb(1, 33, 105, maxColorValue = 255); orange <- rgb(200, 78, 0, maxColorValue = 255)
pdf("figs/forbidden.pdf", width = 7.2, height = 4.3, family = "Times")
par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.45, cex.axis = 1.35)
plot(t, early, type = "n", xlim = c(0.8, 6.4), ylim = c(-0.45, 3.75), xaxt = "n",
     xlab = "period t", ylab = "outcome")
axis(1, at = t)
rect(1.75, -1, 2.25, 4, col = "gray88", border = NA)                ## before: t = 2
rect(3.75, -1, 4.25, 4, col = "gray88", border = NA)                ## after:  t = 4
text(2, 3.6, "before", cex = 1.3, col = "gray20")
text(4, 3.6, "after", cex = 1.3, col = "gray20")
lines(t, never, type = "b", pch = 19, cex = 1.1, lwd = 2, col = "gray55", lty = 2)
lines(t, late,  type = "b", pch = 19, cex = 1.3, lwd = 3, col = orange)
lines(t, early, type = "b", pch = 19, cex = 1.3, lwd = 3, col = blue)
text(1.05, 1.25, "early:\ntreated\nfrom t = 2", cex = 1.15, col = blue)
text(4.35, early[4], "early (control):\n1 -> 3, change +2", pos = 4, cex = 1.3, col = blue)
text(4.35, 1.05, "late (treated):\n0 -> 1, change +1", pos = 4, cex = 1.3, col = orange)
text(4.35, -0.12, "never: 0", pos = 4, cex = 1.3, col = "gray45")
dev.off()
