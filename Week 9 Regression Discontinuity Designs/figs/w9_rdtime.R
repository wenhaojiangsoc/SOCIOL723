## Week 9 -- RD in time: a conceptual sharp RD with calendar days as the running variable.
##   Rscript figs/w9_rdtime.R > figs/w9_rdtime.out ; writes figs/rdtime_concept.pdf
## Hypothetical daily outcome: slow trend, jump of 3 on the event day t*.
set.seed(9)
day <- -90:90; h <- 45
y <- 20 + 0.02 * day + 3 * (day >= 0) + rnorm(length(day), sd = 1.2)
d <- data.frame(y, day, D = as.integer(day >= 0)); w <- subset(d, abs(day) <= h)
fit <- lm(y ~ D * day, data = w, weights = 1 - abs(day) / h)
b <- coef(fit)
cat("h =", h, "days; estimated jump", round(b[["D"]], 2), "(truth 3)\n")
pdf("figs/rdtime_concept.pdf", width = 8, height = 2.6, family = "Times")
par(mar = c(3.6, 4.4, 0.4, 0.6), mgp = c(2.3, 0.8, 0), cex.lab = 1.2, cex.axis = 1.1)
cl <- adjustcolor(ifelse(day < 0, "navy", "firebrick"), 0.45)
plot(day, y, type = "n", ylim = c(15, 29),
     xlab = expression("calendar day minus event day " * t^"*"), ylab = "daily outcome")
rect(-h, par("usr")[3], h, par("usr")[4], col = adjustcolor("gray80", 0.35), border = NA)
points(day, y, pch = 19, cex = 0.6, col = cl)
abline(v = 0, lty = 2, col = "gray50")
gl <- seq(-h, 0, length.out = 50); gr <- seq(0, h, length.out = 50)
lines(gl, b[1] + b[3] * gl, lwd = 3, col = "navy")
lines(gr, b[1] + b[2] + (b[3] + b[4]) * gr, lwd = 3, col = "firebrick")
segments(0, b[1], 0, b[1] + b[2], lwd = 3, col = "darkorange")
arrows(14, 17.4, 1, b[1] + b[2] / 2, length = 0.08, col = "darkorange", lwd = 1.5)
text(14, 17.4, expression(tau * " = jump on day " * t^"*"), pos = 4, col = "darkorange", cex = 1.15)
text(-45, 28.2, "before: everyone untreated", cex = 1.05)
text(45, 28.2, "after: everyone treated", cex = 1.05)
text(h, 15.6, "h", pos = 2, cex = 1.1); text(-h, 15.6, "-h", pos = 4, cex = 1.1)
dev.off()
