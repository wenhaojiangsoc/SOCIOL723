## Week 10 -- Callaway-Sant'Anna on the staggered toy (figs/w10_toy_staggered.R): every ATT(g,t) by hand,
## and a picture of two blocks. Cohort g = the units that start treatment in period g.
## Toy: untreated outcome 0; cohort 2 ("early") effects 1, 2, 3; cohort 4 ("late") effect 1; one never-treated unit.
##   Rscript figs/w10_cs_blocks.R   (from the week folder; writes figs/cs_blocks.pdf)
Y <- rbind(early = c(0, 1, 2, 3), late = c(0, 0, 0, 1), never = c(0, 0, 0, 0))
G <- c(early = 2, late = 4, never = Inf)
att <- function(g, t) {                       # baseline g - 1; controls = not yet treated at t (incl. never)
  tr <- names(G)[G == g]; ct <- names(G)[G > t]
  mean(Y[tr, t] - Y[tr, g - 1]) - mean(Y[ct, t] - Y[ct, g - 1])
}
blocks <- data.frame(g = c(2, 2, 2, 4), t = c(2, 3, 4, 4))
blocks$controls <- sapply(blocks$t, function(t) paste(names(G)[G > t], collapse = "+"))
blocks$att <- mapply(att, blocks$g, blocks$t)
print(blocks); cat("simple average:", mean(blocks$att), "\n")

blue <- rgb(1, 33, 105, maxColorValue = 255); orange <- rgb(200, 78, 0, maxColorValue = 255)
draw <- function(g, t, title) {
  plot(NA, xlim = c(0.3, 4.6), ylim = c(0.4, 3.9), axes = FALSE, xlab = "", ylab = "", main = title)
  for (i in 1:3) for (s in 1:4) {
    treated <- s >= G[i]
    rect(s - 0.45, 4 - i - 0.4, s + 0.45, 4 - i + 0.4, col = if (treated) "#F6DCCB" else "gray93", border = "white")
    text(s, 4 - i, Y[i, s], cex = 1.4)
  }
  ct <- which(G > t); tr <- which(G == g)
  for (s in c(g - 1, t)) {
    for (i in tr) rect(s - 0.45, 4 - i - 0.4, s + 0.45, 4 - i + 0.4, border = orange, lwd = 3.5)
    for (i in ct) rect(s - 0.45, 4 - i - 0.4, s + 0.45, 4 - i + 0.4, border = blue, lwd = 3.5)
  }
  axis(3, at = 1:4, labels = paste0("t = ", 1:4), tick = FALSE, line = -1.2, cex.axis = 1.2)
  axis(2, at = 3:1, labels = c("cohort 2\n(early)", "cohort 4\n(late)", "never"), las = 1, tick = FALSE,
       cex.axis = 1.15, line = -1)
}
pdf("figs/cs_blocks.pdf", width = 9, height = 2.9, family = "Times")
par(mfrow = c(1, 2), mar = c(0.5, 5.5, 3.2, 0.5), cex.main = 1.35)
draw(2, 3, "ATT(2, 3) = (2 - 0) - (0 - 0) = 2")
draw(4, 4, "ATT(4, 4) = (1 - 0) - (0 - 0) = 1")
dev.off()

## ---- covariates inside one block (hypothetical): X = South. True effect 1 for every cohort unit.
## Untreated change: South +4, non-South +2. Cohort g: 3 South, 1 non-South. Controls: 1 South, 3 non-South.
blk <- data.frame(cohort = c(1, 1, 1, 1, 0, 0, 0, 0), south = c(1, 1, 1, 0, 1, 0, 0, 0))
blk$dy <- ifelse(blk$south == 1, 4, 2) + blk$cohort * 1
unadj <- mean(blk$dy[blk$cohort == 1]) - mean(blk$dy[blk$cohort == 0])
m0 <- tapply(blk$dy[blk$cohort == 0], blk$south[blk$cohort == 0], mean)             # controls' change by X
or <- mean(blk$dy[blk$cohort == 1]) - mean(m0[as.character(blk$south[blk$cohort == 1])])
e <- ave(blk$cohort, blk$south)                                                     # Pr(cohort | X)
w <- (e / (1 - e))[blk$cohort == 0]
ipw <- mean(blk$dy[blk$cohort == 1]) - weighted.mean(blk$dy[blk$cohort == 0], w)
cat("\ncovariate block: cohort change", mean(blk$dy[blk$cohort == 1]), "; controls' change", mean(blk$dy[blk$cohort == 0]),
    "\n unadjusted", unadj, "; outcome regression", or, "; IPW", ipw,
    "; control weights (South, non-South):", unique(round(w, 3)), "\n")
