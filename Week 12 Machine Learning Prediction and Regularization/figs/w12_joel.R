## Week 12 -- Joel et al. (2020, PNAS 117(32)), Table 2: overall success rate of each relationship-specific
## construct = share of tests in which random-forest variable selection (VSURF) kept it as a predictor.
## Numbers typed from Table 2 (p. 19067). Intimacy, love, trust, passion are bold in the table: excluded from
## the main models as possibly part of relationship quality itself.
##   Rscript figs/w12_joel.R   (run from the week folder); writes figs/joel_importance.pdf
tab <- data.frame(construct = c("Perceived partner commitment", "Intimacy", "Appreciation", "Love", "Sexual satisfaction",
  "Perceived partner satisfaction", "Conflict", "Perceived partner responsiveness", "Trust", "Investment",
  "Support", "Capitalization", "Normative attachment", "Relationship length", "Passion", "Alternatives",
  "Sexual frequency", "Inclusion of other in self", "Affection", "Empathy", "Intimate partner violence",
  "Conflict strategies", "Power", "Relationship status", "Cohabiting", "Sacrifice motives", "Children"),
  success = c(85, 81, 72, 71, 71, 70, 69, 69, 68, 67, 64, 58, 58, 57, 54, 52, 47, 47, 44, 43, 43, 36, 29, 28, 28, 16, 10))
tab$excluded <- tab$construct %in% c("Intimacy", "Love", "Trust", "Passion")
o <- rev(seq_len(nrow(tab))); y <- seq_len(nrow(tab))
pdf("figs/joel_importance.pdf", width = 5.6, height = 4.4, family = "Times")
par(mar = c(3.2, 11.5, 0.6, 1), mgp = c(2.0, 0.5, 0), tcl = -0.3)
plot(tab$success[o], y, type = "n", xlim = c(0, 100), yaxt = "n", ylab = "", bty = "l", las = 1,
     xlab = "success rate (% of tests where the forest kept it)")
abline(v = seq(0, 100, 20), col = "gray92")
col <- ifelse(tab$excluded[o], "gray60", "navy")
segments(0, y, tab$success[o], y, col = col, lwd = 1.6)
points(tab$success[o], y, pch = 21, bg = ifelse(tab$excluded[o], "white", "navy"), col = col, cex = 0.95)
axis(2, at = y, labels = tab$construct[o], las = 1, cex.axis = 0.72, tick = FALSE)
dev.off()
