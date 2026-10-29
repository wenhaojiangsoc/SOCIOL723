## Week 10 -- why FE fails under selection on growth (hypothetical, no noise).
## Four men, 1980-87, TRUE marriage effect = 0. Two men on steep wage paths (0.08 a year) marry (1983, 1984);
## two men on flat paths (0.02 a year) never marry. Two-way FE reads the steep men's extra growth after
## marriage as a marriage effect; own slopes (FEIS) recover 0.
##   Rscript figs/w10_slopes.R > figs/w10_slopes.out    (from the week folder; writes figs/pan_slopes.pdf)
suppressMessages(library(fixest)); setFixest_notes(FALSE)
yrs <- 1980:1987
men <- data.frame(id = 1:4, level = c(1.9, 1.6, 1.5, 1.1), growth = c(0.08, 0.08, 0.02, 0.02),
                  marry = c(1983, 1984, NA, NA),
                  lab = c("steep, marries in 1983", "steep, marries in 1984", "flat, never married", "flat, never married"))
d <- merge(men, data.frame(year = yrs))
d <- d[order(d$id, d$year), ]
d$D <- as.integer(!is.na(d$marry) & d$year >= d$marry)
d$y <- d$level + d$growth * (d$year - 1980)              ## no marriage effect, no noise
tw <- feols(y ~ D | id + year, d)
fs <- feols(y ~ D | id[year], d)                           ## own intercept and own slope in year
cat("true effect 0; two-way FE:", round(coef(tw)[["D"]], 3), "; own slopes (FEIS):", round(coef(fs)[["D"]], 3), "\n")
fx <- fixef(tw)
d$fit  <- fx$id[as.character(d$id)] + fx$year[as.character(d$year)] + coef(tw)[["D"]] * d$D
d$base <- fx$id[as.character(d$id)] + fx$year[as.character(d$year)]
cols <- c("darkorange", "firebrick", "navy", "steelblue")
pdf("figs/pan_slopes.pdf", width = 6.4, height = 4.4, family = "Times")
par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.2, cex.axis = 1.1)
plot(d$year, d$y, pch = ifelse(d$D == 1, 19, 1), cex = 0.95, col = cols[d$id], ylim = c(1.0, 3.0),
     xaxt = "n", xlab = "year", ylab = "log hourly wage")
axis(1, at = yrs)
for (i in 1:4) { r <- d[d$id == i, ]
  lines(r$year, r$fit, col = cols[i], lwd = 2.5)
  if (any(r$D == 1)) { lines(r$year, r$base, col = cols[i], lwd = 1.5, lty = 2)
    lines(r$year, r$y, col = "gray35", lwd = 1, lty = 3) } }   ## his true path: straight, no jump
legend("topleft", c(men$lab, "solid: two-way FE fit", "dashed: FE's 'same man, unmarried'", "dotted: his true path"),
       col = c(cols, "gray30", "gray30", "gray35"), pch = c(19, 19, 1, 1, NA, NA, NA),
       lwd = c(NA, NA, NA, NA, 2.5, 1.5, 1), lty = c(NA, NA, NA, NA, 1, 2, 3), bty = "n", cex = 0.85, ncol = 2)
dev.off()
