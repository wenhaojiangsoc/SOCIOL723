## Week 11 -- DiD and synthetic control as two 2x2s on Prop 99: same California, two controls.
## Control 1 = average of the 38 donors (given); control 2 = synthetic California (built: ADH predictors, as in w11_prop99.R).
##   Rscript figs/w11_2x2.R > figs/w11_2x2.out   (run from the week folder); writes figs/prop99_2x2.pdf
suppressMessages({library(tidysynth); library(dplyr)})
data(smoking, package = "tidysynth")
sm <- as.data.frame(smoking)
Y <- reshape(sm[, c("state", "year", "cigsale")], idvar = "state", timevar = "year", direction = "wide")
rownames(Y) <- Y$state; Y <- as.matrix(Y[, -1]); colnames(Y) <- 1970:2000
pre <- as.character(1970:1988); post <- as.character(1989:2000)
y1 <- Y["California", ]; dbar <- colMeans(Y[rownames(Y) != "California", ])

## ---- 1. build the synthetic control (ADH 2010 predictors; same call as w11_prop99.R section 1) ----
sc <- sm %>% synthetic_control(outcome = cigsale, unit = state, time = year, i_unit = "California",
                               i_time = 1988, generate_placebos = FALSE) %>%
  generate_predictor(time_window = 1980:1988, ln_income = mean(lnincome, na.rm = TRUE),
                     ret_price = mean(retprice, na.rm = TRUE), youth = mean(age15to24, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1984:1988, beer_sales = mean(beer, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1975, cigsale_1975 = cigsale) %>%
  generate_predictor(time_window = 1980, cigsale_1980 = cigsale) %>%
  generate_predictor(time_window = 1988, cigsale_1988 = cigsale) %>%
  generate_weights(optimization_window = 1970:1988, margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()
sy <- grab_synthetic_control(sc); synth <- sy$synth_y; names(synth) <- sy$time_unit

## ---- 2. the two 2x2 tables: means before (1970-88) and after (1989-2000) ----
tab <- rbind(California = c(mean(y1[pre]), mean(y1[post])),
             `donor average` = c(mean(dbar[pre]), mean(dbar[post])),
             `synthetic California` = c(mean(synth[pre]), mean(synth[post])))
colnames(tab) <- c("before", "after"); tab <- cbind(tab, change = tab[, 2] - tab[, 1])
cat("## 2. means\n"); print(round(tab, 1))
for (ctl in c("donor average", "synthetic California")) {
  g <- tab["California", 1:2] - tab[ctl, 1:2]
  cat(sprintf("\n%s: gap before %.1f, gap after %.1f, 2x2 = after gap - before gap = %.1f\n", ctl, g[1], g[2], g[2] - g[1]))
  cat(sprintf("   = CA change %.1f - control change %.1f\n", tab["California", "change"], tab[ctl, "change"]))
}
gd <- y1 - dbar; gs <- y1 - synth
cat("\ngap CA - donor average: 1970", round(gd["1970"], 1), " 1980", round(gd["1980"], 1), " 1988", round(gd["1988"], 1), "\n")
cat("gap CA - synthetic: range 1970-88", round(range(gs[pre]), 1), "; pre RMSPE", round(sqrt(mean(gs[pre]^2)), 2), "\n")
cat("SC's usual estimate (average gap after):", round(mean(gs[post]), 1), "\n")

## ---- 3. figure: the gap, year by year, with its before and after means ----
pdf("figs/prop99_2x2.pdf", width = 8.5, height = 2.9, family = "Times")
par(mfrow = c(1, 2), mar = c(3.6, 4.2, 1.8, 1))
panel <- function(g, main) {
  yrs <- 1970:2000
  plot(yrs, g, type = "n", ylim = c(-50, 10), xlab = "year", ylab = "California minus control", main = main, font.main = 1, cex.main = 1)
  abline(h = 0, col = "gray80"); abline(v = 1988.5, lty = 2, col = "gray50")
  lines(yrs, g, lwd = 1.5, col = "gray45"); points(yrs, g, pch = 16, cex = 0.45, col = "gray45")
  b <- mean(g[pre]); a <- mean(g[post])
  segments(1970, b, 1988.5, b, lwd = 3, col = "navy"); segments(1988.5, a, 2000, a, lwd = 3, col = "firebrick")
  segments(1988.5, b, 2000, b, lwd = 2, lty = 3, col = "navy")
  arrows(1999, b, 1999, a, length = 0.06, code = 2, lwd = 1.5)
  text(1970.5, b - 5, sprintf("before mean: %.1f", b), col = "navy", cex = 0.85, adj = 0)
  text(1988, a, sprintf("after mean: %.1f", a), col = "firebrick", cex = 0.85, adj = 1)
  text(1998.6, (a + b) / 2, sprintf("2x2: %.1f", a - b), adj = 1, cex = 0.85) }
panel(gd, "Control = average of 38 donors (given)")
panel(gs, "Control = synthetic California (built)")
dev.off()
