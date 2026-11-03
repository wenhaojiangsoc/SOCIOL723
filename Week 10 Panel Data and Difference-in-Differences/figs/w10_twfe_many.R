## Week 10 -- TWFE with many periods and one adoption date (no staggering), then the event study.
## Toy: 4 units (2 treated, 2 control), periods 1-6, treated units switch on at t = 4. No noise.
## Untreated outcome = unit level + common growth 0.5 per period.
##   Rscript figs/w10_twfe_many.R   (from the week folder; writes figs/twfe_many_es.pdf)
suppressMessages(library(fixest))
base <- expand.grid(id = 1:4, t = 1:6)
base$treated <- as.integer(base$id <= 2)
base$D <- base$treated * (base$t >= 4)
base$k <- base$t - 4                                   ## periods since adoption (treated units)
y0 <- c(1, 2, 0, 0.5)[base$id] + 0.5 * (base$t - 1)    ## untreated outcome

cases <- list(
  growing = y0 + base$D * c(0, 0, 0, 1, 2, 3)[base$t],               ## effects 1, 2, 3
  fading  = y0 + base$D * c(0, 0, 0, 3, 2, 1)[base$t],               ## effects 3, 2, 1
  trend   = y0 + base$treated * 0.4 * (base$t - 3))                  ## no effect; treated rise 0.4 faster

twfe <- function(y, d = base) coef(feols(y ~ D | id + t, data = transform(d, y = y)))[["D"]]
es   <- function(y) coef(feols(y ~ i(k, treated, ref = -1) | id + t, data = transform(base, y = y)))

cat("TWFE, one D:\n")
print(round(c(sapply(cases, twfe),
              growing_to_t5 = twfe(cases$growing[base$t <= 5], base[base$t <= 5, ])), 2))
cat("\nevent study tau_k (k = -3, -2, 0, 1, 2; k = -1 omitted):\n")
print(round(sapply(cases, es), 2))

## figure: event-study coefficients for the three cases
pdf("figs/twfe_many_es.pdf", width = 7.5, height = 2.9, family = "Times")
par(mfrow = c(1, 3), mar = c(3.6, 4, 2.4, 0.5), cex.lab = 1.5, cex.axis = 1.35, cex.main = 1.6)
ttl <- c(growing = "growing", fading = "fading", trend = "trend, no effect")
for (nm in names(cases)) {
  b <- es(cases[[nm]]); kk <- c(-3, -2, -1, 0, 1, 2); v <- c(b[1:2], 0, b[3:5])
  plot(kk, v, type = "n", ylim = c(-1, 3.3), xlab = "k",
       ylab = expression(hat(tau)[k]), main = paste0(ttl[nm], ": TWFE ", round(twfe(cases[[nm]]), 1)))
  abline(h = 0, col = "gray60"); abline(v = -0.5, lty = 3, col = "gray40")
  lines(kk, v, type = "b", pch = 19, cex = 1.4, lwd = 2.5, col = "navy")
}
dev.off()

## ---- continuous dose (the 'Doses' hypothetical): two periods, so TWFE = slope of the change on the dose
## changes: d = 0 -> -1.0, d = $1 -> -0.5, d = $2 -> +0.4 (one state per group, or any equal number)
dose_twfe <- function(n) {             ## n = number of states at d = 0, 1, 2
  d <- rep(0:2, n); dy <- c(-1, -0.5, 0.4)[d + 1]
  b <- coef(lm(dy ~ d))[["d"]]
  ## OLS slope = sum over steps of w_s * (step in mean change); w_s prop. to Pr(D >= s) * (E[D | D >= s] - E[D])
  w <- sapply(1:2, function(s) mean(d >= s) * (mean(d[d >= s]) - mean(d)))
  c(twfe = b, w1 = w[1] / sum(w), w2 = w[2] / sum(w))
}
cat("\ndoses, equal groups (1,1,1):\n"); print(round(dose_twfe(c(1, 1, 1)), 3))
cat("steps: first dollar 0.5, second dollar 0.9\n")
cat("doses, most states at $2 (1,1,4):\n"); print(round(dose_twfe(c(1, 1, 4)), 3))
cat("doses, most states at $1 (1,4,1):\n"); print(round(dose_twfe(c(1, 4, 1)), 3))
