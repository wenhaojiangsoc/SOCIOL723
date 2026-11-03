## Week 10 -- toy numbers: the FE weights example, and the staggered toy used on the forbidden-comparison, Callaway-Sant'Anna, and imputation slides.
## Three units, periods 1-4, untreated outcome 0 everywhere: never treated; early (treated from t = 2, effects 1, 2, 3);
## late (treated at t = 4, effect 1). Also the random-effects theta for groups of 1, 4, 20 (sigma 1.5, tau 2).
##   Rscript figs/w10_toy_staggered.R > figs/w10_toy_staggered.out
d <- data.frame(unit = rep(c("never", "early", "late"), each = 4), t = rep(1:4, 3),
                Y = c(0, 0, 0, 0,  0, 1, 2, 3,  0, 0, 0, 1),
                D = c(0, 0, 0, 0,  0, 1, 1, 1,  0, 0, 0, 1))
## TWFE on all 12 cells, and without the never-treated unit
cat("TWFE, all three units:", coef(lm(Y ~ D + factor(unit) + factor(t), d))[["D"]], "\n")
cat("TWFE, early and late only:", coef(lm(Y ~ D + factor(unit) + factor(t), d[d$unit != "never", ]))[["D"]], "\n")
## imputation: fit unit + period effects on untreated cells, impute Y(0) in treated cells, average Y - Y(0)
f0 <- lm(Y ~ factor(unit) + factor(t), d[d$D == 0, ])
d$Y0 <- predict(f0, d)
cat("untreated fit, max |residual|:", max(abs(resid(f0))), "\n")
print(d[d$D == 1, ])
cat("imputation estimate (equal weights on treated cells):", mean(d$Y[d$D == 1] - d$Y0[d$D == 1]), "\n")
## Callaway-Sant'Anna ATT(4, 4): late's change from t = 3 to 4 minus the never-treated change
y <- function(u, tt) d$Y[d$unit == u & d$t == tt]
cat("ATT(4,4) vs never treated:", (y("late", 4) - y("late", 3)) - (y("never", 4) - y("never", 3)), "\n")
## random-effects theta = 1 - sqrt(sigma^2 / (sigma^2 + n tau^2))
th <- function(n) 1 - sqrt(2.25 / (2.25 + n * 4))
cat("theta for n = 1, 4, 20:", round(sapply(c(1, 4, 20), th), 3), "\n")
## fixed-effects weights (What Fixed Effects Averages): man i married m of T years has weight sum_t Dtilde^2 = T p (1 - p)
S <- function(T, m) { p <- m / T; m * (1 - p)^2 + (T - m) * p^2 }
cat("sum Dtilde^2 for T = 8 and m = 1, 2, 4:", S(8, 1), S(8, 2), S(8, 4), "\n")
## two-man example: A married 4 of 8 years with b = 0.10, B married 1 of 8 years with b = 0.40
cat("FE = weighted mean of b:", round((S(8, 4) * 0.10 + S(8, 1) * 0.40) / (S(8, 4) + S(8, 1)), 3),
    "; plain mean:", (0.10 + 0.40) / 2, "\n")
## Goodman-Bacon decomposition of the same TWFE estimate (1.10), comparison by comparison
suppressMessages(library(bacondecomp))
toy <- data.frame(id = rep(1:3, each = 4), t = rep(1:4, 3), Y = d$Y, post = d$D)
bd <- bacon(Y ~ post, data = toy, id_var = "id", time_var = "t", quietly = TRUE)
print(bd)
cat("weighted sum of the 2x2s:", sum(bd$weight * bd$estimate), "\n")
## the TWFE weight on each treated cell: double-demeaned D over its sum on treated cells
d$Dtil <- d$D - ave(d$D, d$unit) - ave(d$D, d$t) + mean(d$D)
d$tau  <- d$Y - d$Y0
tr <- d[d$D == 1, ]
tr$w <- tr$Dtil / sum(tr$Dtil)
print(tr[, c("unit", "t", "tau", "Dtil", "w")])
cat("sum of w * tau:", sum(tr$w * tr$tau), "; true average effect:", mean(tr$tau), "\n")

## ---- Goodman-Bacon weights by hand, without Dbar notation:
## weight of a 2x2 is proportional to (share of the 4 periods it uses)^2 x p(1 - p),
## p = share of the 2x2's own periods in which its treated group is treated (equal group sizes cancel)
gb <- data.frame(pair = c("early vs never", "late vs never", "early vs late", "late vs early"),
                 periods = c(4, 4, 3, 3), treated_periods = c(3, 1, 2, 1))
gb$p <- gb$treated_periods / gb$periods
gb$var_D <- gb$p * (1 - gb$p)
gb$raw <- (gb$periods / 4)^2 * gb$var_D
gb$weight <- gb$raw / sum(gb$raw)
print(transform(gb, p = round(p, 3), var_D = round(var_D, 3), raw = round(raw, 4), weight = round(weight, 3)))

## ---- the TWFE event study inherits the forbidden comparison (slide 'The Event Study Has the Same Problem')
## same toy, but late's effect at t = 4 is 3 instead of 1, so cohorts differ at k = 0 (early 1, late 3)
suppressMessages(library(fixest))
es <- d[, c("unit", "t", "Y")]
es$G <- c(never = 10000, early = 2, late = 4)[es$unit]          ## never treated: G outside the panel
es$k <- ifelse(es$unit == "never", -1000, es$t - es$G)            ## event time; -1000 = never treated
## first the original toy (slide 'Would an Event Study Fix It?'): effects depend only on k (k = 0: early 1, late 1)
cat("\noriginal toy, TWFE event study (truth k = -3, -2, 0, 1, 2: 0, 0, 1, 2, 3):\n")
print(round(coef(feols(Y ~ i(k, ref = c(-1, -1000)) | unit + t, es)), 2))
es$Y[es$unit == "late" & es$t == 4] <- 3
cat("\ntruth by event time: k = -3, -2: 0; k = 0: (1 + 3)/2 = 2; k = 1: 2; k = 2: 3\n")
cat("TWFE event study (ref k = -1):\n")
m_es <- feols(Y ~ i(k, ref = c(-1, -1000)) | unit + t, es)
print(round(coef(m_es), 2))
## why the leads move: the shared tau_0 (2) is short of late's jump (3); OLS raises late's level, the leads cancel it
cat("unit effects:", round(fixef(m_es)$unit, 2), " year effects:", round(fixef(m_es)$t, 2), "\n")
## clean blocks instead: each cohort vs never treated, base period g - 1, then average cohorts at each k
yy <- function(u, tt) es$Y[es$unit == u & es$t == tt]
blk <- function(u, tt, base) (yy(u, tt) - yy(u, base)) - (yy("never", tt) - yy("never", base))
cat("clean blocks: early k = 0, 1, 2:", blk("early", 2, 1), blk("early", 3, 1), blk("early", 4, 1),
    "; late k = -3, -2, 0:", blk("late", 1, 3), blk("late", 2, 3), blk("late", 4, 3),
    "; average at k = 0:", (blk("early", 2, 1) + blk("late", 4, 3)) / 2, "\n")
