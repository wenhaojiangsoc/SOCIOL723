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
