## Week 14 -- marginal structural model on a two-period simulation with round numbers.
## Every number on the MSM frames comes from here.   Rscript figs/w14_msm.R > figs/w14_msm.out
## D1, D2 = poor neighborhood in childhood, adolescence; L = low family income in between;
## U = an unmeasured family disadvantage (0/1); Y = test score.
## Truth by construction: D1 lowers Y by 2 directly and by 5 x 0.2 = 1 through income: -3. D2: -2.
sim <- function(n, d1 = NULL, d2 = NULL) {
  U  <- rbinom(n, 1, 0.5)
  D1 <- if (is.null(d1)) rbinom(n, 1, 0.5) else rep(d1, n)
  L  <- rbinom(n, 1, 0.1 + 0.2 * D1 + 0.4 * U)              # D1 and U both lower income
  D2 <- if (is.null(d2)) rbinom(n, 1, 0.2 + 0.1 * D1 + 0.6 * L) else rep(d2, n)
  Y  <- 50 - 2 * D1 - 2 * D2 - 5 * L - 3 * U + rnorm(n, 0, 3)
  data.frame(U, D1, L, D2, Y) }
wts <- function(d) { den <- fitted(glm(D2 ~ D1 * L, binomial, d)); num <- fitted(glm(D2 ~ D1, binomial, d))
  data.frame(w  = ifelse(d$D2 == 1, 1 / den, 1 / (1 - den)),
             sw = ifelse(d$D2 == 1, num / den, (1 - num) / (1 - den))) }

## ---- truth: set (d1, d2) for everyone ------------------------------------------------------
set.seed(723)
mu <- sapply(list(c(0,0), c(1,0), c(0,1), c(1,1)), function(a) mean(sim(2e6, a[1], a[2])$Y))
cat("E[Y(d1,d2)] for 00, 10, 01, 11:", round(mu, 2), " (by construction 47, 44, 45, 42)\n")

## ---- a very large sample: what each method converges to ----------------------------------
set.seed(723); big <- sim(2e6); big <- cbind(big, wts(big))
tab <- rbind("truth" = c(-3, -2),
             "regression: D1, D2" = coef(lm(Y ~ D1 + D2, big))[2:3],
             "regression: D1, D2, L" = coef(lm(Y ~ D1 + D2 + L, big))[2:3],
             "weighted regression: D1, D2" = coef(lm(Y ~ D1 + D2, big, weights = w))[2:3],
             "same, stabilized weights" = coef(lm(Y ~ D1 + D2, big, weights = sw))[2:3])
cat("\n## n = 2,000,000: coefficient of D1, D2\n"); print(round(tab, 2))
cat(sprintf("share low income by D1 = 1 / 0: %.2f / %.2f\n", mean(big$L[big$D1 == 1]), mean(big$L[big$D1 == 0])))
cat("share in a poor neighborhood in period 2, by (D1, L):\n"); print(round(tapply(big$D2, big[c("D1", "L")], mean), 2))
cat("share with U = 1 by D1, everyone:", round(tapply(big$U, big$D1, mean), 2), "\n")
cat("share with U = 1 by D1 among L = 1:", round(with(big[big$L == 1, ], tapply(U, D1, mean)), 2),
    "; among L = 0:", round(with(big[big$L == 0, ], tapply(U, D1, mean)), 2), "\n")
cat(sprintf("does income predict D2 given D1? raw slope %.2f; weighted %.2f; stabilized %.2f\n",
            coef(lm(D2 ~ L + D1, big))["L"], coef(lm(D2 ~ L + D1, big, weights = w))["L"],
            coef(lm(D2 ~ L + D1, big, weights = sw))["L"]))
cat("Pr(D2 = 1 | D1):", round(tapply(big$D2, big$D1, mean), 2), "\n")
cat("weights 1/p: mean", round(mean(big$w), 2), "range", round(range(big$w), 2),
    "; stabilized: mean", round(mean(big$sw), 2), "range", round(range(big$sw), 2), "\n")

## ---- one sample of 5,000 (the lab's sample) -----------------------------------------------
set.seed(723); d <- sim(5000); d <- cbind(d, wts(d))
hc <- function(m) sqrt(diag(sandwich::vcovHC(m, type = "HC1")))[2:3]
m1 <- lm(Y ~ D1 + D2, d); m2 <- lm(Y ~ D1 + D2 + L, d)
m3 <- lm(Y ~ D1 + D2, d, weights = w); m4 <- lm(Y ~ D1 + D2, d, weights = sw)
cat("\n## n = 5,000: estimates (robust s.e.)\n")
print(round(rbind(noL = c(coef(m1)[2:3], hc(m1)), withL = c(coef(m2)[2:3], hc(m2)),
                  w = c(coef(m3)[2:3], hc(m3)), sw = c(coef(m4)[2:3], hc(m4))), 2))

## ---- five periods: why stabilize ------------------------------------------------------------
## each period: income responds to last period's neighborhood; the neighborhood is sticky
## (0.6 on last period's) and responds to income (0.2).
## every period of exposure costs 2 directly and 1 through next period's income: MSM slope -3.
simK <- function(n, K = 5) {
  U <- rbinom(n, 1, 0.5); Dp <- rep(0, n); w <- sw <- rep(1, n); cumD <- sumL <- rep(0, n)
  for (k in 1:K) {
    L <- rbinom(n, 1, 0.1 + 0.2 * Dp + 0.4 * U); sumL <- sumL + L
    D <- rbinom(n, 1, 0.1 + 0.6 * Dp + 0.2 * L)
    den <- fitted(glm(D ~ Dp * L, binomial)); num <- fitted(glm(D ~ Dp, binomial))
    w <- w * ifelse(D == 1, 1 / den, 1 / (1 - den)); sw <- sw * ifelse(D == 1, num / den, (1 - num) / (1 - den))
    cumD <- cumD + D; Dp <- D }
  L <- rbinom(n, 1, 0.1 + 0.2 * Dp + 0.4 * U); sumL <- sumL + L   # income after the last period
  Y <- 50 - 2 * cumD - 5 * sumL - 3 * U + rnorm(n, 0, 3)
  data.frame(Y, cumD, sumL, w, sw) }
set.seed(7231); R <- 500; res <- matrix(NA, R, 6)
for (r in 1:R) { x <- simK(2000)
  res[r, ] <- c(coef(lm(Y ~ cumD, x))[2], coef(lm(Y ~ cumD, x, weights = w))[2], coef(lm(Y ~ cumD, x, weights = sw))[2],
                max(x$w), max(x$sw), mean(x$sw)) }
cat("\n## five periods, n = 2,000, 500 samples; true slope per period of exposure: -3\n")
colnames(res) <- c("unweighted", "1/p weights", "stabilized", "max w", "max sw", "mean sw")
print(round(rbind(mean = colMeans(res), sd = apply(res, 2, sd)), 2))
x <- simK(2000); cat("one sample: 1/p weights mean", round(mean(x$w), 1), "max", round(max(x$w)),
  "; stabilized mean", round(mean(x$sw), 2), "max", round(max(x$sw), 1), "\n")
cat("share of the total weight held by the top 1% of children: 1/p", round(sum(sort(x$w, TRUE)[1:20]) / sum(x$w), 2),
    "; stabilized", round(sum(sort(x$sw, TRUE)[1:20]) / sum(x$sw), 2), "\n")
