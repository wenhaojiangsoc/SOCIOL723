## Week 13 -- AIPW with forests predicting, on Week 6's g-computation simulation.
## Same data as Week 6's figs/w6_fuzzy.R (seed, n, curves). Every number on the frame
## "Week 6's Picture, With Forests Predicting" comes from here.
##   Rscript figs/w13_aipw_forest.R > figs/w13_aipw_forest.out
suppressMessages(library(ranger))
set.seed(723)
n <- 4000
X <- runif(n)
e <- pnorm((X - 0.6) / 0.12)            # steep propensity: no controls above about 0.9
D <- rbinom(n, 1, e)
m0 <- function(x) 3 * x^2               # curved control mean
tauf <- function(x) 2 * x               # effect grows with x: ATE = 1
Y <- m0(X) + tauf(X) * D + rnorm(n, 0, 0.3)
dat <- data.frame(X, D, Y)
ATE <- mean(tauf(X))

## ---- Week 6: one line per arm, fitted on everyone, extended ------------------
f1_lin <- lm(Y ~ X, dat[D == 1, ]); f0_lin <- lm(Y ~ X, dat[D == 0, ])
g_lin <- mean(predict(f1_lin, dat) - predict(f0_lin, dat))

## ---- Week 13: forests predict, 5-fold train-test split -----------------------
## each person's m1-hat, m0-hat, e-hat come from forests fitted on the other four folds
set.seed(723)
fold <- sample(rep(1:5, length.out = n))
m1 <- m0h <- eh <- numeric(n)
grid <- data.frame(X = seq(0, 1, 0.005)); g1 <- g0 <- 0   # for the picture: the five forests' average curve
for (k in 1:5) {
  tr <- dat[fold != k, ]; te <- dat[fold == k, ]
  r1 <- ranger(Y ~ X, tr[tr$D == 1, ], num.trees = 500, min.node.size = 40, seed = 723)
  r0 <- ranger(Y ~ X, tr[tr$D == 0, ], num.trees = 500, min.node.size = 40, seed = 723)
  re <- ranger(D ~ X, tr,              num.trees = 500, min.node.size = 40, seed = 723)
  m1[fold == k]  <- predict(r1, te)$predictions
  m0h[fold == k] <- predict(r0, te)$predictions
  eh[fold == k]  <- predict(re, te)$predictions
  g1 <- g1 + predict(r1, grid)$predictions / 5; g0 <- g0 + predict(r0, grid)$predictions / 5
}
g_rf <- mean(m1 - m0h)                                   # g-computation with forests
ehc <- pmin(pmax(eh, 0.01), 0.99)                        # keep e-hat off 0 and 1
aipw <- m1 - m0h + D * (Y - m1) / ehc - (1 - D) * (Y - m0h) / (1 - ehc)
cat(sprintf("true ATE in this sample: %.3f\n", ATE))
cat(sprintf("g-computation, linear per arm (Week 6): %.3f\n", g_lin))
cat(sprintf("g-computation, forests, held out:       %.3f\n", g_rf))
cat(sprintf("AIPW, forests, held out:                %.3f (se %.3f)\n", mean(aipw), sd(aipw) / sqrt(n)))
cat(sprintf("e-hat range before clipping: [%.3f, %.3f]; share clipped: %.3f\n", min(eh), max(eh), mean(eh < 0.01 | eh > 0.99)))

## where both groups are present (x in [0.3, 0.9]) the forests need no extrapolation
ov <- X >= 0.3 & X <= 0.9
cat(sprintf("x in [0.3, 0.9], %d people: true mean effect %.3f | linear g-comp %.3f | forest g-comp %.3f | AIPW %.3f (se %.3f)\n",
            sum(ov), mean(tauf(X[ov])), mean((predict(f1_lin, dat) - predict(f0_lin, dat))[ov]),
            mean((m1 - m0h)[ov]), mean(aipw[ov]), sd(aipw[ov]) / sqrt(sum(ov))))
cat(sprintf("treated with x < 0.3: %d\n", sum(D == 1 & X < 0.3)))

## m0-hat where there are no controls, and where there are plenty
at <- function(x0, v) mean(v[abs(X - x0) < 0.02])
cat(sprintf("controls with x > 0.9: %d\n", sum(D == 0 & X > 0.9)))
for (x0 in c(0.5, 0.95))
  cat(sprintf("x = %.2f: true m0 %.2f | linear %.2f | forest %.2f ; true tau %.2f | forest m1 - m0 %.2f | mean AIPW %.2f\n",
              x0, m0(x0), predict(f0_lin, data.frame(X = x0)), at(x0, m0h), tauf(x0), at(x0, m1 - m0h), at(x0, aipw)))
## one person for the picture: treated, x near 0.5
i <- which(D == 1 & abs(X - 0.5) < 0.005)[1]
cat(sprintf("person %d: x %.3f, Y %.2f, m1-hat %.2f, m0-hat %.2f, e-hat %.2f, AIPW %.2f (true effect %.2f)\n",
            i, X[i], Y[i], m1[i], m0h[i], eh[i], aipw[i], tauf(X[i])))

## ---- figure: Week 6's picture, with the forests' held-out predictions --------
pdf("figs/aipw_forest.pdf", width = 3.6, height = 2.9)
par(mar = c(3.0, 3.0, 0.5, 0.5), mgp = c(1.8, 0.6, 0), cex = 0.85)
plot(X, Y, col = ifelse(D == 1, adjustcolor("#C84E00", 0.2), adjustcolor("#012169", 0.2)),
     pch = 16, cex = 0.4, xlab = "x", ylab = "y")
abline(f0_lin, col = "#012169", lwd = 1.5, lty = 2)
lines(grid$X, g1, col = "#C84E00", lwd = 2)
lines(grid$X, g0, col = "#012169", lwd = 2)
abline(v = 0.9, lty = 3, col = "gray50")
text(0.955, -0.6, "no\ncontrols", cex = 0.75)
segments(X[i], m0h[i], X[i], m1[i], lwd = 2)
points(X[i], Y[i], pch = 21, bg = "white", cex = 1.2, lwd = 1.5)
legend("topleft", bty = "n", cex = 0.78, lwd = c(2, 2, 1.5), lty = c(1, 1, 2), col = c("#C84E00", "#012169", "#012169"),
       legend = c("treated: forest", "controls: forest", "controls: Week 6's line"))
dev.off()
