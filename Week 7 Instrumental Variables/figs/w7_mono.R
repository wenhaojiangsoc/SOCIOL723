## Week 7 -- detecting a monotonicity failure in a judge design: leniency by case type.
## Every number on the 'Detecting a Monotonicity Failure' frame comes from here.
##   Rscript figs/w7_mono.R > figs/w7_mono.out ; writes figs/mono_judges.pdf
set.seed(723)
J <- 50; nj <- 200                                   # judges, cases per judge (half of each type)
lam <- runif(J, 0.25, 0.75)                         # common harshness
## monotone world: every judge is harsher on drug cases by the same margin; ordering is shared
pA_m <- pmin(lam + 0.10, 0.95); pB_m <- pmax(lam - 0.10, 0.05)
## failing world: judges harsh on drug cases are lenient on property cases (the worked failure)
pA_f <- lam;                   pB_f <- 1 - lam
rate <- function(p) rbinom(J, nj / 2, p) / (nj / 2)      # observed rates with sampling noise
rA_m <- rate(pA_m); rB_m <- rate(pB_m); rA_f <- rate(pA_f); rB_f <- rate(pB_f)
cat(sprintf("monotone world: corr of judge rates across case types = %.2f\n", cor(rA_m, rB_m)))
cat(sprintf("failing world:  corr of judge rates across case types = %.2f\n", cor(rA_f, rB_f)))
## pairwise check: share of judge pairs ordered the same way on both case types
pairs_same <- function(a, b) { o <- outer(a, a, ">"); p <- outer(b, b, ">"); mean((o == p)[upper.tri(o)]) }
cat(sprintf("share of judge pairs with the same ordering on both types: monotone %.2f, failing %.2f\n",
            pairs_same(rA_m, rB_m), pairs_same(rA_f, rB_f)))
pdf("figs/mono_judges.pdf", width = 7.5, height = 3.6, family = "Times")
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), cex = 1.1)
plot(rA_m, rB_m, pch = 19, col = "navy", xlim = c(0, 1), ylim = c(0, 1), main = "A. monotonicity holds",
     xlab = "judge's rate, drug cases", ylab = "judge's rate, property cases"); abline(0, 1, lty = 3)
plot(rA_f, rB_f, pch = 19, col = "firebrick", xlim = c(0, 1), ylim = c(0, 1), main = "B. monotonicity fails",
     xlab = "judge's rate, drug cases", ylab = "judge's rate, property cases"); abline(0, 1, lty = 3)
dev.off()
