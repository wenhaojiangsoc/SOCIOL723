## =============================================================================
## w13_brand.R -- every number on the Brand et al. (2021) slides
## Effect of college (BA by 25) on the share of 1990-2014 in low-wage work,
## NLSY79, causal forest replication.  Run from the Week 13 folder:
##   Rscript figs/w13_brand.R > figs/w13_brand.out
## Data: ../Data/nlsy79_brand.rds, built by ../Data/build_nlsy79_brand.R
## =============================================================================
suppressMessages({library(grf); library(rpart)})
## grf warns at every call that some propensities are near 0 (38% are below .05);
## that is reported in section 1 and on the overlap slide, so the repeats are silenced
options(warn = -1)
d <- readRDS("../Data/nlsy79_brand.rds")
xvars <- c("male","black","hispanic","south","rural","parinc","fatheduc","motheduc",
           "fatherwc","twoparent","siblings","ability","collegeprep","rotter","delinq",
           "expect","aspire","friendasp","schooldis","married18","child18",
           "parinc_miss","schooldis_miss","fatheduc_miss")
X <- as.matrix(d[, xvars]); Y <- d$lowwage; D <- d$college; w <- d$weight
n <- nrow(d)
cat("n =", n, " treated =", sum(D), " share treated (weighted) =", round(weighted.mean(D, w), 3), "\n")
cat("mean outcome, no college / college (weighted):",
    round(weighted.mean(Y[D == 0], w[D == 0]), 3), round(weighted.mean(Y[D == 1], w[D == 1]), 3), "\n")

## ---- 1. Average effects (Brand Table 2) --------------------------------------
cat("\n== 1. average effects ==\n")
raw <- lm(Y ~ D, weights = w)
cat("unadjusted:", round(coef(raw)[2], 3), "(", round(sqrt(sandwich::vcovHC(raw, "HC1")[2, 2]), 3), ")\n")
ols <- lm(Y ~ D + X, weights = w)
cat("OLS, 24 linear controls:", round(coef(ols)[2], 3), "(", round(sqrt(sandwich::vcovHC(ols, "HC1")[2, 2]), 3), ")\n")

set.seed(723)
ps_forest <- regression_forest(X, D, num.trees = 2000, seed = 723)
e_hat <- predict(ps_forest)$predictions               # out-of-bag propensity
e_hat <- pmin(pmax(e_hat, 0.01), 0.99)
ipw_w <- w * ifelse(D == 1, 1 / e_hat, 1 / (1 - e_hat))
ipw <- lm(Y ~ D, weights = ipw_w)
cat("IPW (forest propensity):", round(coef(ipw)[2], 3), "(", round(sqrt(sandwich::vcovHC(ipw, "HC1")[2, 2]), 3), ")\n")
cat("propensity range:", round(range(e_hat), 3), " share < .05:", round(mean(e_hat < .05), 3), "\n")

cf <- causal_forest(X, Y, D, sample.weights = w, num.trees = 4000, seed = 723)
ate <- average_treatment_effect(cf, target.sample = "all")
att <- average_treatment_effect(cf, target.sample = "treated")
cat("causal forest ATE:", round(ate, 3), "\n")
cat("causal forest ATT:", round(att, 3), "\n")

## the pieces the forest uses: Y.hat = E[Y|X], W.hat = e(X) (both out of bag)
cat("\n== 2. the residuals behind the forest (local centering) ==\n")
cat("R2 of Y.hat:", round(1 - mean((Y - cf$Y.hat)^2) / var(Y), 3),
    "  R2 of W.hat:", round(1 - mean((D - cf$W.hat)^2) / var(D), 3), "\n")
## the forest-wide (constant-effect) DML estimate = residual-on-residual
rY <- Y - cf$Y.hat; rD <- D - cf$W.hat
cat("residual-on-residual slope (partially linear, weighted):",
    round(sum(w * rD * rY) / sum(w * rD^2), 3), "\n")

## ---- 3. CATEs: distribution, calibration test --------------------------------
cat("\n== 3. CATEs ==\n")
tau <- predict(cf)$predictions                        # out-of-bag CATE
cat("CATE quantiles (5,25,50,75,95%):", round(quantile(tau, c(.05,.25,.5,.75,.95)), 3), "\n")
cat("share of CATEs < 0:", round(mean(tau < 0), 3), "\n")
print(test_calibration(cf))

## doubly robust scores (AIPW pseudo-outcome) from the forest's own nuisances
Gamma <- get_scores(cf)
cat("mean DR score (= ATE, unweighted):", round(mean(Gamma), 3), "\n")

## ---- 4. Brand Table 3: averages within prespecified partitions ---------------
cat("\n== 4. GATEs within prespecified partitions (DR scores averaged) ==\n")
gate <- function(g) {
  t(sapply(split(seq_len(n), g), function(idx) {
    a <- average_treatment_effect(cf, subset = idx)
    c(est = round(a[1], 3), se = round(a[2], 3), n = length(idx))
  }))
}
pst <- cut(e_hat, c(0, .2, .5, 1), labels = c("low <.2", "mid .2-.5", "high >=.5"), include.lowest = TRUE)
inc3 <- cut(d$parinc, quantile(d$parinc, c(0, 1/3, 2/3, 1)), include.lowest = TRUE, labels = c("low","mid","high"))
ab3  <- cut(d$ability, quantile(d$ability, c(0, 1/3, 2/3, 1)), include.lowest = TRUE, labels = c("low","mid","high"))
med3 <- cut(d$motheduc, c(-Inf, 11, 12, Inf), labels = c("<HS","HS","some college+"))
race <- ifelse(d$black == 1, "black", ifelse(d$hispanic == 1, "hispanic", "white/other"))
cat("propensity strata\n"); print(gate(pst))
cat("parental income terciles\n"); print(gate(inc3))
cat("mother's education\n"); print(gate(med3))
cat("ability terciles\n"); print(gate(ab3))
cat("race\n"); print(gate(race))

## best linear projection of the CATE on the propensity score (Brand Fig A1)
cat("\nBLP of CATE on the propensity score:\n")
print(best_linear_projection(cf, A = cbind(propensity = e_hat)))

## ---- 5. An honest tree on the DR scores (Brand Table 4, the idea) ------------
cat("\n== 5. honest tree: split on half A, estimate on half B ==\n")
set.seed(723)
half <- sample(rep(1:2, length.out = n))
A <- half == 1; B <- half == 2
## grow on half A: regress the DR scores on X (transformed-outcome tree)
trA <- rpart(Gamma ~ ., data = data.frame(Gamma = Gamma, X)[A, ],
             control = rpart.control(maxdepth = 2, minbucket = 150, cp = 0))
print(trA)
## estimate on half B: drop B's people down the tree, average B's DR scores per leaf
leafB <- predict(trA, newdata = data.frame(X)[B, ])          # leaf label = A's mean
leafB_id <- factor(leafB)
estB <- t(sapply(split(which(B), leafB_id), function(idx) {
  a <- average_treatment_effect(cf, subset = idx)
  c(est = round(a[1], 3), se = round(a[2], 3), n = length(idx))
}))
cat("leaf means on half A (the 'adaptive' estimates):\n"); print(sort(unique(round(leafB, 3))))
cat("same leaves, effects estimated on half B (honest):\n"); print(estB)

## ---- 6. variable importance --------------------------------------------------
cat("\n== 6. variable importance (share of splits, depth-weighted) ==\n")
vi <- variable_importance(cf)
o <- order(vi, decreasing = TRUE)
print(data.frame(var = xvars[o], importance = round(vi[o], 3))[1:10, ])

## ---- 7. RATE: does ranking by the forest find larger effects? -----------------
cat("\n== 7. RATE (train on half A, evaluate on half B) ==\n")
cfA <- causal_forest(X[A, ], Y[A], D[A], sample.weights = w[A], num.trees = 2000, seed = 723)
cfB <- causal_forest(X[B, ], Y[B], D[B], sample.weights = w[B], num.trees = 2000, seed = 723)
prioB <- -predict(cfA, X[B, ])$predictions     # larger reduction in low-wage work first
rate_cf <- rank_average_treatment_effect(cfB, prioB)
rate_ps <- rank_average_treatment_effect(cfB, -e_hat[B])   # low propensity first
print(rate_cf); print(rate_ps)

## ---- 8. figures --------------------------------------------------------------
navy <- rgb(1, 33, 105, maxColorValue = 255); fire <- "firebrick"
pdf("figs/brand_cate_hist.pdf", width = 6.5, height = 3.4)
par(mar = c(4.2, 4.2, 1, 1), bty = "l", las = 1, cex.axis = .9)
hist(tau, breaks = 40, col = adjustcolor(navy, .35), border = "white",
     main = "", xlab = "estimated effect of college on share of years in low-wage work",
     ylab = "people")
abline(v = ate[1], col = fire, lwd = 2)
text(ate[1], par("usr")[4] * .92, paste0("ATE ", sprintf("%.3f", ate[1])), pos = 4, col = fire, cex = .9)
dev.off()

pdf("figs/brand_gate_ps.pdf", width = 6.5, height = 3.4)
g <- gate(pst)
par(mar = c(4.2, 4.6, 1, 1), bty = "l", las = 1, cex.axis = .9)
x <- 1:3
plot(x, g[, 1], ylim = c(-.35, 0), xlim = c(.6, 3.4), pch = 19, col = navy, xaxt = "n",
     xlab = "estimated propensity of college", ylab = "effect on low-wage share")
axis(1, at = x, labels = c("low (< .2)", "middle (.2 to .5)", "high (> .5)"))
arrows(x, g[, 1] - 1.96 * g[, 2], x, g[, 1] + 1.96 * g[, 2], angle = 90, code = 3, length = .05, col = navy)
abline(h = ate[1], lty = 2, col = fire)
abline(h = 0, col = "gray70")
text(x, g[, 1], sprintf("%.3f", g[, 1]), pos = 4, cex = .85)
dev.off()
## TOC curves on half B: gain from serving only the top q, by each ranking rule
toc <- function(r) r$TOC
tc <- toc(rate_cf); tp <- toc(rate_ps)
for (q in c(.1, .2, .3, .5)) {
  i <- which.min(abs(tc$q - q)); j <- which.min(abs(tp$q - q))
  cat(sprintf("TOC(%.1f): forest %.3f (%.3f); propensity %.3f (%.3f)\n", q,
              tc$estimate[i], tc$std.err[i], tp$estimate[j], tp$std.err[j]))
}
pdf("figs/brand_toc.pdf", width = 6.5, height = 3.4)
par(mar = c(4.2, 4.6, 1, 1), bty = "l", las = 1, cex.axis = .9)
plot(tc$q, tc$estimate, type = "l", lwd = 2, col = navy, ylim = range(c(tc$estimate - 1.96 * tc$std.err, tp$estimate, 0.03)),
     xlab = "share served, q (highest predicted effect first)",
     ylab = "TOC(q): effect in top q minus ATE")
polygon(c(tc$q, rev(tc$q)), c(tc$estimate - 1.96 * tc$std.err, rev(tc$estimate + 1.96 * tc$std.err)),
        col = adjustcolor(navy, .15), border = NA)
lines(tp$q, tp$estimate, lwd = 2, col = fire, lty = 2)
abline(h = 0, col = "gray50")
legend("topleft", c("ranked by forest's tau-hat(x)", "ranked by propensity, lowest first"),
       col = c(navy, fire), lty = c(1, 2), lwd = 2, bty = "n", cex = .85, inset = c(.15, 0))
dev.off()
cat("\nfigures written\n")
