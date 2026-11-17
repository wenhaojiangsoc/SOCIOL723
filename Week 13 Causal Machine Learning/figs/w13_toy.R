## =============================================================================
## w13_toy.R -- hypothetical worked numbers for the causal-tree / forest frames
##   Rscript figs/w13_toy.R > figs/w13_toy.out
## =============================================================================

## ---- A. eight people, one split by hand ---------------------------------------
cat("== A. eight people: which split separates effects best? ==\n")
toy <- data.frame(
  id   = 1:8,
  momHS = c(0, 0, 0, 0, 1, 1, 1, 1),        # mother finished high school
  sibs  = c(4, 1, 2, 5, 3, 4, 1, 2),        # number of siblings
  D     = c(1, 0, 1, 0, 1, 0, 1, 0),        # college
  Y     = c(.10, .50, .15, .45, .05, .20, .10, .25)   # share of years in low-wage work
)
print(toy)
leaf_eff <- function(idx) with(toy[idx, ], mean(Y[D == 1]) - mean(Y[D == 0]))
crit <- function(L) {           # n_L tau_L^2 + n_R tau_R^2
  R <- setdiff(toy$id, L)
  tl <- leaf_eff(L); tr <- leaf_eff(R)
  c(tau_L = tl, tau_R = tr, n_L = length(L), n_R = length(R),
    crit = length(L) * tl^2 + length(R) * tr^2)
}
all_eff <- leaf_eff(toy$id)
cat("no split: tau =", round(all_eff, 3), " n tau^2 =", round(8 * all_eff^2, 4), "\n")
cat("split on mother HS:\n"); print(round(crit(which(toy$momHS == 0)), 4))
cat("split on siblings >= 3:\n"); print(round(crit(which(toy$sibs >= 3)), 4))

## ---- B. why maximizing n tau^2: identity check --------------------------------
## sum_l n_l tau_l^2 = n * (mean tau)^2 + sum_l n_l (tau_l - mean tau)^2  (with n_l-weighted mean)
cat("\n== B. identity: sum n_l tau_l^2 = n tau^2 + sum n_l (tau_l - tau)^2 ==\n")
v <- crit(which(toy$momHS == 0))
tbar <- (v["n_L"] * v["tau_L"] + v["n_R"] * v["tau_R"]) / 8
cat("weighted mean of leaf effects:", round(tbar, 4), "\n")
cat("lhs:", round(v["crit"], 4), " rhs:", round(8 * tbar^2 + v["n_L"] * (v["tau_L"] - tbar)^2 +
                                                v["n_R"] * (v["tau_R"] - tbar)^2, 4), "\n")

## ---- C. adaptive bias: no true heterogeneity, pick the best of 20 splits -------
cat("\n== C. adaptive vs honest leaf estimates, no true heterogeneity ==\n")
set.seed(723)
R <- 2000; n <- 400; p <- 20
res <- t(replicate(R, {
  X <- matrix(rbinom(n * p, 1, .5), n, p); D <- rbinom(n, 1, .5)
  Y <- -0.15 * D + rnorm(n, 0, .3)                      # true effect -0.15 for everyone
  A <- 1:(n / 2); B <- (n / 2 + 1):n                     # half A chooses, half B estimates
  eff <- function(idx) mean(Y[idx][D[idx] == 1]) - mean(Y[idx][D[idx] == 0])
  ## choose the variable whose split makes the leaf effects differ most in half A
  sc <- sapply(1:p, function(j) {
    l <- A[X[A, j] == 0]; r <- A[X[A, j] == 1]
    length(l) * eff(l)^2 + length(r) * eff(r)^2
  })
  j <- which.max(sc)
  lA <- A[X[A, j] == 0]; rA <- A[X[A, j] == 1]
  lB <- B[X[B, j] == 0]; rB <- B[X[B, j] == 1]
  ## the leaf that looks most negative in A
  if (eff(lA) < eff(rA)) c(eff(lA), eff(lB), eff(rA), eff(rB)) else c(eff(rA), eff(rB), eff(lA), eff(lB))
}))
colnames(res) <- c("bigleaf_A", "bigleaf_B", "smallleaf_A", "smallleaf_B")
cat("true effect in every leaf: -0.150\n")
print(round(colMeans(res), 3))
cat("average gap between leaves: adaptive", round(mean(res[, 3] - res[, 1]), 3),
    "; honest", round(mean(res[, 4] - res[, 2]), 3), "\n")
## naive test: share of draws where the adaptive gap is 'significant' at 5%
se_gap <- sqrt(4 * .3^2 / 100 + 4 * .3^2 / 100)      # two leaves of ~100, half treated each
cat("share of draws with |adaptive gap| > 1.96 se:", round(mean(abs(res[, 3] - res[, 1]) > 1.96 * se_gap), 3),
    "; honest:", round(mean(abs(res[, 4] - res[, 2]) > 1.96 * se_gap), 3), "\n")

## ---- D. forest weights by hand: three trees, six people -----------------------
cat("\n== D. forest weights alpha_i(x): three trees, six people ==\n")
## leaf membership of persons 1..6 in each tree, and the leaf of the target x
leaf <- rbind(tree1 = c(1, 1, 1, 2, 2, 2),
              tree2 = c(1, 1, 2, 2, 2, 2),
              tree3 = c(1, 2, 1, 2, 1, 2))
x_leaf <- c(tree1 = 1, tree2 = 1, tree3 = 1)        # x falls in leaf 1 of every tree
alpha <- sapply(1:6, function(i) mean(sapply(1:3, function(b) {
  same <- leaf[b, ] == x_leaf[b]; (leaf[b, i] == x_leaf[b]) / sum(same)
})))
names(alpha) <- paste0("person", 1:6)
print(round(alpha, 3)); cat("sum:", sum(alpha), "\n")
## each tree's contribution
for (b in 1:3) { same <- leaf[b, ] == x_leaf[b]; cat(rownames(leaf)[b], ":", round(same / sum(same), 3), "\n") }

## the effect at x from the weighted neighbours: weighted treated mean minus weighted untreated mean
Dn <- c(1, 0, 1, 0, 1, 0); Yn <- c(.10, .40, .15, .30, .05, .35)
mt <- sum(alpha * Dn * Yn) / sum(alpha * Dn); mu <- sum(alpha * (1 - Dn) * Yn) / sum(alpha * (1 - Dn))
cat("weights on treated:", round(sum(alpha * Dn), 3), " untreated:", round(sum(alpha * (1 - Dn)), 3), "\n")
cat("weighted treated mean", round(mt, 3), "- weighted untreated mean", round(mu, 3), "=", round(mt - mu, 3), "\n")

## ---- E. doubly robust scores for two people -----------------------------------
cat("\n== E. AIPW (doubly robust) score, two people ==\n")
## person a: went to college, e = .20, m1 = .10, m0 = .30, Y = .05
## person b: no college,       e = .20, m1 = .10, m0 = .30, Y = .40
G <- function(D, Y, e, m1, m0) m1 - m0 + D * (Y - m1) / e - (1 - D) * (Y - m0) / (1 - e)
cat("Gamma_a =", G(1, .05, .2, .10, .30), "; Gamma_b =", G(0, .40, .2, .10, .30),
    "; plug-in m1 - m0 =", .10 - .30, "\n")

## ---- F. inside one leaf: difference in means vs residual on residual ---------
## 20 hypothetical people in one leaf, effect -0.15 for everyone, no noise.
## low ability: e = .2, baseline Y(0) = .40; high ability: e = .6, Y(0) = .20
cat("\n== F. confounding inside a leaf ==\n")
f <- data.frame(ability = c("low","low","high","high"), D = c(1,0,1,0),
                count = c(2,8,6,4), e = c(.2,.2,.6,.6), mu0 = c(.40,.40,.20,.20))
tau <- -0.15
f$Y   <- f$mu0 + tau * f$D
f$ell <- f$mu0 + tau * f$e              # E[Y|X] = mu0 + tau e
f$Yt  <- f$Y - f$ell; f$Dt <- f$D - f$e
print(f)
dm <- with(f, sum(count*Y*D)/sum(count*D) - sum(count*Y*(1-D))/sum(count*(1-D)))
cat("difference in means:", round(dm, 3), "\n")
cat("sum Dt*Yt:", with(f, sum(count*Dt*Yt)), " sum Dt^2:", with(f, sum(count*Dt^2)),
    " ratio:", with(f, sum(count*Dt*Yt)/sum(count*Dt^2)), "\n")
cat("mean baseline among treated:", with(f, sum(count*mu0*D)/sum(count*D)),
    " among untreated:", round(with(f, sum(count*mu0*(1-D))/sum(count*(1-D))), 3), "\n")
