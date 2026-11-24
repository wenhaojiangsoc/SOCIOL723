## Week 14 -- a latent variable as a SEM, and how it differs from PCA.
## Bollen's (1989) democracy data, lavaan::PoliticalDemocracy: 75 developing countries, 1960.
## y1 press freedom, y2 freedom of political opposition, y3 fairness of elections,
## y8 effectiveness of the elected legislature, rated in 1965 (expert ratings). The 1960 legislature
## rating (y4) comes from the same source and year as y2, and Bollen frees that error covariance;
## with y8 in its place a single latent variable fits (chi-square 3.9 on 2 df).
##   Rscript figs/w14_factor.R > figs/w14_factor.out
suppressMessages(library(lavaan))
d <- as.data.frame(scale(PoliticalDemocracy[, c("y1", "y2", "y3", "y8")]))
names(d) <- c("A", "B", "C", "D"); R <- cor(d)
cat("## observed correlations (6 of them)\n"); print(round(R, 3))
f <- cfa("T =~ A + B + C + D", d, std.lv = TRUE)          # variance of T fixed at 1
lam <- standardizedSolution(f); lam <- lam[lam$op == "=~", c("rhs", "est.std", "se")]
cat("\n## loadings (standardized): A = lambda_A T + e_A, ...\n"); print(format(lam, digits = 3), row.names = FALSE)
l <- setNames(lam$est.std, lam$rhs)
cat("error variances 1 - lambda^2:", round(1 - l^2, 2), "\n")
imp <- outer(l, l); diag(imp) <- 1
cat("\n## implied correlations lambda_j x lambda_k, and observed minus implied\n")
print(round(imp, 3)); print(round(R - imp, 3))
cat(sprintf("A and B: %.3f x %.3f = %.3f (observed %.3f)\n", l["A"], l["B"], l["A"] * l["B"], R["A", "B"]))
print(round(fitMeasures(f, c("npar", "chisq", "df", "pvalue", "rmsea", "srmr")), 3))

## PCA on the same four columns (Week 13)
e <- eigen(R); w <- e$vectors[, 1] * sign(sum(e$vectors[, 1]))
cat("\n## PCA: first component weights", round(w, 3), "; share of variance", round(e$values[1] / 4, 3), "\n")
pc <- as.matrix(d) %*% w; fs <- lavPredict(f)
cat("correlation of the PCA score with the factor score:", round(cor(pc, fs), 3), "\n")
cat("correlation of each indicator with PC1:", round(cor(d, pc), 3), "\n")

## loadings by hand: r_AB r_AC / r_BC = lambda_A^2, from each triple of indicators that contains A
cat("\n## lambda_A from three correlations at a time\n")
for (tr in list(c("B", "C"), c("B", "D"), c("C", "D"))) {
  v <- R["A", tr[1]] * R["A", tr[2]] / R[tr[1], tr[2]]
  cat(sprintf("A with %s, %s: %.3f x %.3f / %.3f = %.3f, lambda_A = %.2f\n", tr[1], tr[2],
              R["A", tr[1]], R["A", tr[2]], R[tr[1], tr[2]], v, sqrt(v))) }
cat("maximum likelihood (all six correlations at once):", round(l["A"], 3), "\n")
