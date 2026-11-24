## Week 14 -- SEM on Blau and Duncan's (1967) status-attainment model.
## Every number on the SEM frames comes from here.   Rscript figs/w14_sem.R > figs/w14_sem.out
## Data: their published correlations among five variables (Table 5.1), about 20,700 men.
## V = father's education, X = father's occupation, U = own education, W = first job,
## Y = occupation in 1962.
suppressMessages(library(lavaan))
nm <- c("V", "X", "U", "W", "Y"); n <- 20700
R <- diag(5); dimnames(R) <- list(nm, nm)
r <- c(XV = .516, UV = .453, UX = .438, WV = .332, WX = .417, WU = .538,
       YV = .322, YX = .405, YU = .596, YW = .541)
for (k in names(r)) { a <- substr(k, 1, 1); b <- substr(k, 2, 2); R[a, b] <- R[b, a] <- r[[k]] }
cat("## 1. the data: ten correlations\n"); print(R)

## OLS from a correlation matrix: slopes, and s.e. = sqrt(residual variance x [Rxx^-1]_jj / n)
ols <- function(y, x, n) { Ri <- solve(R[x, x, drop = FALSE]); b <- drop(Ri %*% R[x, y])
  s2 <- 1 - sum(b * R[x, y]); cbind(est = b, se = sqrt(s2 * diag(Ri) / n)) }

## ---- 2. by hand: the education equation U = a1 V + a2 X + e -------------------------
a1 <- (R["U","V"] - R["U","X"] * R["X","V"]) / (1 - R["X","V"]^2)
a2 <- (R["U","X"] - R["U","V"] * R["X","V"]) / (1 - R["X","V"]^2)
cat(sprintf("\n## 2. by hand: a1 = %.3f, a2 = %.3f\n", a1, a2))
b <- ols("W", c("U", "X"), n)[, 1]; cc <- ols("Y", c("U", "X", "W"), n)[, 1]
cat("first job on U, X:", round(b, 3), "; occupation 1962 on U, X, W:", round(cc, 3), "\n")

## ---- 3. two correlations the model did not use ------------------------------------------
rWV <- b[1] * R["U","V"] + b[2] * R["X","V"]
cat(sprintf("\n## 3. implied r(W,V) = %.3f x %.3f + %.3f x %.3f = %.3f; observed %.3f\n",
            b[1], R["U","V"], b[2], R["X","V"], rWV, R["W","V"]))
rYV <- cc[1] * R["U","V"] + cc[2] * R["X","V"] + cc[3] * rWV
cat(sprintf("implied r(Y,V) = %.3f; observed %.3f\n", rYV, R["Y","V"]))

## ---- 4. the test of fit: put each left-out arrow in, and test its coefficient ------------
cat("\n## 4. left-out arrows, added one equation at a time (OLS from the correlations)\n")
for (nn in c(20700, 500)) {
  tW <- ols("W", c("U", "X", "V"), nn)["V", ]; tY <- ols("Y", c("U", "X", "W", "V"), nn)["V", ]
  cat(sprintf("n = %5d: V -> W %.3f (se %.4f, t %.2f); V -> Y %.3f (se %.4f, t %.2f)\n", nn,
              tW[1], tW[2], tW[1] / tW[2], tY[1], tY[2], tY[1] / tY[2])) }
m0 <- ' U ~ V + X
        W ~ U + X
        Y ~ U + X + W '
f0 <- sem(m0, sample.cov = R, sample.nobs = n, sample.cov.rescale = FALSE, fixed.x = FALSE)
cat("software's single summary of the two gaps (chi-square test of fit):\n")
print(round(fitMeasures(f0, c("chisq", "df", "pvalue")), 3))
f5 <- sem(m0, sample.cov = R, sample.nobs = 500, sample.cov.rescale = FALSE, fixed.x = FALSE)
cat("the same with n = 500:\n"); print(round(fitMeasures(f5, c("chisq", "df", "pvalue")), 3))

## ---- 5. direct, indirect, total ----------------------------------------------------------
cat("\n## 5. effects on occupation in 1962 (Y)\n")
tu <- cc[1] + b[1] * cc[3]
cat(sprintf("education U: direct %.3f, through first job %.3f x %.3f = %.3f, total %.3f\n",
            cc[1], b[1], cc[3], b[1] * cc[3], tu))
cat(sprintf("father's occupation X: direct %.3f, through education %.3f, through first job only %.3f, total %.3f\n",
            cc[2], a2 * tu, b[2] * cc[3], cc[2] + a2 * tu + b[2] * cc[3]))
cat(sprintf("father's education V: all indirect, %.3f x %.3f = %.3f\n", a1, tu, a1 * tu))

## ---- 6. fit is not direction: reverse an arrow, same fit ---------------------------------
mR <- ' U ~ V + X
        Y ~ U + X
        W ~ U + X + Y '
fR <- sem(mR, sample.cov = R, sample.nobs = n, sample.cov.rescale = FALSE, fixed.x = FALSE)
cat("\n## 6. occupation in 1962 -> first job (arrow reversed):\n"); print(round(fitMeasures(fR, c("chisq", "df")), 3))
cat("largest gap between observed and implied correlations: original",
    round(max(abs(resid(f0, "cor")$cov)), 3), "; reversed", round(max(abs(resid(fR, "cor")$cov)), 3), "\n")

## ---- 7. what a paper reports: residual paths and the usual fit measures -------------------
ru <- sqrt(1 - sum(c(a1, a2) * R[c("V","X"), "U"])); rw <- sqrt(1 - sum(b * R[c("U","X"), "W"]))
ry <- sqrt(1 - sum(cc * R[c("U","X","W"), "Y"]))
cat(sprintf("\n## 7. residual paths sqrt(1 - R2): U %.3f, W %.3f, Y %.3f; R2: %.2f, %.2f, %.2f\n",
            ru, rw, ry, 1 - ru^2, 1 - rw^2, 1 - ry^2))
print(round(fitMeasures(f0, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr", "bic")), 4))
cat(sprintf("RMSEA by hand: sqrt((chisq - df) / (df x n)) = %.4f\n", sqrt((18.119 - 2) / (2 * n))))
m1 <- ' U ~ V + X
        W ~ U + X + V
        Y ~ U + X + W + V '
f1 <- sem(m1, sample.cov = R, sample.nobs = n, sample.cov.rescale = FALSE, fixed.x = FALSE)
cat("model with both arrows added (saturated): BIC", round(BIC(f1), 1), "; restricted: BIC", round(BIC(f0), 1), "\n")
mI <- ' U ~ a1*V + a2*X
        W ~ b1*U + b2*X
        Y ~ c1*U + c2*X + c3*W
        X_indirect := a2*(c1 + b1*c3) + b2*c3 '
fI <- sem(mI, sample.cov = R, sample.nobs = n, sample.cov.rescale = FALSE, fixed.x = FALSE)
pI <- parameterEstimates(fI); print(format(pI[pI$op == ":=", c("lhs", "est", "se", "ci.lower", "ci.upper")], digits = 3), row.names = FALSE)

## ---- 8. education and first job share an unobserved cause: correlated errors ---------------
## multiply the first-job equation by V and by X (both unrelated to e_W); V is the instrument
b1iv <- (R["W","V"] - R["W","X"] * R["X","V"]) / (R["U","V"] - R["U","X"] * R["X","V"])
b2iv <- R["W","X"] - b1iv * R["U","X"]
cat(sprintf("\n## 8. correlated errors of U and W: by hand b1 = %.3f / %.3f = %.3f, b2 = %.3f (OLS: %.3f, %.3f)\n",
            R["W","V"] - R["W","X"] * R["X","V"], R["U","V"] - R["U","X"] * R["X","V"], b1iv, b2iv, b[1], b[2]))
rf <- ols("W", c("V", "X"), n)["V", 1]; fs <- ols("U", c("V", "X"), n)["V", 1]
cat(sprintf("as a ratio: reduced form %.3f / first stage %.3f = %.3f\n", rf, fs, rf / fs))
mC <- ' U ~ V + X
        W ~ U + X
        Y ~ U + X + W
        U ~~ W '
fC <- sem(mC, sample.cov = R, sample.nobs = n, sample.cov.rescale = FALSE, fixed.x = FALSE)
pC <- parameterEstimates(fC, standardized = TRUE)
print(format(pC[pC$op == "~" | (pC$op == "~~" & pC$lhs != pC$rhs), c("lhs", "op", "rhs", "est", "se", "std.all")], digits = 3), row.names = FALSE)
print(round(fitMeasures(fC, c("chisq", "df", "pvalue")), 3))
