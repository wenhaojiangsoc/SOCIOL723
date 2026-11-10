## Week 12 -- worked examples for the frames written in the 690S style (slow versions).
## Same data, seed, and train/test split as figs/w12_gss.R.
##   Rscript figs/w12_690s.R > figs/w12_690s.out      (run from the week folder)
## Figures 3.1-3.4 and the sparse/dense simulation table are from
## ../SOCIOL690S/Week 2 Machine Learning Basics/regularization.qmd (copied as figs/fig3_*.jpg).
set.seed(723)
suppressMessages({library(glmnet); library(ranger); library(rpart)})
d <- readRDS("../Data/gss_earnings.rds")
d <- d[complete.cases(d[, c("lnearn", "educ", "age", "female", "black", "otherace", "wordsum", "pareduc",
                            "hours", "childs", "evermar", "region", "year")]), ]
d$region <- factor(d$region); d$yr <- factor(d$year)
f_main <- lnearn ~ educ + age + female + black + otherace + wordsum + pareduc + hours +
  childs + evermar + region + yr
num <- c("educ", "age", "wordsum", "pareduc", "hours", "childs")
f_big <- as.formula(paste("lnearn ~ (educ + age + female + black + otherace + wordsum + pareduc + hours + childs + evermar + region + yr)^2 +",
                          paste0("I(", num, "^2)", collapse = " + ")))
Xm <- model.matrix(f_main, d)[, -1]; Xb <- model.matrix(f_big, d)[, -1]
Xb <- Xb[, apply(Xb, 2, var) > 0]; Xb <- Xb[, !duplicated(t(Xb))]
y <- d$lnearn; n <- nrow(d)
te <- sample(n, round(0.3 * n)); tr <- setdiff(1:n, te)          # identical to w12_gss.R
mse <- function(p) mean((y[te] - p)^2); r2 <- function(p) 1 - mse(p) / mean((y[te] - mean(y[tr]))^2)
set.seed(7231)                                                    # new stream for this script

## ---- A. bias-variance by hand: three training samples, f(x0) = 10 -----------------
cat("## A. bias-variance toy\n")
fhat <- c(9, 10, 14); f0 <- 10
cat("mean fhat", mean(fhat), "; bias", mean(fhat) - f0, "; variance", round(mean((fhat - mean(fhat))^2), 3),
    "; MSE", round(mean((fhat - f0)^2), 3), "\n")
rigid <- c(11, 11, 11)
cat("rigid method: bias", mean(rigid) - f0, "; variance", mean((rigid - mean(rigid))^2), "; MSE", mean((rigid - f0)^2), "\n")

## ---- B. lasso on one standardized regressor: the subgradient rule ------------------
cat("\n## B. one regressor, n = 100, D = sum x^2 = 100, lambda = 40\n")
for (sxy in c(30, 15, -30)) {
  S <- 2 * sxy; D <- 100; lam <- 40
  b <- if (S > lam) (S - lam) / (2 * D) else if (S < -lam) (S + lam) / (2 * D) else 0
  cat("sum xy", sxy, "; S", S, "; OLS", S / (2 * D), "; lasso", b, "\n")
}
cat("ridge, same regressor, lambda = 25: shrink factor D/(D + lambda) =", 100 / 125, "\n")
for (sxy in c(30, 15, -30)) cat("sum xy", sxy, "; OLS", sxy / 100, "; ridge", 2 * sxy / (2 * (100 + 25)), "\n")

## ---- C. 5-fold CV on the GSS, lasso on the 294-term design --------------------------
cat("\n## C. 5-fold CV, lasso, three penalty levels (training sample)\n")
fold <- sample(rep(1:5, length.out = length(tr)))
path <- glmnet(Xb[tr, ], y[tr], alpha = 1)
lams <- path$lambda[c(1, 30, 60)]                                 # big, middle, small
cvtab <- sapply(lams, function(l) sapply(1:5, function(k) {
  m <- glmnet(Xb[tr[fold != k], ], y[tr[fold != k]], alpha = 1, lambda = path$lambda)
  mean((y[tr[fold == k]] - predict(m, Xb[tr[fold == k], ], s = l))^2) }))
nz <- sapply(lams, function(l) sum(coef(path, s = l)[-1] != 0))
colnames(cvtab) <- paste0("lambda=", signif(lams, 2), " (", nz, " terms)")
cvtab <- rbind(cvtab, average = colMeans(cvtab)); print(round(cvtab, 3))
cat("fold sizes:", table(fold), "\n")

## ---- D. plug-in penalty (690S iteration), and post-lasso -------------------------
cat("\n## D. plug-in lambda = 2 c sigma sqrt(n) z_{1 - a/(2p)}, c = 1.1, a = 0.1\n")
ntr <- length(tr); p <- ncol(Xb); z <- qnorm(1 - 0.1 / (2 * p))
Xs <- scale(Xb[tr, ]); sig <- sd(y[tr])                            # step 0: intercept only
for (it in 1:15) {
  lam <- 2 * 1.1 * sig * sqrt(ntr) * z
  m <- glmnet(Xs, y[tr], alpha = 1, lambda = lam / (2 * ntr), standardize = FALSE)
  sig_new <- sqrt(mean((y[tr] - predict(m, Xs))^2))
  cat(sprintf("iter %d: sigma %.4f -> lambda %.1f, terms kept %d, new sigma %.4f\n", it, sig, lam, sum(coef(m)[-1] != 0), sig_new))
  if (abs(sig_new - sig) < 1e-4) break; sig <- sig_new
}
cat("z_{1-a/2p} =", round(z, 3), "; sqrt(n) =", round(sqrt(ntr), 2), "\n")
Xte_s <- scale(Xb[te, ], attr(Xs, "scaled:center"), attr(Xs, "scaled:scale"))
keep <- which(coef(m)[-1] != 0)
cat("plug-in lasso: test R2", round(r2(as.vector(predict(m, Xte_s))), 3), "; kept:", colnames(Xb)[keep], "\n")
post <- lm.fit(cbind(1, Xb[tr, keep]), y[tr])
cat("post-lasso (plug-in): test R2", round(r2(as.vector(cbind(1, Xb[te, keep]) %*% coef(post))), 3), "\n")
cl <- cv.glmnet(Xb[tr, ], y[tr], alpha = 1, nfolds = 10)
k2 <- which(coef(cl, s = "lambda.min")[-1] != 0)
pc <- coef(lm.fit(cbind(1, Xb[tr, k2]), y[tr])); pc[is.na(pc)] <- 0
cat("CV lasso", length(k2), "terms: test R2", round(r2(as.vector(predict(cl, Xb[te, ], s = "lambda.min"))), 3),
    "; post-lasso test R2", round(r2(as.vector(cbind(1, Xb[te, k2]) %*% pc)), 3), "\n")
## shrinkage: lasso vs post-lasso coefficient on education (main-effects design, standardized)
Zm <- scale(Xm[tr, ]); clm <- cv.glmnet(Zm, y[tr], alpha = 1, nfolds = 10)
bl <- coef(clm, s = "lambda.1se")[-1]; names(bl) <- colnames(Xm); km <- which(bl != 0)
bp <- coef(lm(y[tr] ~ Zm[, km]))[-1]; names(bp) <- colnames(Xm)[km]
cat("standardized coefficients, lasso (1se) vs post-lasso:\n"); print(round(rbind(lasso = bl[km], post = bp), 3))

## ---- F. trees: a split by hand, and the GSS root split ----------------------------
cat("\n## F. six people: years of schooling and log earnings\n")
toy <- data.frame(educ = c(10, 12, 12, 14, 16, 18), y = c(9.6, 9.8, 10.0, 10.4, 10.8, 11.0))
sse <- function(v) sum((v - mean(v))^2)
cat("SSE before any split:", round(sse(toy$y), 3), "; mean", round(mean(toy$y), 3), "\n")
for (s in c(11, 13, 15, 17)) {
  L <- toy$y[toy$educ <= s]; R <- toy$y[toy$educ > s]
  cat(sprintf("cut educ <= %d: left mean %.3f (n %d), right mean %.3f (n %d), SSE %.3f + %.3f = %.3f\n",
              s, mean(L), length(L), mean(R), length(R), sse(L), sse(R), sse(L) + sse(R)))
}
cat("\n## F2. GSS: the first split chosen by rpart on the training sample\n")
dtr <- data.frame(y = y[tr], Xm[tr, ]); dte <- data.frame(Xm[te, ])
root <- rpart(y ~ ., dtr, control = rpart.control(maxdepth = 1, cp = 0))
print(root)
cat("SSE root", round(sse(y[tr]), 1), "\n")
t3 <- rpart(y ~ ., dtr, control = rpart.control(maxdepth = 3, cp = 0)); print(t3)
cat("depth-3 tree test R2", round(r2(predict(t3, dte)), 3), "\n")
for (dep in c(1, 2, 3, 5, 8, 12, 20)) {
  tt <- rpart(y ~ ., dtr, control = rpart.control(maxdepth = min(dep, 30), cp = 0, minsplit = 2, minbucket = 1))
  cat(sprintf("depth %2d: leaves %4d, training R2 %.3f, test R2 %.3f\n", dep, sum(tt$frame$var == "<leaf>"),
              1 - mean((y[tr] - predict(tt, dtr))^2) / mean((y[tr] - mean(y[tr]))^2), r2(predict(tt, dte))))
}

## ---- G. one deep tree vs bagging vs random forest on the GSS ---------------------
cat("\n## G. deep tree, bagging (mtry = p), random forest (mtry = 5); 500 trees\n")
deep <- ranger(y ~ ., dtr, num.trees = 1, mtry = ncol(Xm), replace = FALSE, sample.fraction = 1, min.node.size = 5, seed = 1)
bag <- ranger(y ~ ., dtr, num.trees = 500, mtry = ncol(Xm), min.node.size = 5, seed = 1)
rf5 <- ranger(y ~ ., dtr, num.trees = 500, mtry = 5, min.node.size = 5, seed = 1)
cat("p =", ncol(Xm), "; one deep tree test R2", round(r2(predict(deep, dte)$predictions), 3),
    "; bagging", round(r2(predict(bag, dte)$predictions), 3), "; random forest", round(r2(predict(rf5, dte)$predictions), 3), "\n")
## correlation between two bagged trees' test predictions vs two forest trees'
pb <- predict(bag, dte, predict.all = TRUE)$predictions; pf <- predict(rf5, dte, predict.all = TRUE)$predictions
cb <- cor(pb[, 1:50]); cf <- cor(pf[, 1:50])
cat("average correlation between trees: bagging", round(mean(cb[upper.tri(cb)]), 2),
    "; random forest", round(mean(cf[upper.tri(cf)]), 2), "\n")
cat("variance of the average: rho s2 + (1-rho) s2 / B, s2 = 1, B = 500, rho = .6 / .3:",
    round(0.6 + 0.4 / 500, 4), round(0.3 + 0.7 / 500, 4), "\n")

## ---- H. boosting by hand on the six people: stumps, learning rate 0.5 ----------
cat("\n## H. boosting the six people, stumps, learning rate 0.5\n")
stump <- function(x, r) {      # best single split of r on x; returns fitted values
  cuts <- sort(unique(x)); cuts <- (head(cuts, -1) + tail(cuts, -1)) / 2
  s <- cuts[which.min(sapply(cuts, function(c) sse(r[x <= c]) + sse(r[x > c])))]
  list(cut = s, fit = ifelse(x <= s, mean(r[x <= s]), mean(r[x > s])))
}
R <- toy$y - mean(toy$y); fhat <- rep(mean(toy$y), 6)
cat("round 0: f = mean", round(mean(toy$y), 3), "; residuals", round(R, 3), "; SSR", round(sum(R^2), 3), "\n")
for (j in 1:4) {
  st <- stump(toy$educ, R); fhat <- fhat + 0.5 * st$fit; R <- R - 0.5 * st$fit
  cat(sprintf("round %d: cut %.0f, stump fits %s; residuals %s; SSR %.3f\n", j, st$cut,
              paste(round(unique(st$fit), 3), collapse = "/"), paste(round(R, 3), collapse = " "), sum(R^2)))
}

## ---- I. gradient descent by hand, and one backpropagation step ------------------
cat("\n## I. gradient descent on R(theta) = 1/2 sum (Y - theta)^2, Y = 1, 2, 6, rho = 0.1\n")
Y <- c(1, 2, 6); th <- 0
for (t in 0:5) { g <- -sum(Y - th); cat(sprintf("t %d: theta %.3f, R %.3f, gradient %.3f\n", t, th, 0.5 * sum((Y - th)^2), g)); th <- th - 0.1 * g }
cat("minimizer = mean(Y) =", mean(Y), "\n")
cat("\n## I2. one case, one ReLU unit: X = 1, Y = 4, w0 = 0, w1 = 2, b0 = 0, b1 = 1.5, rho = 0.1\n")
X <- 1; Yc <- 4; w0 <- 0; w1 <- 2; b0 <- 0; b1 <- 1.5
fw <- function(w1, b1) { z <- w0 + w1 * X; A <- max(0, z); b0 + b1 * A }
z <- w0 + w1 * X; A <- max(0, z); f <- b0 + b1 * A; r <- Yc - f
gb1 <- -r * A; gw1 <- -r * b1 * (z > 0) * X
cat("forward: z", z, "A", A, "f", f, "residual", r, "loss", 0.5 * r^2, "\n")
cat("backward: dR/db1", gb1, "; dR/dw1", gw1, "\n")
b1n <- b1 - 0.1 * gb1; w1n <- w1 - 0.1 * gw1; fn <- fw(w1n, b1n)
cat("after one step: b1", b1n, "w1", w1n, "f", fn, "loss", round(0.5 * (Yc - fn)^2, 4), "\n")

## ---- I3. activation values --------------------------------------------------------
zz <- c(-1, 0.5, 2)
cat("\n## I3. sigmoid at z = -1, .5, 2:", round(1 / (1 + exp(-zz)), 2), "; ReLU:", pmax(0, zz), "\n")
cat("two-neuron check X1 = 2, X2 = 3: h1", (2 + 3)^2, "h2", (2 - 3)^2, "f", ((2 + 3)^2 - (2 - 3)^2) / 4, "\n")

## ---- K. the two prediction-error bounds (constants dropped) -----------------------
cat("\n## K. sqrt(p/n) vs sqrt(s log(max(p,n)) / n)\n")
cat("GSS: p", ncol(Xb), ", n", ntr, "\n")
cat("690S simulation: p 400, n 100, s 6:", round(sqrt(400 / 100), 3), "vs", round(sqrt(6 * log(400) / 100), 3), "\n")

## ---- J. parameter counts ---------------------------------------------------------
cat("\n## J. parameters: single layer p =", ncol(Xm), ", K = 2:", (ncol(Xm) + 1) * 2 + 2 + 1,
    "; MNIST 784-256-128-10:", 785 * 256 + 257 * 128 + 129 * 10,
    "=", 785 * 256, "+", 257 * 128, "+", 129 * 10, "\n")
