## Week 12 -- tuning a random forest on the GSS (same data and split as figs/w12_gss.R).
## Every number on 'Tuning a Random Forest' comes from here.
##   Rscript figs/w12_rf_tune.R > figs/w12_rf_tune.out   (run from the week folder)
set.seed(723)
suppressMessages(library(ranger))
d <- readRDS("../Data/gss_earnings.rds")
d <- d[complete.cases(d[, c("lnearn", "educ", "age", "female", "black", "otherace", "wordsum", "pareduc",
                            "hours", "childs", "evermar", "region", "year")]), ]
d$region <- factor(d$region); d$yr <- factor(d$year)
f_main <- lnearn ~ educ + age + female + black + otherace + wordsum + pareduc + hours +
  childs + evermar + region + yr
Xm <- model.matrix(f_main, d)[, -1]
y <- d$lnearn; n <- nrow(d)
te <- sample(n, round(0.3 * n)); tr <- setdiff(1:n, te)      # identical to w12_gss.R
dtr <- data.frame(y = y[tr], Xm[tr, ]); dte <- data.frame(Xm[te, ])
r2 <- function(p) 1 - mean((y[te] - p)^2) / mean((y[te] - mean(y[tr]))^2)
p <- ncol(Xm)
cat("p =", p, "; ranger default mtry = floor(sqrt(p)) =", floor(sqrt(p)), "; default min.node.size (regression) = 5\n")

## ---- A. mtry x min.node.size grid, judged by out-of-bag MSE (training data only) ----
M <- c(1, 2, 3, 5, 8, 12, 23); N <- c(1, 5, 20, 50, 100)
oob <- sapply(N, function(nd) sapply(M, function(m)
  ranger(y ~ ., dtr, num.trees = 1000, mtry = m, min.node.size = nd, seed = 723)$prediction.error))
dimnames(oob) <- list(paste0("mtry=", M), paste0("node=", N))
cat("\nA. out-of-bag MSE, 1,000 trees\n"); print(round(oob, 3))
b <- which(oob == min(oob), arr.ind = TRUE)
mb <- M[b[1]]; nb <- N[b[2]]
cat("best by OOB: mtry", mb, "; min.node.size", nb, "; OOB MSE", round(min(oob), 3), "\n")

## ---- B. the chosen forest vs the default, on the test set (touched once) -------------
f_def <- ranger(y ~ ., dtr, num.trees = 1000, seed = 723)
f_tun <- ranger(y ~ ., dtr, num.trees = 1000, mtry = mb, min.node.size = nb, seed = 723)
f_bag <- ranger(y ~ ., dtr, num.trees = 1000, mtry = p, seed = 723)
cat("\nB. test R2: default", round(r2(predict(f_def, dte)$predictions), 3),
    "; tuned", round(r2(predict(f_tun, dte)$predictions), 3),
    "; bagging (mtry = p)", round(r2(predict(f_bag, dte)$predictions), 3), "\n")
cat("OOB MSE: default", round(f_def$prediction.error, 3), "; tuned", round(f_tun$prediction.error, 3), "\n")

## ---- C. number of trees: OOB MSE flattens, never turns up ----------------------------
cat("\nC. OOB MSE by number of trees (default mtry, node size)\n")
for (B in c(10, 50, 100, 300, 1000, 3000))
  cat(sprintf("  %4d trees: OOB MSE %.3f\n", B, ranger(y ~ ., dtr, num.trees = B, seed = 723)$prediction.error))
