## Week 12 -- variable importance for the GSS random forest (the benchmark forest of figs/w12_gss.R:
## same data, same 70/30 split, same ranger call). Every number on the importance frames comes from here.
##   Rscript figs/w12_importance.R > figs/w12_importance.out   (run from the week folder)
## writes figs/rf_importance.pdf
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

## ---- A. the forest, with both importance measures --------------------------------
rf_p <- ranger(y ~ ., dtr, num.trees = 1000, importance = "permutation", seed = 723)
rf_i <- ranger(y ~ ., dtr, num.trees = 1000, importance = "impurity", seed = 723)
mse <- function(p) mean((y[te] - p)^2)
base <- mse(predict(rf_p, dte)$predictions)
cat("test MSE of the forest:", round(base, 3), "; same forest grown with impurity importance:",
    round(mse(predict(rf_i, dte)$predictions), 3), "\n")

## ---- B. permutation importance by hand, on the test set ------------------------------
## shuffle one column (or several together) in the test data, predict again, record the rise in MSE
perm <- function(vars, S = 20) {
  mean(replicate(S, { z <- dte; idx <- sample(nrow(z)); z[vars] <- z[idx, vars, drop = FALSE]
    mse(predict(rf_p, z)$predictions) })) - base
}
set.seed(1)
cat("\nB. rise in test MSE when shuffled (20 shuffles each):\n")
hand <- sapply(names(dte), perm)
print(round(sort(hand, decreasing = TRUE)[1:8], 3))
top1 <- names(sort(hand, decreasing = TRUE))[1]
cat("one shuffle of the top variable (", top1, "), worked: test MSE", round(base, 3), "->", round(base + hand[[top1]], 3), "\n")
for (pr in list(c("educ", "wordsum"), c("educ", "pareduc"), c("age", "evermar"), c("age", "childs"))) {
  cat(pr, ": alone", round(hand[[pr[1]]], 3), round(hand[[pr[2]]], 3), "; sum", round(hand[[pr[1]]] + hand[[pr[2]]], 3),
      "; together", round(perm(pr), 3), "; corr", round(cor(d[[pr[1]]], d[[pr[2]]]), 2), "\n")
}

## ---- C. ranger's measures (permutation: out-of-bag; impurity: SSE reduction) ------
ip <- rf_p$variable.importance; ii <- rf_i$variable.importance
cat("\nC. ranger permutation importance (OOB rise in MSE), top 8:\n"); print(round(sort(ip, decreasing = TRUE)[1:8], 4))
cat("ranger impurity importance (SSE reduction per tree), top 8:\n"); print(round(sort(ii, decreasing = TRUE)[1:8], 1))
cat("impurity share of total, top 8:\n"); print(round(sort(ii / sum(ii), decreasing = TRUE)[1:8], 3))

## ---- D. figure: the two measures, top 10 by permutation ------------------------------
lab <- c(educ = "education", age = "age", female = "female",
         hours = "hours worked", evermar = "ever married", wordsum = "vocabulary", pareduc = "parents' educ.",
         childs = "children", black = "Black", otherace = "other race", yr2022 = "year 2022")
top <- names(sort(ip, decreasing = TRUE))[1:10]
nm <- ifelse(top %in% names(lab), lab[top], top)
pdf("figs/rf_importance.pdf", width = 7.2, height = 3.0, family = "Times")
par(mfrow = c(1, 2), mar = c(3.3, 7.2, 1.6, 0.8), mgp = c(2.0, 0.6, 0))
v1 <- rev(ip[top]); v2 <- rev((ii / sum(ii))[top])
plot(v1, 1:10, pch = 19, col = "navy", yaxt = "n", ylab = "", xlim = c(0, max(v1) * 1.05), las = 1,
     xlab = "rise in MSE when shuffled", main = "permutation (out-of-bag)", font.main = 1, cex.main = 1)
segments(0, 1:10, v1, 1:10, col = "navy", lwd = 2); axis(2, 1:10, rev(nm), las = 1, cex.axis = 0.85)
plot(v2, 1:10, pch = 19, col = "firebrick", yaxt = "n", ylab = "", xlim = c(0, max(v2) * 1.05), las = 1,
     xlab = "share of all SSE reduction", main = "impurity (split gains)", font.main = 1, cex.main = 1)
segments(0, 1:10, v2, 1:10, col = "firebrick", lwd = 2); axis(2, 1:10, rev(nm), las = 1, cex.axis = 0.85)
dev.off()
cat("female: permutation rank", rank(-ip)[["female"]], "; impurity rank", rank(-ii)[["female"]],
    "; impurity share", round(ii[["female"]] / sum(ii), 3), "\n")
cat("distinct values in training data: age", length(unique(dtr$age)), "; hours", length(unique(dtr$hours)),
    "; female", length(unique(dtr$female)), "\n")
