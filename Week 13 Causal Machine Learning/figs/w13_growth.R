## Week 13 -- DML example: the convergence hypothesis (Barro and Lee 1994; hdm::GrowthData).
## 90 countries; Y = growth of GDP per capita; D = log GDP per capita at the start (gdpsh465);
## X = 60 country characteristics (schooling, trade, investment, government, politics, ...)
##     AND their squares: 120 controls, more than the 90 countries, so OLS cannot be estimated.
## Convergence: poor countries grow faster given X, i.e. tau < 0.
##   Rscript figs/w13_growth.R > figs/w13_growth.out ; writes figs/growth_learners.pdf
set.seed(723)
suppressMessages({library(hdm); library(DoubleML); library(mlr3); library(mlr3learners); library(data.table)})
lgr::get_logger("mlr3")$set_threshold("warn")
data(GrowthData, package = "hdm")
g0 <- GrowthData[, names(GrowthData) != "intercept"]
X0 <- setdiff(names(g0), c("Outcome", "gdpsh465"))
sq <- as.data.frame(g0[X0]^2); names(sq) <- paste0(X0, "_sq")      # all 60 are continuous
g <- cbind(g0, sq)
X <- c(X0, names(sq))
cat("n =", nrow(g), "; original controls:", length(X0), "; with squares p =", length(X), "\n")

## ---- 1. OLS: 60 controls is barely possible, 120 is not ----
cat("\n## 1. no controls:", round(coef(lm(Outcome ~ gdpsh465, g))[2], 4), "\n")
o60 <- lm(Outcome ~ ., g0); b <- coef(o60)["gdpsh465"]; s <- sqrt(sandwich::vcovHC(o60, "HC1")["gdpsh465", "gdpsh465"])
cat("    OLS, the 60 original controls:", round(b, 4), "(", round(s, 4), "), residual df", o60$df.residual, "\n")
o120 <- lm(Outcome ~ ., g)
cat("    OLS, 120 controls: coefficients to estimate", length(coef(o120)), "; rank", o120$rank,
    "; residual df", o120$df.residual, "; coefficients R could not estimate (NA):", sum(is.na(coef(o120))),
    "; largest residual", signif(max(abs(resid(o120))), 2), "\n")

## ---- 2. DML, partially linear, four learners, 5 folds x 20 repetitions ----
dat <- DoubleMLData$new(as.data.table(g), y_col = "Outcome", d_cols = "gdpsh465", x_cols = X)
learners <- list(
  "lasso"    = lrn("regr.cv_glmnet", alpha = 1, s = "lambda.min"),
  "ridge"    = lrn("regr.cv_glmnet", alpha = 0, s = "lambda.min"),
  "forest"   = lrn("regr.ranger", num.trees = 500),
  "boosting" = lrn("regr.xgboost", nrounds = 200, eta = 0.05, max_depth = 2))
R <- 20
r2 <- function(y, p) 1 - mean((y - p)^2) / var(y)        # out-of-fold R2: 0 = no better than the mean
fits <- lapply(names(learners), function(k) {
  set.seed(723)
  m <- DoubleMLPLR$new(dat, ml_l = learners[[k]]$clone(), ml_m = learners[[k]]$clone(), n_folds = 5, n_rep = R)
  m$fit(store_predictions = TRUE)
  c(R2_Y = mean(sapply(1:R, function(r) r2(g$Outcome, m$predictions$ml_l[, r, 1]))),
    R2_D = mean(sapply(1:R, function(r) r2(g$gdpsh465, m$predictions$ml_m[, r, 1]))),
    est = m$coef[[1]], se = m$se[[1]])
})
tab <- do.call(rbind, fits); rownames(tab) <- names(learners)
cat("\n## 2. out-of-fold R2 (mean over", R, "repetitions) and the DML estimate (median over repetitions)\n")
print(round(tab, 4))

## ---- 3. pick the best learner for each nuisance separately ----
bY <- rownames(tab)[which.max(tab[, "R2_Y"])]; bD <- rownames(tab)[which.max(tab[, "R2_D"])]
set.seed(723)
mb <- DoubleMLPLR$new(dat, ml_l = learners[[bY]]$clone(), ml_m = learners[[bD]]$clone(), n_folds = 5, n_rep = R); mb$fit()
cat("\n## 3. best for Y:", bY, "; best for D:", bD, "->", round(mb$coef, 4), "(", round(mb$se, 4), ") 95% CI [",
    round(mb$coef - 1.96 * mb$se, 4), ",", round(mb$coef + 1.96 * mb$se, 4), "]\n")
cat("    range of the estimate across the", R, "repetitions (best combination): min", round(min(mb$all_coef), 4), "max", round(max(mb$all_coef), 4), "\n")

## ---- 4. figure: prediction quality and the estimate, by learner ----
lab <- rownames(tab); y <- length(lab):1
pdf("figs/growth_learners.pdf", width = 7.2, height = 2.7, family = "Times")
par(mfrow = c(1, 3), mar = c(3.6, 5.2, 2.2, 0.8), mgp = c(2.3, 0.7, 0), cex.main = 1.25, font.main = 1,
    cex.axis = 1.2, cex.lab = 1.25, las = 1)
for (v in c("R2_Y", "R2_D")) {
  best <- if (v == "R2_Y") bY else bD
  plot(tab[, v], y, xlim = c(min(-0.3, tab[, v]) - 0.05, 1), ylim = c(0.4, length(lab) + 0.6), yaxt = "n", type = "n",
       xlab = "out-of-fold R-squared", ylab = "", main = if (v == "R2_Y") "predicting growth Y" else "predicting initial GDP D")
  abline(h = y, col = "gray90"); abline(v = 0, lty = 3)
  points(tab[, v], y, pch = 19, cex = 1.8, col = ifelse(lab == best, "#C84E00", "#012169"))
  axis(2, at = y, labels = lab, tick = FALSE, cex.axis = 1.3)
}
ci <- cbind(tab[, "est"] - 1.96 * tab[, "se"], tab[, "est"] + 1.96 * tab[, "se"])
plot(tab[, "est"], y, xlim = range(c(ci, 0)) + c(-0.005, 0.005), ylim = c(0.4, length(lab) + 0.6), yaxt = "n", type = "n",
     xlab = "estimate of tau", ylab = "", main = "DML estimate, 95% interval")
abline(h = y, col = "gray90"); abline(v = 0, lty = 3)
segments(ci[, 1], y, ci[, 2], y, col = "#012169", lwd = 2.5)
points(tab[, "est"], y, pch = 19, cex = 1.6, col = "#012169")
axis(2, at = y, labels = lab, tick = FALSE, cex.axis = 1.3)
dev.off()
