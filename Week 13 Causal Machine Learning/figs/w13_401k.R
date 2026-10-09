## Week 13 -- causal ML on the 401(k) data (Chernozhukov et al. 2018; hdm::pension, SIPP 1991, n = 9,915).
## Effect of 401(k) ELIGIBILITY (e401) on net financial assets (net_tfa, dollars).
##   Rscript figs/w13_401k.R > figs/w13_401k.out ; writes figs/k401_gates.pdf
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
for (p in c("hdm", "DoubleML", "mlr3", "mlr3learners", "grf", "ranger", "glmnet", "xgboost"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
suppressMessages({library(hdm); library(DoubleML); library(mlr3); library(mlr3learners); library(grf); library(data.table)})
lgr::get_logger("mlr3")$set_threshold("warn")
data(pension, package = "hdm")
d <- pension
X <- c("age", "inc", "educ", "fsize", "marr", "twoearn", "db", "pira", "hown")
cat("n =", nrow(d), "; eligible:", round(mean(d$e401), 3), "; participate:", round(mean(d$p401), 3), "\n")
cat("mean net_tfa: eligible", round(mean(d$net_tfa[d$e401 == 1])), "; not", round(mean(d$net_tfa[d$e401 == 0])), "\n")

## ---- 1. benchmarks ----------------------------------------------------------------------
r0 <- lm(net_tfa ~ e401, d); r1 <- lm(as.formula(paste("net_tfa ~ e401 +", paste(X, collapse = "+"))), d)
rob <- function(f) sqrt(sandwich::vcovHC(f, "HC1")["e401", "e401"])
cat("\n## 1. difference in means:", round(coef(r0)[2]), "(", round(rob(r0)), ")",
    "; OLS with linear controls:", round(coef(r1)[2]), "(", round(rob(r1)), ")\n")

## flexible dictionary: polynomials in age, income, education, family size + interactions with binaries
dict <- model.matrix(~ (poly(age, 4, raw = TRUE) + poly(inc, 4, raw = TRUE) + poly(educ, 2, raw = TRUE) + poly(fsize, 2, raw = TRUE) +
                        marr + twoearn + db + pira + hown)^2, d)[, -1]
dict <- scale(dict); dict <- dict[, colSums(is.na(dict)) == 0]
cat("dictionary columns:", ncol(dict), "\n")

## ---- 2. single selection vs double selection (hdm) ----------------------------------------
cat("\n## 2. lasso selection on the", ncol(dict), "-column dictionary\n")
ysel <- rlasso(dict, d$net_tfa); dsel <- rlasso(dict, d$e401)
sY <- which(coef(ysel)[-1] != 0); sD <- which(coef(dsel)[-1] != 0)
cat("selected by Y-lasso:", length(sY), "; by D-lasso:", length(sD), "; union:", length(union(sY, sD)), "\n")
naive <- lm(d$net_tfa ~ d$e401 + dict[, sY, drop = FALSE])
cat("single selection (controls chosen to predict Y):", round(coef(naive)[2]), "\n")
pds <- rlassoEffect(dict, d$net_tfa, d$e401, method = "double selection")
cat("double selection:", round(pds$alpha), "(", round(pds$se), ")\n")

## ---- 3. DML, partially linear and interactive -------------------------------------------
cat("\n## 3. DoubleML, 5 folds, 5 repetitions (median)\n")
dt <- as.data.table(d[, c("net_tfa", "e401", X)])
dml_data <- DoubleMLData$new(dt, y_col = "net_tfa", d_cols = "e401", x_cols = X)
dml_dict <- DoubleMLData$new(data.table(net_tfa = d$net_tfa, e401 = d$e401, dict), y_col = "net_tfa", d_cols = "e401")
lrn_rf <- lrn("regr.ranger", num.trees = 500, min.node.size = 5); cls_rf <- lrn("classif.ranger", num.trees = 500, min.node.size = 5)
lrn_las <- lrn("regr.cv_glmnet", s = "lambda.min"); cls_las <- lrn("classif.cv_glmnet", s = "lambda.min")
lrn_xg <- lrn("regr.xgboost", nrounds = 300, eta = 0.05, max_depth = 3); cls_xg <- lrn("classif.xgboost", nrounds = 300, eta = 0.05, max_depth = 3)
run <- function(obj) { obj$fit(); c(est = obj$coef[[1]], se = obj$se[[1]]) }
out <- rbind(
  PLR_lasso = run(DoubleMLPLR$new(dml_dict, ml_l = lrn_las, ml_m = lrn_las, n_folds = 5, n_rep = 5)),
  PLR_forest = run(DoubleMLPLR$new(dml_data, ml_l = lrn_rf, ml_m = lrn_rf, n_folds = 5, n_rep = 5)),
  PLR_boosting = run(DoubleMLPLR$new(dml_data, ml_l = lrn_xg, ml_m = lrn_xg, n_folds = 5, n_rep = 5)),
  IRM_lasso = run(DoubleMLIRM$new(dml_dict, ml_g = lrn_las, ml_m = cls_las, n_folds = 5, n_rep = 5, trimming_threshold = 0.01)),
  IRM_forest = run(DoubleMLIRM$new(dml_data, ml_g = lrn_rf, ml_m = cls_rf, n_folds = 5, n_rep = 5, trimming_threshold = 0.01)),
  IRM_boosting = run(DoubleMLIRM$new(dml_data, ml_g = lrn_xg, ml_m = cls_xg, n_folds = 5, n_rep = 5, trimming_threshold = 0.01)))
print(round(out))

## ---- 4. causal forest: heterogeneity -----------------------------------------------------
cat("\n## 4. causal forest (grf)\n")
Xm <- as.matrix(d[, X])
cf <- causal_forest(Xm, d$net_tfa, d$e401, num.trees = 4000, seed = 723)
ate <- average_treatment_effect(cf); cat("AIPW ATE:", round(ate[1]), "(", round(ate[2]), ")\n")
print(test_calibration(cf))
blp <- best_linear_projection(cf, Xm[, c("inc", "age", "educ")]); print(blp)
sc <- get_scores(cf)                                    # doubly robust scores Gamma_i
q <- cut(d$inc, quantile(d$inc, 0:5 / 5), include.lowest = TRUE, labels = 1:5)
gates <- t(sapply(1:5, function(k) { s <- sc[q == k]; c(quintile = k, mean_income = mean(d$inc[q == k]), est = mean(s), se = sd(s) / sqrt(length(s))) }))
cat("GATES by income quintile (DR scores):\n"); print(round(gates))
tau <- predict(cf)$predictions                          # out-of-bag CATEs
qt <- cut(tau, quantile(tau, 0:5 / 5), include.lowest = TRUE, labels = 1:5)
sg <- t(sapply(1:5, function(k) { s <- sc[qt == k]; c(group = k, est = mean(s), se = sd(s) / sqrt(length(s))) }))
cat("sorted GATES by quintile of out-of-bag tau-hat:\n"); print(round(sg))
vi <- variable_importance(cf); names(vi) <- X; cat("split-frequency importance:\n"); print(round(sort(vi[, 1], decreasing = TRUE), 3))
## RATE on a held-out half
tr <- sample(nrow(d), nrow(d) / 2)
cf_tr <- causal_forest(Xm[tr, ], d$net_tfa[tr], d$e401[tr], num.trees = 2000, seed = 723)
cf_te <- causal_forest(Xm[-tr, ], d$net_tfa[-tr], d$e401[-tr], num.trees = 2000, seed = 723)
pr <- predict(cf_tr, Xm[-tr, ])$predictions
ra <- rank_average_treatment_effect(cf_te, pr, target = "AUTOC")
cat("RATE (AUTOC) on held-out half, prioritizing by the training-half CATE:", round(ra$estimate), "(", round(ra$std.err), ")\n")
ra2 <- rank_average_treatment_effect(cf_te, Xm[-tr, "inc"], target = "AUTOC")
cat("RATE (AUTOC), prioritizing by income:", round(ra2$estimate), "(", round(ra2$std.err), ")\n")

pdf("figs/k401_gates.pdf", width = 9, height = 3.2, family = "Times")
par(mfrow = c(1, 2), mar = c(4, 4.6, 1.8, 1))
plot(gates[, 1], gates[, 3], pch = 19, col = "navy", ylim = range(c(gates[, 3] - 2 * gates[, 4], gates[, 3] + 2 * gates[, 4], 0)),
     xlab = "income quintile", ylab = "effect of eligibility ($)", main = "by income (pre-specified)", cex.main = 0.95, font.main = 1)
segments(gates[, 1], gates[, 3] - 1.96 * gates[, 4], gates[, 1], gates[, 3] + 1.96 * gates[, 4], col = "navy"); abline(h = 0, lty = 3)
plot(sg[, 1], sg[, 2], pch = 19, col = "firebrick", ylim = range(c(sg[, 2] - 2 * sg[, 3], sg[, 2] + 2 * sg[, 3], 0)),
     xlab = "quintile of predicted effect", ylab = "effect of eligibility ($)", main = "sorted by the forest's CATE", cex.main = 0.95, font.main = 1)
segments(sg[, 1], sg[, 2] - 1.96 * sg[, 3], sg[, 1], sg[, 2] + 1.96 * sg[, 3], col = "firebrick"); abline(h = 0, lty = 3)
dev.off()
