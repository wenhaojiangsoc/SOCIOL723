## Week 13 -- the growth example, stacked learners with ddml (short-stacking, nnls weights).
## Same data as figs/w13_growth.R: 60 controls and their squares (120 controls, 90 countries).
##   Rscript figs/w13_growth_stack.R > figs/w13_growth_stack.out
set.seed(723)
suppressMessages({library(hdm); library(ddml)})
data(GrowthData, package = "hdm")
g <- GrowthData[, names(GrowthData) != "intercept"]
X0 <- as.matrix(g[, setdiff(names(g), c("Outcome", "gdpsh465"))])
X <- cbind(X0, X0^2); colnames(X) <- c(colnames(X0), paste0(colnames(X0), "_sq"))
learners <- list(list(fun = mdl_glmnet, args = list(alpha = 1)),
                 list(fun = mdl_glmnet, args = list(alpha = 0)),
                 list(fun = mdl_ranger, args = list(num.trees = 500)),
                 list(fun = mdl_xgboost, args = list(nrounds = 200, learning_rate = 0.05, max_depth = 2)))
fit <- ddml_plm(y = g$Outcome, D = g$gdpsh465, X = X, learners = learners, sample_folds = 5,
                ensemble_type = "nnls", shortstack = TRUE, silent = TRUE)
w <- round(cbind(Y = fit$weights$y_X[, 1], D = fit$weights$D1_X[, 1]), 2)
rownames(w) <- c("lasso", "ridge", "forest", "boosting"); print(w)
s <- summary(fit); print(round(s[, , 1], 4))
