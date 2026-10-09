## Week 12 -- prediction on the course's GSS extract: log earnings, n = 3,509.
## Every number on the 'GSS' frames comes from here.
##   Rscript figs/w12_gss.R > figs/w12_gss.out ; writes figs/lasso_path.pdf, figs/optimism.pdf
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
for (p in c("glmnet", "ranger", "xgboost"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
suppressMessages({library(glmnet); library(ranger); library(rpart); library(xgboost)})
d <- readRDS("../Data/gss_earnings.rds")
d <- d[complete.cases(d[, c("lnearn", "educ", "age", "female", "black", "otherace", "wordsum", "pareduc",
                            "prestige", "sei", "hours", "childs", "evermar", "region", "year")]), ]
d$region <- factor(d$region); d$yr <- factor(d$year)
cat("n complete:", nrow(d), "; sd(lnearn):", round(sd(d$lnearn), 3), "\n")

## main-effects design and an expanded design (all pairwise interactions + squares)
f_main <- lnearn ~ educ + age + female + black + otherace + wordsum + pareduc + prestige + sei + hours +
  childs + evermar + region + yr
num <- c("educ", "age", "wordsum", "pareduc", "prestige", "sei", "hours", "childs")
f_big <- as.formula(paste("lnearn ~ (educ + age + female + black + otherace + wordsum + pareduc + prestige + sei + hours + childs + evermar + region + yr)^2 +",
                          paste0("I(", num, "^2)", collapse = " + ")))
Xm <- model.matrix(f_main, d)[, -1]; Xb <- model.matrix(f_big, d)[, -1]
Xb <- Xb[, apply(Xb, 2, var) > 0]; Xb <- Xb[, !duplicated(t(Xb))]
cat("predictors: main", ncol(Xm), "; expanded", ncol(Xb), "\n")
y <- d$lnearn; n <- nrow(d)
te <- sample(n, round(0.3 * n)); tr <- setdiff(1:n, te)
mse <- function(p) mean((y[te] - p)^2); r2 <- function(p) 1 - mse(p) / mean((y[te] - mean(y[tr]))^2)

fit_all <- function(trn, label) {
  out <- list()
  out$mean <- rep(mean(y[trn]), length(te))
  out$OLS_main <- as.vector(cbind(1, Xm[te, ]) %*% coef(lm.fit(cbind(1, Xm[trn, ]), y[trn])))
  bo <- coef(lm.fit(cbind(1, Xb[trn, ]), y[trn])); bo[is.na(bo)] <- 0
  out$OLS_expanded <- as.vector(cbind(1, Xb[te, ]) %*% bo)
  cr <- cv.glmnet(Xb[trn, ], y[trn], alpha = 0, nfolds = 10); out$ridge <- as.vector(predict(cr, Xb[te, ], s = "lambda.min"))
  cl <- cv.glmnet(Xb[trn, ], y[trn], alpha = 1, nfolds = 10); out$lasso <- as.vector(predict(cl, Xb[te, ], s = "lambda.min"))
  out$lasso_1se <- as.vector(predict(cl, Xb[te, ], s = "lambda.1se"))
  ce <- cv.glmnet(Xb[trn, ], y[trn], alpha = 0.5, nfolds = 10); out$elastic_net <- as.vector(predict(ce, Xb[te, ], s = "lambda.min"))
  dtr <- data.frame(y = y[trn], Xm[trn, ]); dte <- data.frame(Xm[te, ])
  tt <- rpart(y ~ ., dtr, cp = 0.001); cpb <- tt$cptable[which.min(tt$cptable[, "xerror"]), "CP"]
  out$tree <- predict(prune(tt, cp = cpb), dte)
  rf <- ranger(y ~ ., dtr, num.trees = 1000, importance = "permutation", seed = 723); out$random_forest <- predict(rf, dte)$predictions
  dm <- xgb.DMatrix(Xm[trn, ], label = y[trn])
  cvb <- xgb.cv(params = list(eta = 0.05, max_depth = 3, subsample = 0.8, objective = "reg:squarederror"), data = dm,
                nrounds = 2000, nfold = 5, early_stopping_rounds = 50, verbose = 0)
  bst <- xgb.train(params = list(eta = 0.05, max_depth = 3, subsample = 0.8, objective = "reg:squarederror"), data = dm,
                   nrounds = which.min(cvb$evaluation_log[[4]]), verbose = 0)   # column 4 = test rmse mean
  out$boosting <- predict(bst, Xm[te, ])
  ## single-hidden-layer neural network (nnet): standardized inputs, size and weight decay by 5-fold CV
  mu <- colMeans(Xm[trn, ]); sdv <- apply(Xm[trn, ], 2, sd); sdv[sdv == 0] <- 1
  Ztr <- scale(Xm[trn, ], mu, sdv); Zte <- scale(Xm[te, ], mu, sdv); ym <- mean(y[trn]); ys <- sd(y[trn])
  grid <- expand.grid(size = c(2, 5, 10, 20), decay = c(0.01, 0.1, 1))
  fold <- sample(rep(1:5, length.out = length(trn)))
  cvm <- apply(grid, 1, function(g) mean(sapply(1:5, function(k) {
    m <- nnet::nnet(Ztr[fold != k, ], (y[trn][fold != k] - ym) / ys, size = g[1], decay = g[2], linout = TRUE, maxit = 500, trace = FALSE, MaxNWts = 5000)
    mean((y[trn][fold == k] - (ym + ys * predict(m, Ztr[fold == k, ])))^2) })))
  gb <- grid[which.min(cvm), ]
  nn <- replicate(5, { m <- nnet::nnet(Ztr, (y[trn] - ym) / ys, size = gb$size, decay = gb$decay, linout = TRUE, maxit = 500, trace = FALSE, MaxNWts = 5000)
    ym + ys * as.vector(predict(m, Zte)) })
  out$neural_net <- rowMeans(nn)                      # average of 5 random starts
  cat("neural net chosen: size", gb$size, "decay", gb$decay, "; parameters", (ncol(Xm) + 1) * gb$size + gb$size + 1, "\n")
  res <- t(sapply(out, function(p) c(test_MSE = mse(p), test_R2 = r2(p))))
  cat("\n##", label, "(training n =", length(trn), ")\n"); print(round(res, 3))
  list(cl = cl, cr = cr, rf = rf, nzero = cl$nzero[cl$lambda == cl$lambda.min], nzero1 = cl$nzero[cl$lambda == cl$lambda.1se], bst = bst)
}
full <- fit_all(tr, "1. full training sample")
cat("lasso nonzero at lambda.min:", full$nzero, "; at 1se:", full$nzero1, "of", ncol(Xb), "\n")
cat("random forest OOB MSE:", round(full$rf$prediction.error, 3), "\n")
imp <- sort(full$rf$variable.importance, decreasing = TRUE)
cat("permutation importance (top 6):\n"); print(round(head(imp, 6), 4))
small <- fit_all(sample(tr, 300), "2. small training sample")

## ---- 3. optimism: training vs test error as the expanded design grows ----------------
cat("\n## 3. OLS training vs test MSE as p grows (columns of the expanded design, in order)\n")
ps <- c(5, 10, 20, 40, 80, 120, 160, 200, 240)
ps <- ps[ps <= ncol(Xb)]
trs <- sample(tr, 600)
op <- t(sapply(ps, function(k) {
  X1 <- cbind(1, Xb[trs, 1:k]); b <- coef(lm.fit(X1, y[trs])); b[is.na(b)] <- 0
  c(p = k, train = mean((y[trs] - X1 %*% b)^2), test = mean((y[te] - cbind(1, Xb[te, 1:k]) %*% b)^2))
}))
s2 <- mean((y[trs] - cbind(1, Xb[trs, 1:20]) %*% coef(lm.fit(cbind(1, Xb[trs, 1:20]), y[trs])))^2) * 600 / (600 - 21)
op <- cbind(op, gap = op[, "test"] - op[, "train"], theory_2sig2p_n = 2 * s2 * (op[, "p"] + 1) / 600)
print(round(op, 3))
pdf("figs/optimism.pdf", width = 9, height = 3.3, family = "Times")
par(mar = c(4, 4.2, 1, 1))
plot(op[, "p"], op[, "test"], type = "b", pch = 19, col = "firebrick", lwd = 2, ylim = c(0, max(op[, "test"])),
     xlab = "number of predictors (training n = 600)", ylab = "mean squared error")
lines(op[, "p"], op[, "train"], type = "b", pch = 17, col = "navy", lwd = 2)
legend("topleft", c("test", "training"), col = c("firebrick", "navy"), pch = c(19, 17), lwd = 2, bty = "n")
dev.off()

## ---- 4. lasso path, and instability of the selected set ------------------------------
pdf("figs/lasso_path.pdf", width = 9, height = 3.3, family = "Times")
par(mfrow = c(1, 2), mar = c(4, 4.2, 2.4, 1))
plot(full$cl$glmnet.fit, xvar = "lambda", label = FALSE); title("coefficient paths", line = 1.2, cex.main = 0.95, font.main = 1)
plot(full$cl); title("10-fold CV error", line = 1.2, cex.main = 0.95, font.main = 1)
dev.off()
cat("\n## 4. lasso selection frequency over 100 bootstrap samples (main-effects design, lambda.1se)\n")
sel <- replicate(100, { b <- sample(tr, replace = TRUE); cl <- cv.glmnet(Xm[b, ], y[b], alpha = 1, nfolds = 5)
  as.vector(coef(cl, s = "lambda.1se"))[-1] != 0 })
fr <- rowMeans(sel); names(fr) <- colnames(Xm)
print(round(sort(fr, decreasing = TRUE)[1:16], 2))
cat("selection frequency: prestige", fr[["prestige"]], "; sei", fr[["sei"]], "\n")
cat("correlation prestige-sei:", round(cor(d$prestige, d$sei), 2), "; educ-wordsum:", round(cor(d$educ, d$wordsum), 2), "\n")
