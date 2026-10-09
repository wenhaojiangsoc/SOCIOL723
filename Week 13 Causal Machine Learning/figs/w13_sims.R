## Week 13 -- why DML needs both ingredients, and when it still fails.
## Partially linear model Y = theta D + g(W) + e, D = m(W) + v, theta = 0.5, 300 draws per design.
##   Design A (Chernozhukov et al. 2018, Fig. 1): n = 500, p = 20, W ~ N(0, Sigma), Sigma_jk = 0.7^|j-k|,
##     m(W) = W1 + 0.25 logistic(W3), g(W) = logistic(W1) + 0.25 W3  (smooth, nearly linear nuisances)
##   Design B: n = 1000, p = 10, W ~ N(0, I), strongly nonlinear g and m
##   Estimators: OLS on (D, W); naive plug-in (forest g from Y ~ (D, W), then Y - g_hat on raw D);
##     partialling out with in-sample forests; DML (partialling out, 5-fold cross-fitting)
##   Rscript figs/w13_sims.R > figs/w13_sims.out
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
suppressMessages(library(ranger))
theta <- 0.5; R <- 300
rf <- function(y, X, Xnew = X) predict(ranger(y = y, x = as.data.frame(X), num.trees = 300, min.node.size = 5,
                                             num.threads = 4), as.data.frame(Xnew))$predictions
one <- function(n, drawW, g, m) {
  W <- drawW(n); D <- m(W) + rnorm(n); Y <- theta * D + g(W) + rnorm(n)
  colnames(W) <- paste0("w", seq_len(ncol(W)))
  fit <- ranger(y = Y, x = data.frame(D = D, W), num.trees = 300, min.node.size = 5, num.threads = 4)
  a <- sum(D * (Y - predict(fit, data.frame(D = 0, W))$predictions)) / sum(D^2)
  lh <- rf(Y, W); mh <- rf(D, W); dt <- D - mh; yt <- Y - lh
  b <- sum(dt * yt) / sum(dt^2); seb <- sqrt(sum(dt^2 * (yt - b * dt)^2)) / sum(dt^2)
  f <- sample(rep(1:5, length.out = n)); lc <- mc <- numeric(n)
  for (k in 1:5) { tr <- f != k; lc[!tr] <- rf(Y[tr], W[tr, ], W[!tr, ]); mc[!tr] <- rf(D[tr], W[tr, ], W[!tr, ]) }
  dc <- D - mc; yc <- Y - lc
  cc <- sum(dc * yc) / sum(dc^2); sec <- sqrt(sum(dc^2 * (yc - cc * dc)^2)) / sum(dc^2)
  c(ols = coef(lm(Y ~ D + W))[["D"]], naive = a, insample = b, cover_in = abs(b - theta) < 1.96 * seb,
    dml = cc, cover_dml = abs(cc - theta) < 1.96 * sec, se_dml = sec,
    rmse_m = sqrt(mean((mc - m(W))^2)), rmse_l = sqrt(mean((lc - (theta * m(W) + g(W)))^2)))
}
report <- function(res, label) {
  cat("\n##", label, "\n")
  for (v in c("ols", "naive", "insample", "dml"))
    cat(sprintf("%-9s mean %.3f  bias %+.3f  sd %.3f  bias/sd %+.2f\n", v, mean(res[, v]), mean(res[, v]) - theta, sd(res[, v]),
                (mean(res[, v]) - theta) / sd(res[, v])))
  cat("coverage of 95% interval: in-sample", round(mean(res[, "cover_in"]), 3), "; DML", round(mean(res[, "cover_dml"]), 3), "\n")
  cat("mean DML se:", round(mean(res[, "se_dml"]), 3), "; sd of DML estimates:", round(sd(res[, "dml"]), 3), "\n")
  cat("cross-fitted RMSE of m-hat:", round(mean(res[, "rmse_m"]), 3), "; of l-hat:", round(mean(res[, "rmse_l"]), 3), "\n")
}
pA <- 20; ChA <- chol(0.7^abs(outer(1:pA, 1:pA, "-")))
resA <- t(replicate(R, one(500, function(n) matrix(rnorm(n * pA), n, pA) %*% ChA,
                           function(W) plogis(W[, 1]) + 0.25 * W[, 3], function(W) W[, 1] + 0.25 * plogis(W[, 3]))))
report(resA, "Design A: smooth nuisances, n = 500, p = 20")
resB <- t(replicate(R, one(1000, function(n) matrix(rnorm(n * 10), n, 10),
                           function(W) 2 * sin(W[, 1]) + W[, 2]^2 + 0.5 * W[, 3] * W[, 4],
                           function(W) 1.5 * tanh(W[, 1]) + 0.5 * W[, 2]^2 - 0.5)))
report(resB, "Design B: strongly nonlinear nuisances, n = 1000, p = 10")
