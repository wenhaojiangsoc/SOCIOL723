## Week 12 -- evaluating a classifier: confusion matrix, accuracy, precision, recall, F1, AUC.
## Outcome: top 10% of log earnings (cut set on the training set). Same GSS data and 70/30 split as figs/w12_gss.R.
##   Rscript figs/w12_classify.R > figs/w12_classify.out   (run from the week folder)
set.seed(723)
suppressMessages(library(ranger))
d <- readRDS("../Data/gss_earnings.rds")
d <- d[complete.cases(d[, c("lnearn", "educ", "age", "female", "black", "otherace", "wordsum", "pareduc",
                            "hours", "childs", "evermar", "region", "year")]), ]
d$region <- factor(d$region); d$yr <- factor(d$year)
f_main <- lnearn ~ educ + age + female + black + otherace + wordsum + pareduc + hours +
  childs + evermar + region + yr
Xm <- model.matrix(f_main, d)[, -1]; n <- nrow(d)
te <- sample(n, round(0.3 * n)); tr <- setdiff(1:n, te)      # identical to w12_gss.R
cut <- quantile(d$lnearn[tr], 0.9)
yb <- as.integer(d$lnearn >= cut)
cat("top-10% cut (training):", round(cut, 2), "log points = $", round(exp(cut)), "; share of 1s: training",
    round(mean(yb[tr]), 3), "test", round(mean(yb[te]), 3), "; test n", length(te), "\n")
dtr <- data.frame(y = yb[tr], Xm[tr, ]); dte <- data.frame(Xm[te, ])
lg <- glm(y ~ ., binomial, dtr); p_lg <- predict(lg, dte, type = "response")
rf <- ranger(factor(y) ~ ., dtr, num.trees = 1000, probability = TRUE, seed = 723)
p_rf <- predict(rf, dte)$predictions[, "1"]
yt <- yb[te]
auc <- function(p) { r <- rank(p); n1 <- sum(yt); n0 <- length(yt) - n1; (sum(r[yt == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
metr <- function(p, c) {
  yh <- as.integer(p > c)
  TP <- sum(yh == 1 & yt == 1); FP <- sum(yh == 1 & yt == 0); FN <- sum(yh == 0 & yt == 1); TN <- sum(yh == 0 & yt == 0)
  prec <- if (TP + FP > 0) TP / (TP + FP) else NA; rec <- TP / (TP + FN)
  c(cutoff = c, TP = TP, FP = FP, FN = FN, TN = TN, accuracy = (TP + TN) / length(yt), precision = prec,
    recall = rec, F1 = 2 * prec * rec / (prec + rec))
}
cat("\nbaseline 'predict 0 for everyone': accuracy", round(mean(yt == 0), 3), "; recall 0\n")
for (m in c("logit", "forest")) {
  p <- if (m == "logit") p_lg else p_rf
  cat("\n##", m, "; AUC", round(auc(p), 3), "\n")
  print(round(t(sapply(c(0.5, 0.3, 0.2, 0.1), function(c) metr(p, c))), 3))
}
