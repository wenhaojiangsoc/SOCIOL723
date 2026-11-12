## Week 12 -- SHAP (Shapley) values for one respondent, from the GSS benchmark forest
## (same data, split, and ranger call as figs/w12_importance.R). Every number on the SHAP frames comes from here.
##   Rscript figs/w12_shap.R > figs/w12_shap.out   (run from the week folder)
## Four players: education, age, female, and the other 20 columns as one block.
## Value of a coalition S: the forest's average prediction when the columns in S are set to the
## respondent's values and the rest keep their values in a background sample of training cases.
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
rf <- ranger(y ~ ., dtr, num.trees = 1000, seed = 723)
cat("test MSE of the forest:", round(mean((y[te] - predict(rf, dte)$predictions)^2), 3), "\n")

## ---- A. the respondent and the background ------------------------------------------
cols <- names(dte)
players <- list(education = "educ", age = "age", female = "female",
                `other 20 columns` = setdiff(cols, c("educ", "age", "female")))
## a woman in the test set with 16 years of schooling, closest to age 45
cand <- which(dte$female == 1 & dte$educ == 16)
i0 <- cand[which.min(abs(dte$age[cand] - 45))]
x0 <- dte[i0, ]
cat("\nA. respondent: test row", i0, "; educ", x0$educ, "; age", x0$age, "; female", x0$female,
    "; hours", x0$hours, "; children", x0$childs, "; ever married", x0$evermar,
    "; actual log earnings", round(y[te][i0], 3), "\n")
set.seed(1)
bg <- dtr[sample(nrow(dtr), 500), cols]                      # background: 500 training cases
v <- function(S) {                                           # value of coalition S (player names)
  z <- bg
  for (s in S) z[players[[s]]] <- x0[rep(1, nrow(z)), players[[s]], drop = FALSE]
  mean(predict(rf, z)$predictions)
}

## ---- B. every coalition, then the Shapley average ----------------------------------
K <- length(players); nm <- names(players)
subsets <- unlist(lapply(0:K, function(k) combn(nm, k, simplify = FALSE)), recursive = FALSE)
vals <- sapply(subsets, v); names(vals) <- sapply(subsets, function(S) if (length(S)) paste(S, collapse = "+") else "{}")
cat("\nB. value of each coalition (average prediction):\n"); print(round(vals, 3))
key <- function(S) if (length(S)) paste(nm[nm %in% S], collapse = "+") else "{}"
phi <- sapply(nm, function(j) {
  others <- setdiff(nm, j)
  sum(sapply(unlist(lapply(0:(K - 1), function(k) combn(others, k, simplify = FALSE)), recursive = FALSE),
             function(S) {
    w <- factorial(length(S)) * factorial(K - length(S) - 1) / factorial(K)
    w * (vals[[key(c(S, j))]] - vals[[key(S)]])
  }))
})
base <- vals[["{}"]]; full <- vals[[key(nm)]]
cat("\naverage prediction (nobody's values known):", round(base, 3), "\n")
cat("Shapley values:\n"); print(round(phi, 3))
cat("base + sum of Shapley values:", round(base + sum(phi), 3), "; forest's prediction for her:", round(full, 3),
    "; direct predict():", round(predict(rf, x0)$predictions, 3), "\n")
cat("in earnings: exp(base)", round(exp(base)), "; exp(prediction)", round(exp(full)), "\n")

## ---- C. education's value worked out: the 2-player view inside the 4 players --------
cat("\nC. education's marginal gain by coalition it joins:\n")
for (S in unlist(lapply(0:(K - 1), function(k) combn(setdiff(nm, "education"), k, simplify = FALSE)), recursive = FALSE)) {
  w <- factorial(length(S)) * factorial(K - length(S) - 1) / factorial(K)
  cat(sprintf("  joins %-35s weight %.4f  gain %.3f\n", key(S), w, vals[[key(c(S, "education"))]] - vals[[key(S)]]))
}
cat("first to arrive (alone):", round(vals[["education"]] - base, 3),
    "; last to arrive:", round(full - vals[[key(setdiff(nm, "education"))]], 3), "\n")
