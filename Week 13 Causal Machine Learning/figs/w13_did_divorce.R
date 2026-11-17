## Week 13 -- DML inside a difference-in-differences: unilateral divorce and female suicide
## (Stevenson and Wolfers 2006; bacondecomp::divorce, women; Week 10's data).
## 2x2: treated = 25 states adopting 1969-73; controls = 8 states with no law through 1978;
## before = 1964-68, after = 1974-78. Y = change in the mean female suicide rate (per million).
## X = 19 state characteristics, all measured 1964-68 (before any of these laws).
##   Rscript figs/w13_did_divorce.R > figs/w13_did_divorce.out ; writes figs/design_did.pdf
set.seed(723)
suppressMessages({library(bacondecomp); library(DoubleML); library(mlr3); library(mlr3learners); library(data.table); library(sandwich)})
lgr::get_logger("mlr3")$set_threshold("warn")
d <- subset(divorce, sex == 2)
d$suiciderate_jag <- d$suiciderate_jag * 1e6        # suicides per million women, as in Week 10
st <- d[d$year == 1964, c("st", "divyear")]
st$grp <- ifelse(st$divyear >= 1969 & st$divyear <= 1973, 1, ifelse(st$divyear >= 1979, 0, NA))
st <- st[!is.na(st$grp), ]
cat("treated states:", sum(st$grp == 1), "; control states:", sum(st$grp == 0), "\n")
xv <- c("lnpersinc", "stateur", "femep", "fshare", "shblack", "sa14t19", "sa20t29", "sa30t39", "sa40t49", "sa50t59",
        "sa60t69", "afdcmax", "afdcrolls", "murderrate", "violentcrimerate", "propertycrimerate", "prison", "drate")
pre <- subset(d, year %in% 1964:1968); post <- subset(d, year %in% 1974:1978)
m <- function(df, v) tapply(df[[v]], df$st, mean, na.rm = TRUE)
s <- data.frame(st = st$st, D = st$grp,
                y_pre = m(pre, "suiciderate_jag")[st$st], y_post = m(post, "suiciderate_jag")[st$st])
for (v in xv) s[[v]] <- m(pre, v)[st$st]
s$dY <- s$y_post - s$y_pre
X <- c("y_pre", xv)
cat("covariates:", length(X), "; any missing:", anyNA(s), "\n")

## ---- 1. the 2x2 by hand, and with linear controls ----
mm <- aggregate(cbind(y_pre, y_post, dY) ~ D, s, mean); print(round(mm, 1))
raw <- lm(dY ~ D, s); lin <- lm(as.formula(paste("dY ~ D +", paste(X, collapse = "+"))), s)
rob <- function(f) sqrt(vcovHC(f, "HC1")["D", "D"])
cat("\n## 1. raw 2x2:", round(coef(raw)[2], 2), "(", round(rob(raw), 2), ") ; all", length(X), "controls linearly:",
    round(coef(lin)[2], 2), "(", round(rob(lin), 2), "), residual df", lin$df.residual, "\n")

## ---- 2. DML: partial X out of the change dY and out of D ----
dat <- DoubleMLData$new(as.data.table(s[, c("dY", "D", X)]), y_col = "dY", d_cols = "D", x_cols = X)
L <- list("OLS" = lrn("regr.lm"), "lasso" = lrn("regr.cv_glmnet", alpha = 1, s = "lambda.min", nfolds = 5),
          "ridge" = lrn("regr.cv_glmnet", alpha = 0, s = "lambda.min", nfolds = 5),
          "forest" = lrn("regr.ranger", num.trees = 500))
R <- 50
r2 <- function(v, p) 1 - mean((v - p)^2) / var(v)
fit <- function(ly, ld) { set.seed(723)
  o <- DoubleMLPLR$new(dat, ml_l = ly$clone(), ml_m = ld$clone(), n_folds = 5, n_rep = R); o$fit(store_predictions = TRUE)
  list(o = o, row = c(R2_dY = mean(sapply(1:R, function(k) r2(s$dY, o$predictions$ml_l[, k, 1]))),
    R2_D = mean(sapply(1:R, function(k) r2(s$D, o$predictions$ml_m[, k, 1]))), est = o$coef[[1]], se = o$se[[1]])) }
res <- lapply(L, function(l) fit(l, l)); tab <- t(sapply(res, `[[`, "row"))
cat("\n## 2. out-of-fold R2 and DML estimate, 5 folds x", R, "splits\n"); print(round(tab, 3))
bY <- names(which.max(tab[, "R2_dY"])); bD <- names(which.max(tab[, "R2_D"]))
best <- fit(L[[bY]], L[[bD]])
cat("best for dY:", bY, "; best for D:", bD, "->", round(best$row["est"], 2), "(", round(best$row["se"], 2), ")",
    "; range over splits", round(range(best$o$all_coef), 2), "\n")

## ---- 3. picture: raw vs residualized (one split, best learners) ----
set.seed(723)
o <- DoubleMLPLR$new(dat, ml_l = L[[bY]]$clone(), ml_m = L[[bD]]$clone(), n_folds = 5, n_rep = 1); o$fit(store_predictions = TRUE)
Yt <- s$dY - o$predictions$ml_l[, 1, 1]; Dt <- s$D - o$predictions$ml_m[, 1, 1]
cat("this split: residual-on-residual slope", round(sum(Dt * Yt) / sum(Dt^2), 2), "\n")
pdf("figs/design_did.pdf", width = 7.2, height = 3, family = "Times")
par(mfrow = c(1, 2), mar = c(4, 4.2, 1.8, 0.8), cex.main = 1, font.main = 1, mgp = c(2.3, 0.7, 0))
plot(jitter(s$D, 0.15), s$dY, pch = 19, col = ifelse(s$D == 1, "navy", "gray45"), xaxt = "n", xlab = "", las = 1,
     ylab = "change in suicide rate", main = "raw: change by group", xlim = c(-0.4, 1.4))
axis(1, at = 0:1, labels = c("no law", "adopted 1969-73"))
segments(c(-0.25, 0.75), tapply(s$dY, s$D, mean), c(0.25, 1.25), tapply(s$dY, s$D, mean), lwd = 3, col = "firebrick")
plot(Dt, Yt, pch = 19, col = ifelse(s$D == 1, "navy", "gray45"), las = 1, xlab = "D minus its prediction from X",
     ylab = "change minus its prediction", main = "after removing what X predicts")
abline(0, sum(Dt * Yt) / sum(Dt^2), col = "firebrick", lwd = 2.5)
dev.off()
