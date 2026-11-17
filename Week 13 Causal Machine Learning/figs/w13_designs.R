## =============================================================================
## w13_designs.R -- DML inside four designs from earlier weeks
##   1. weighting (Week 6): AIPW / interactive model on Brand et al.'s NLSY79 data
##   2. DiD (Week 10): doubly robust DiD with ML on the 1980-87 marriage 2x2
##   3. RD (Week 9): ML covariate adjustment on the Senate data (Noack, Olma, Rothe)
##   4. IV (Week 7): partially linear IV on Card's proximity instrument
## Run from the Week 13 folder:  Rscript figs/w13_designs.R > figs/w13_designs.out
## =============================================================================
suppressMessages({library(DoubleML); library(mlr3); library(mlr3learners)
                  library(ranger); library(rdrobust); library(ivreg)})
lgr::get_logger("mlr3")$set_threshold("warn")
rf_reg <- function() lrn("regr.ranger", num.trees = 500, min.node.size = 5)
rf_cls <- function() lrn("classif.ranger", num.trees = 500, min.node.size = 5)
show <- function(lbl, est, se) cat(sprintf("%-46s %8.3f (%6.3f)\n", lbl, est, se))

## ---- 1. weighting: AIPW with forests on the NLSY79 --------------------------
cat("== 1. AIPW / interactive regression model, NLSY79 (unweighted) ==\n")
d <- readRDS("../Data/nlsy79_brand.rds")
xvars <- c("male","black","hispanic","south","rural","parinc","fatheduc","motheduc",
           "fatherwc","twoparent","siblings","ability","collegeprep","rotter","delinq",
           "expect","aspire","friendasp","schooldis","married18","child18",
           "parinc_miss","schooldis_miss","fatheduc_miss")
dat <- DoubleMLData$new(d[, c("lowwage", "college", xvars)], y_col = "lowwage", d_cols = "college", x_cols = xvars)
show("difference in means", coef(lm(lowwage ~ college, d))[2],
     summary(lm(lowwage ~ college, d))$coef[2, 2])
olsx <- lm(as.formula(paste("lowwage ~ college +", paste(xvars, collapse = "+"))), d)
show("OLS, 24 linear controls", coef(olsx)["college"], summary(olsx)$coef["college", 2])
set.seed(723)
plr <- DoubleMLPLR$new(dat, ml_l = rf_reg(), ml_m = rf_reg(), n_folds = 5, n_rep = 1)
plr$fit(); show("DML partially linear (forests)", plr$coef, plr$se)
for (tgt in c("ATE", "ATTE")) {
  set.seed(723)
  irm <- DoubleMLIRM$new(dat, ml_g = rf_reg(), ml_m = rf_cls(), score = tgt,
                         n_folds = 5, trimming_threshold = 0.01)
  irm$fit(store_predictions = TRUE)
  show(paste("DML interactive (AIPW), target", tgt), irm$coef, irm$se)
}
ehat <- irm$predictions$ml_m[, 1, 1]
cat("cross-fitted propensity: share < .05 =", round(mean(ehat < .05), 3),
    "; share > .95 =", round(mean(ehat > .95), 3), "\n")
## overlap-trimmed ATE: drop propensities outside [.05, .95] (Crump et al. 2009)
keep <- ehat >= .05 & ehat <= .95
datk <- DoubleMLData$new(d[keep, c("lowwage", "college", xvars)], y_col = "lowwage", d_cols = "college", x_cols = xvars)
set.seed(723)
irmk <- DoubleMLIRM$new(datk, ml_g = rf_reg(), ml_m = rf_cls(), score = "ATE", n_folds = 5)
irmk$fit(); show(sprintf("ATE among .05 <= e <= .95 (n = %d)", sum(keep)), irmk$coef, irmk$se)

## ---- 2. DiD: doubly robust DiD with ML, marriage 1980 -> 1987 ----------------
cat("\n== 2. DiD, wagepan 1980 -> 1987: marry vs. no change ==\n")
data(wagepan, package = "wooldridge")
w80 <- subset(wagepan, year == 1980); w87 <- subset(wagepan, year == 1987)
dd <- merge(w80, w87[, c("nr", "lwage", "married")], by = "nr", suffixes = c("", ".87"))
dd$dD <- dd$married.87 - dd$married
dd <- subset(dd, dD >= 0)
dd$dy <- dd$lwage.87 - dd$lwage; dd$T <- dd$dD
cat("n =", nrow(dd), "; marry =", sum(dd$T), "\n")
show("2x2 DiD, no covariates", coef(lm(dy ~ T, dd))[2], summary(lm(dy ~ T, dd))$coef[2, 2])
## baseline (1980) covariates: schooling, race, experience, union, hours, industry and occupation
bx <- c("educ", "black", "hisp", "exper", "union", "hours", "lwage",
        grep("^occ", names(dd), value = TRUE), "agric", "min", "construc", "trad", "tra",
        "fin", "bus", "per", "ent", "manuf", "pro", "pub", "south", "nrthcen", "nrtheast", "rur")
bx <- intersect(bx, names(dd))
cat("baseline covariates:", length(bx), "\n")
ols_cov <- lm(as.formula(paste("dy ~ T +", paste(bx, collapse = "+"))), dd)
show("regression of change on T + 1980 covariates", coef(ols_cov)["T"], summary(ols_cov)$coef["T", 2])
## DML for the ATT on the change in log wage (Chang 2020): the AIPW ATT score with Y = change
datd <- DoubleMLData$new(dd[, c("dy", "T", bx)], y_col = "dy", d_cols = "T", x_cols = bx)
set.seed(723)
didml <- DoubleMLIRM$new(datd, ml_g = rf_reg(), ml_m = rf_cls(), score = "ATTE", n_folds = 5, n_rep = 5)
didml$fit(); show("DML DiD (ATT, forests, 5 folds x 5 reps)", didml$coef, didml$se)
## the partialling-out version: residualize the change and T on the 1980 covariates, then regress
set.seed(723)
didplr <- DoubleMLPLR$new(datd, ml_l = rf_reg(), ml_m = rf_reg(), n_folds = 5, n_rep = 5)
didplr$fit(); show("DML partially linear on change (forests, 5x5)", didplr$coef, didplr$se)

## ---- 3. RD: ML covariate adjustment, Senate --------------------------------
cat("\n== 3. RD with ML covariate adjustment, Senate ==\n")
data(rdrobust_RDsenate)
## the four pre-determined covariates of Week 9 (w9_senate.R section 8)
cv <- c("demvoteshlag1", "demvoteshlag2", "presdemvoteshlag1", "dopen")
s <- rdrobust_RDsenate[complete.cases(rdrobust_RDsenate[, c("vote", "margin", cv)]), ]
cat("n =", nrow(s), "\n")
r0 <- rdrobust(s$vote, s$margin)
r1 <- rdrobust(s$vote, s$margin, covs = as.matrix(s[, cv]))
## Noack-Olma-Rothe: predict vote from covariates ONLY (no margin), cross-fitted, pooled over both
## sides; then run the usual RD on the adjusted outcome vote - eta_hat(Z)
set.seed(723)
K <- 5; fold <- sample(rep(1:K, length.out = nrow(s))); eta <- numeric(nrow(s))
for (k in 1:K) {
  tr <- fold != k & abs(s$margin) < 40          # learn eta from races within 40 points
  fit <- ranger(vote ~ ., data = s[tr, c("vote", cv)], num.trees = 1000, seed = 723)
  eta[fold == k] <- predict(fit, s[fold == k, cv])$predictions
}
cat("R2 of eta_hat (cross-fitted):", round(1 - mean((s$vote - eta)^2) / var(s$vote), 3), "\n")
r2 <- rdrobust(s$vote - eta, s$margin)
tab <- rbind(no_covariates = c(r0$coef[1], r0$se[3], r0$ci[3, ], r0$bws[1, 1]),
             linear_covs   = c(r1$coef[1], r1$se[3], r1$ci[3, ], r1$bws[1, 1]),
             forest_adjust = c(r2$coef[1], r2$se[3], r2$ci[3, ], r2$bws[1, 1]))
colnames(tab) <- c("conv.est", "robust.se", "ci.lo", "ci.hi", "h")
print(round(tab, 2))
re <- rdrobust(eta, s$margin)
cat("jump in eta_hat(Z) at the cutoff (should be ~0 if Z is balanced):",
    round(re$coef[1], 2), "(robust se", round(re$se[3], 2), ")\n")
cat("identity: no-covariate jump - jump in eta_hat =", round(r0$coef[1] - re$coef[1], 2),
    " vs adjusted", round(r2$coef[1], 2), "(same h only approximately)\n")
cat("robust CI length relative to no covariates:", round(apply(tab[, 3:4], 1, diff) / diff(tab[1, 3:4]), 3), "\n")

## ---- 4. IV: partially linear IV, Card --------------------------------------
cat("\n== 4. partially linear IV, Card (1995) ==\n")
data("SchoolingReturns", package = "ivreg")
cr <- SchoolingReturns
cr$lwage <- log(cr$wage); cr$near <- as.integer(cr$nearcollege == "yes")
num <- function(f) as.integer(f == "yes")
cr <- transform(cr, black = as.integer(ethnicity == "afam"), smsa_ = num(smsa), south_ = num(south),
                smsa66_ = num(smsa66), south66_ = num(south66), married_ = num(married),
                lib14 = num(library14), both14 = as.integer(parents14 == "both"),
                famed = as.integer(fameducation))
wv <- c("age", "black", "smsa_", "south_", "smsa66_", "south66_", "married_",
        "lib14", "both14", "famed", "feducation", "meducation")
cr <- cr[complete.cases(cr[, c("lwage", "education", "near", wv)]), ]
cat("n =", nrow(cr), "\n")
ols <- lm(as.formula(paste("lwage ~ education +", paste(wv, collapse = "+"))), cr)
show("OLS with linear controls", coef(ols)["education"], summary(ols)$coef["education", 2])
iv <- ivreg(as.formula(paste("lwage ~ education +", paste(wv, collapse = "+"), "|",
                             "near +", paste(wv, collapse = "+"))), data = cr)
show("2SLS with linear controls", coef(iv)["education"], summary(iv, vcov. = sandwich::vcovHC(iv, type = "HC1"))$coef["education", 2])
datc <- DoubleMLData$new(cr[, c("lwage", "education", "near", wv)], y_col = "lwage",
                         d_cols = "education", z_cols = "near", x_cols = wv)
set.seed(723)
pliv <- DoubleMLPLIV$new(datc, ml_l = rf_reg(), ml_m = rf_reg(), ml_r = rf_reg(), n_folds = 5, n_rep = 5)
pliv$fit(); show("DML partially linear IV (forests)", pliv$coef, pliv$se)
set.seed(723)
lasso <- function() lrn("regr.cv_glmnet", s = "lambda.min")
pliv2 <- DoubleMLPLIV$new(datc, ml_l = lasso(), ml_m = lasso(), ml_r = lasso(), n_folds = 5, n_rep = 5)
pliv2$fit(); show("DML partially linear IV (lasso)", pliv2$coef, pliv2$se)
## first-stage strength after partialling out: the residualized Z on residualized D
