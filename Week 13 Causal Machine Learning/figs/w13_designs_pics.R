## =============================================================================
## w13_designs_pics.R -- one picture per design: the raw comparison vs. the same
## comparison after partialling the controls out by cross-fitted forests.
## Estimates on the slides come from w13_designs.R (5 folds x 5 splits); these
## pictures use one split, so their slopes are close to, not equal to, those.
## Run from the Week 13 folder:  Rscript figs/w13_designs_pics.R > figs/w13_designs_pics.out
## =============================================================================
suppressMessages({library(DoubleML); library(mlr3); library(mlr3learners)
                  library(ranger); library(rdrobust); library(ivreg)})
lgr::get_logger("mlr3")$set_threshold("warn")
rf_reg <- function() lrn("regr.ranger", num.trees = 500, min.node.size = 5)
navy <- rgb(1, 33, 105, maxColorValue = 255); fire <- "firebrick"

## binned means of y within quantile bins of x
binned <- function(x, y, nb = 20) {
  b <- cut(x, unique(quantile(x, seq(0, 1, length.out = nb + 1))), include.lowest = TRUE)
  data.frame(x = tapply(x, b, mean), y = tapply(y, b, mean))
}
## residuals from a DoubleML fit with stored cross-fitted predictions
resid_of <- function(v, pred) v - pred[, 1, 1]

panel <- function(x, y, slope, xlab, ylab, main, through0 = TRUE, nb = 20) {
  bm <- binned(x, y, nb)
  plot(bm$x, bm$y, pch = 19, col = navy, xlab = xlab, ylab = ylab, main = main,
       cex.main = 1, font.main = 1)
  if (through0) abline(0, slope, col = fire, lwd = 2) else abline(lm(y ~ x), col = fire, lwd = 2)
  abline(h = 0, v = 0, col = "gray80")
  legend("topright", sprintf("slope %.3f", slope), bty = "n", text.col = fire)
}

## ---- 1. selection on observables, NLSY79 --------------------------------------
d <- readRDS("../Data/nlsy79_brand.rds")
xvars <- c("male","black","hispanic","south","rural","parinc","fatheduc","motheduc",
           "fatherwc","twoparent","siblings","ability","collegeprep","rotter","delinq",
           "expect","aspire","friendasp","schooldis","married18","child18",
           "parinc_miss","schooldis_miss","fatheduc_miss")
dat <- DoubleMLData$new(d[, c("lowwage", "college", xvars)], y_col = "lowwage", d_cols = "college", x_cols = xvars)
set.seed(723)
plr <- DoubleMLPLR$new(dat, ml_l = rf_reg(), ml_m = rf_reg(), n_folds = 5, n_rep = 1)
plr$fit(store_predictions = TRUE)
yt <- resid_of(d$lowwage, plr$predictions$ml_l); dt <- resid_of(d$college, plr$predictions$ml_m)
b1 <- sum(dt * yt) / sum(dt^2)
cat("NLSY: residual-on-residual slope (one split):", round(b1, 3), "\n")
dm <- coef(lm(lowwage ~ college, d))[2]
pdf("figs/design_sel.pdf", width = 8, height = 3.3)
par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2, 1), bty = "l", las = 1, cex.axis = .85)
m <- tapply(d$lowwage, d$college, mean)
plot(c(0, 1), m, pch = 19, col = navy, xlim = c(-.2, 1.2), ylim = c(0, .35), xaxt = "n",
     xlab = "college D", ylab = "mean low-wage share Y", main = "raw: graduates vs. others",
     cex.main = 1, font.main = 1)
axis(1, at = 0:1); segments(0, m[1], 1, m[2], col = fire, lwd = 2)
legend("topright", sprintf("difference %.3f", dm), bty = "n", text.col = fire)
panel(dt, yt, b1, "college residual D - e-hat(X)", "Y - l-hat(X)",
      "after partialling out X (forests)")
dev.off()

## ---- 2. DiD, marriage 1980 -> 1987 ---------------------------------------------
data(wagepan, package = "wooldridge")
w80 <- subset(wagepan, year == 1980); w87 <- subset(wagepan, year == 1987)
dd <- merge(w80, w87[, c("nr", "lwage", "married")], by = "nr", suffixes = c("", ".87"))
dd$dD <- dd$married.87 - dd$married; dd <- subset(dd, dD >= 0)
dd$dy <- dd$lwage.87 - dd$lwage; dd$T <- dd$dD
bx <- c("educ", "black", "hisp", "exper", "union", "hours", "lwage",
        grep("^occ", names(dd), value = TRUE), "agric", "min", "construc", "trad", "tra",
        "fin", "bus", "per", "ent", "manuf", "pro", "pub", "south", "nrthcen", "nrtheast", "rur")
bx <- intersect(bx, names(dd))
datd <- DoubleMLData$new(dd[, c("dy", "T", bx)], y_col = "dy", d_cols = "T", x_cols = bx)
set.seed(723)
pd <- DoubleMLPLR$new(datd, ml_l = rf_reg(), ml_m = rf_reg(), n_folds = 5, n_rep = 1)
pd$fit(store_predictions = TRUE)
yt2 <- resid_of(dd$dy, pd$predictions$ml_l); dt2 <- resid_of(dd$T, pd$predictions$ml_m)
b2 <- sum(dt2 * yt2) / sum(dt2^2)
cat("DiD: residual-on-residual slope (one split):", round(b2, 3), "\n")
pdf("figs/design_did.pdf", width = 8, height = 3.3)
par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2, 1), bty = "l", las = 1, cex.axis = .85)
m2 <- tapply(dd$dy, dd$T, mean)
plot(c(0, 1), m2, pch = 19, col = navy, xlim = c(-.2, 1.2), ylim = c(0, .6), xaxt = "n",
     xlab = "married between 1980 and 1987", ylab = "mean change in log wage",
     main = "raw 2x2: change of married vs. others", cex.main = 1, font.main = 1)
axis(1, at = 0:1); segments(0, m2[1], 1, m2[2], col = fire, lwd = 2)
legend("topright", sprintf("difference %.3f", diff(m2)), bty = "n", text.col = fire)
panel(dt2, yt2, b2, "marriage residual D - e-hat(X)",
      "change - l-hat(X)", "after partialling out 1980 X (forests)", nb = 12)
dev.off()

## ---- 3. RD, Senate --------------------------------------------------------------
data(rdrobust_RDsenate)
cv <- c("demvoteshlag1", "demvoteshlag2", "presdemvoteshlag1", "dopen")
s <- rdrobust_RDsenate[complete.cases(rdrobust_RDsenate[, c("vote", "margin", cv)]), ]
set.seed(723)
K <- 5; fold <- sample(rep(1:K, length.out = nrow(s))); eta <- numeric(nrow(s))
for (k in 1:K) {
  tr <- fold != k & abs(s$margin) < 40
  fit <- ranger(vote ~ ., data = s[tr, c("vote", cv)], num.trees = 1000, seed = 723)
  eta[fold == k] <- predict(fit, s[fold == k, cv])$predictions
}
s$adj <- s$vote - eta + mean(eta)          # re-centred so both panels share a scale
rdpanel <- function(y, main) {
  keep <- abs(s$margin) <= 40
  xm <- s$margin[keep]; ym <- y[keep]
  br <- seq(-40, 40, by = 2.5)
  b <- cut(xm, br, include.lowest = TRUE)
  bx_ <- tapply(xm, b, mean); by_ <- tapply(ym, b, mean)
  plot(bx_, by_, pch = 19, col = navy, xlab = "Democratic margin at t (running variable)",
       ylab = "Democratic vote share at t+6", main = main, cex.main = 1, font.main = 1,
       ylim = c(25, 75))
  abline(v = 0, col = "gray60")
  h <- 17.9
  for (side in c(-1, 1)) {
    w <- side * xm >= 0 & abs(xm) <= h
    f <- lm(ym[w] ~ xm[w]); xs <- if (side < 0) c(-h, 0) else c(0, h)
    lines(xs, coef(f)[1] + coef(f)[2] * xs, col = fire, lwd = 2)
  }
}
pdf("figs/design_rd.pdf", width = 8, height = 3.3)
par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2, 1), bty = "l", las = 1, cex.axis = .85)
rdpanel(s$vote, "raw outcome")
rdpanel(s$adj, "outcome minus forest prediction")
dev.off()
cat("RD: sd of outcome within 18 points, raw vs adjusted:",
    round(sd(s$vote[abs(s$margin) < 18]), 2), round(sd(s$adj[abs(s$margin) < 18]), 2), "\n")

## ---- 4. IV, Card -----------------------------------------------------------------
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
datc <- DoubleMLData$new(cr[, c("lwage", "education", "near", wv)], y_col = "lwage",
                         d_cols = "education", z_cols = "near", x_cols = wv)
set.seed(723)
pl <- DoubleMLPLIV$new(datc, ml_l = rf_reg(), ml_m = rf_reg(), ml_r = rf_reg(), n_folds = 5, n_rep = 1)
pl$fit(store_predictions = TRUE)
yt4 <- resid_of(cr$lwage, pl$predictions$ml_l)
zt4 <- resid_of(cr$near, pl$predictions$ml_m)
dt4 <- resid_of(cr$education, pl$predictions$ml_r)
fs <- sum(zt4 * dt4) / sum(zt4^2); rf <- sum(zt4 * yt4) / sum(zt4^2)
cat("IV (one split): first stage", round(fs, 3), " reduced form", round(rf, 4),
    " ratio", round(rf / fs, 3), "\n")
## raw versions for comparison
cat("IV raw: first stage", round(coef(lm(education ~ near, cr))[2], 3),
    " reduced form", round(coef(lm(lwage ~ near, cr))[2], 4), "\n")
pdf("figs/design_iv.pdf", width = 8, height = 3.3)
par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2, 1), bty = "l", las = 1, cex.axis = .85)
panel(zt4, dt4, fs, "proximity residual Z - q-hat(X)",
      "schooling - r-hat(X)", "first stage, partialled out")
panel(zt4, yt4, rf, "proximity residual Z - q-hat(X)",
      "log wage - l-hat(X)", "reduced form, partialled out")
dev.off()
cat("figures written\n")
