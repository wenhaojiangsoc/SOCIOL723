## Week 9 -- every Senate number on the slides comes from here.
## Data: rdrobust_RDsenate (Cattaneo, Frandsen, and Titiunik 2015), U.S. Senate elections 1914-2010.
##   running variable margin = Democratic margin of victory at t (percentage points);
##   outcome vote = Democratic vote share in the same seat at t + 6.
##   Rscript figs/w9_senate.R > figs/w9_senate.out ; writes figs/senate_rdplot.pdf, figs/poly_weights.pdf
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
for (p in c("RDHonest", "rdlocrand"))
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
library(rdrobust); library(rddensity); library(RDHonest); library(rdlocrand)
data(rdrobust_RDsenate)
s <- subset(rdrobust_RDsenate, !is.na(vote))
x <- s$margin; y <- s$vote
cat("n =", nrow(s), "; below =", sum(x < 0), "; above =", sum(x >= 0), "\n")

## ---- 1. the headline estimate ----------------------------------------------------
r <- rdrobust(y, x)
h <- r$bws[1, 1]; b <- r$bws[2, 1]
cat("\n## 1. rdrobust defaults (MSE-optimal h, triangular, local linear, bias with local quadratic)\n")
cat("h =", round(h, 2), " b =", round(b, 2), " eff n left/right =", r$N_h, "\n")
print(round(cbind(coef = r$coef, se = r$se, r$ci), 3))

## ---- 2. the estimator by hand: kernel-weighted interacted regression --------------
cat("\n## 2. by hand: weighted lm within h, triangular weights\n")
d <- subset(s, abs(margin) <= h); d$D <- as.integer(d$margin >= 0)
d$w <- 1 - abs(d$margin) / h
fit <- lm(vote ~ D * margin, data = d, weights = w)
print(round(coef(fit), 3))
mu_minus <- coef(fit)[["(Intercept)"]]; mu_plus <- mu_minus + coef(fit)[["D"]]
cat("mu_minus =", round(mu_minus, 2), " mu_plus =", round(mu_plus, 2), "\n")
cat("local constant (difference in means within h):",
    round(mean(d$vote[d$D == 1]) - mean(d$vote[d$D == 0]), 2), "\n")
## exact accounting with a UNIFORM kernel: each OLS line passes through its side's means
fu <- lm(vote ~ D * margin, data = d)
bm <- coef(fu)[["margin"]]; bp <- bm + coef(fu)[["D:margin"]]
xa <- mean(d$margin[d$D == 1]); xb <- mean(d$margin[d$D == 0])
cat("uniform-kernel local linear tau =", round(coef(fu)[["D"]], 3), "; slopes below/above =", round(bm, 3), round(bp, 3),
    "; mean distance above/below =", round(xa, 2), round(xb, 2), "\n")
cat("tau_LL + bp * xa - bm * xb =", round(coef(fu)[["D"]] + bp * xa - bm * xb, 3), "(= difference in means)\n")
cat("means within h: above", round(mean(d$vote[d$D == 1]), 2), " below", round(mean(d$vote[d$D == 0]), 2), "\n")

## ---- 3. bias correction = local quadratic when b = h ----------------------------
cat("\n## 3. robust bias correction with b = h vs local quadratic at h\n")
rbh <- rdrobust(y, x, h = h, b = h)
rq  <- rdrobust(y, x, h = h, p = 2)
print(round(rbind(bc_b_eq_h = c(rbh$coef[2], rbh$se[3]), local_quadratic = c(rq$coef[1], rq$se[1])), 4))

## ---- 4. other bandwidths and intervals --------------------------------------------
cat("\n## 4. CER-optimal h for inference\n")
rc <- rdrobust(y, x, bwselect = "cerrd")
cat("h_CER =", round(rc$bws[1, 1], 2), " eff n =", rc$N_h, "\n")
print(round(cbind(coef = rc$coef, se = rc$se, rc$ci), 3))
cat("ratio h_CER / h_MSE =", round(rc$bws[1, 1] / h, 3), " ; n^(-1/20) =", round(nrow(s)^(-1/20), 3), "\n")
cat("\n## 4b. honest CI (Armstrong-Kolesar), rule-of-thumb M\n")
hon <- RDHonest(vote ~ margin, data = s)
print(hon$coefficients)

## ---- 5. bandwidth sensitivity -----------------------------------------------------
cat("\n## 5. bandwidth sensitivity (conventional coef, robust CI; b = h / rho with rho = h/b of the default)\n")
rho <- h / b
hs <- c(5, 7.5, 10, 12.5, 15, h, 20, 25, 30, 40, 50)
bw <- t(sapply(hs, function(hh) { q <- rdrobust(y, x, h = hh, b = hh / rho); c(h = hh, coef = q$coef[1], lo = q$ci[3, 1], hi = q$ci[3, 2], nl = q$N_h[1], nr = q$N_h[2]) }))
print(round(bw, 2))

## ---- 6. global polynomials: estimates and implied weights ---------------------------
cat("\n## 6. global polynomial of degree p, fitted separately on each side (all data)\n")
gp <- sapply(1:6, function(p) {
  fr <- lm(y[x >= 0] ~ poly(x[x >= 0], p, raw = TRUE)); fl <- lm(y[x < 0] ~ poly(x[x < 0], p, raw = TRUE))
  coef(fr)[1] - coef(fl)[1] })
print(round(setNames(gp, paste0("p=", 1:6)), 2))
## weight each observation above the cutoff gets in the estimate of mu_plus: e1'(X'WX)^{-1} x_i w_i
wts <- function(xx, p, w = rep(1, length(xx))) {
  X <- outer(xx / 100, 0:p, `^`); A <- solve(crossprod(X, X * w)); as.vector(X %*% A[, 1]) * w }
xr <- sort(x[x >= 0]); n_r <- length(xr)
w1 <- wts(xr, 1); w4 <- wts(xr, 4); w6 <- wts(xr, 6)
wl <- wts(xr, 1, pmax(0, 1 - xr / h))
cat("max |weight| x n_r: global p=1", round(max(abs(w1)) * n_r, 2), " p=4", round(max(abs(w4)) * n_r, 2),
    " p=6", round(max(abs(w6)) * n_r, 2), " local linear", round(max(abs(wl)) * n_r, 2), "\n")
cat("share of weight from margins > 50 points: p=1", round(sum(w1[xr > 50]), 3), " p=4", round(sum(w4[xr > 50]), 3),
    " p=6", round(sum(w6[xr > 50]), 3), " local", round(sum(wl[xr > 50]), 3), "\n")
cat("share of |weight| on races won by more than h = 17.75: p=1", round(sum(abs(w1[xr > h])) / sum(abs(w1)), 3),
    " p=4", round(sum(abs(w4[xr > h])) / sum(abs(w4)), 3), " p=6", round(sum(abs(w6[xr > h])) / sum(abs(w6)), 3), "\n")
cat("weight x n_r at margin ~75: p=4", round(w4[which.min(abs(xr - 75))] * n_r, 2), " p=6", round(w6[which.min(abs(xr - 75))] * n_r, 2), "\n")
pdf("figs/poly_weights.pdf", width = 9, height = 3.4, family = "Times")
par(mar = c(4, 4.2, 1.2, 1))
plot(xr, w1 * n_r, type = "l", lwd = 2, col = "gray50", ylim = range(c(w1, w4, w6, wl) * n_r),
     xlab = "Democratic margin at t (above the cutoff only)", ylab = expression("weight" %*% n[`+`]))
lines(xr, w4 * n_r, lwd = 2, col = "firebrick"); lines(xr, w6 * n_r, lwd = 2, col = "firebrick", lty = 2)
lines(xr, wl * n_r, lwd = 2, col = "navy"); abline(h = 0, lty = 3)
legend("topright", c("global, degree 1", "global, degree 4", "global, degree 6", "local linear, h = 17.8"),
       col = c("gray50", "firebrick", "firebrick", "navy"), lty = c(1, 1, 2, 1), lwd = 2, bty = "n")
dev.off()

## ---- 7. the RD plot ----------------------------------------------------------------
pdf("figs/senate_rdplot.pdf", width = 9, height = 3.6, family = "Times")
par(mar = c(4, 4.2, 1, 1))
br <- seq(-100, 100, by = 5); mids <- br[-1] - 2.5
bm <- tapply(y, cut(x, br, right = FALSE), mean)
plot(mids, bm, pch = 19, col = ifelse(mids < 0, "navy", "firebrick"), cex = 0.8, ylim = c(0, 100),
     xlab = "Democratic margin of victory at t", ylab = "Democratic vote share at t + 6")
abline(v = 0, lty = 2, col = "gray50")
gl <- seq(-h, 0, length.out = 50); gr <- seq(0, h, length.out = 50)
lines(gl, coef(fit)[1] + coef(fit)[3] * gl, lwd = 3, col = "navy")
lines(gr, coef(fit)[1] + coef(fit)[2] + (coef(fit)[3] + coef(fit)[4]) * gr, lwd = 3, col = "firebrick")
dev.off()

## ---- 8. covariates: precision, and the omitted-variable identity -------------------
cat("\n## 8. covariates (pre-determined)\n")
cv <- c("demvoteshlag1", "demvoteshlag2", "presdemvoteshlag1", "dopen")
sc <- s[complete.cases(s[, c("vote", "margin", cv)]), ]
r0 <- rdrobust(sc$vote, sc$margin); rz <- rdrobust(sc$vote, sc$margin, covs = as.matrix(sc[, cv]))
cat("complete cases n =", nrow(sc), "\n")
print(round(rbind(no_covs = c(r0$coef[1], r0$se[1], r0$ci[3, ], r0$bws[1, 1]),
                  covs = c(rz$coef[1], rz$se[1], rz$ci[3, ], rz$bws[1, 1])), 3))
cat("robust CI length ratio covs/no covs:", round(diff(rz$ci[3, ]) / diff(r0$ci[3, ]), 3), "\n")
## identity at a fixed h: tau_adj = tau_Y - tau_Z' gamma
hh <- 17.75; dd <- subset(sc, abs(margin) <= hh); dd$D <- as.integer(dd$margin >= 0); dd$w <- 1 - abs(dd$margin) / hh
tY <- coef(lm(vote ~ D * margin, dd, weights = w))[["D"]]
tZ <- sapply(cv, function(z) coef(lm(as.formula(paste(z, "~ D * margin")), dd, weights = w))[["D"]])
full <- lm(as.formula(paste("vote ~ D * margin +", paste(cv, collapse = "+"))), dd, weights = w)
gam <- coef(full)[cv]
cat("at h = 17.75: tau_Y =", round(tY, 3), "; tau_adj =", round(coef(full)[["D"]], 3),
    "; tau_Y - tau_Z'gamma =", round(tY - sum(tZ * gam), 3), "\n")
print(round(rbind(tau_Z = tZ, gamma = gam), 3))

## ---- 9. validity: covariate balance, density, placebo cutoffs, donut ---------------
cat("\n## 9a. covariates as outcomes (rdrobust defaults)\n")
bal <- t(sapply(c(cv, "population", "termshouse", "termssenate", "dmidterm", "dpresdem"), function(z) {
  ok <- !is.na(s[[z]]); q <- rdrobust(s[[z]][ok], s$margin[ok])
  c(mean_below = mean(s[[z]][ok & s$margin < 0 & s$margin > -q$bws[1, 1]]),
    coef = q$coef[1], lo = q$ci[3, 1], hi = q$ci[3, 2], p = q$pv[3], h = q$bws[1, 1]) }))
print(round(bal, 3))
cat("\n## 9b. density test (Cattaneo, Jansson, Ma)\n")
dt <- rddensity(x); cat("T =", round(dt$test$t_jk, 3), " p =", round(dt$test$p_jk, 3),
                        " h left/right =", round(dt$h$left, 2), round(dt$h$right, 2), "\n")
cat("binned counts within 2 points: [-2,0):", sum(x >= -2 & x < 0), " [0,2):", sum(x >= 0 & x < 2), "\n")
cat("\n## 9c. placebo cutoffs (each side only)\n")
pl <- t(sapply(c(-20, -15, -10, -5, 5, 10, 15, 20), function(cc) {
  sub <- if (cc < 0) x < 0 else x >= 0; q <- rdrobust(y[sub], x[sub], c = cc)
  c(cutoff = cc, coef = q$coef[1], lo = q$ci[3, 1], hi = q$ci[3, 2], p = q$pv[3]) }))
print(round(pl, 3))
cat("\n## 9d. donut: drop |margin| < delta\n")
dn <- t(sapply(c(0, 0.25, 0.5, 1, 2), function(dl) {
  ok <- abs(x) >= dl; q <- rdrobust(y[ok], x[ok]); c(delta = dl, dropped = sum(!ok), coef = q$coef[1], lo = q$ci[3, 1], hi = q$ci[3, 2]) }))
print(round(dn, 3))

## ---- 10. local randomization ------------------------------------------------------
cat("\n## 10. local randomization: window selection on covariates, then Fisher inference\n")
Xc <- as.matrix(s[, c("presdemvoteshlag1", "population", "demvoteshlag1", "demvoteshlag2", "demwinprv1", "demwinprv2", "dmidterm", "dpresdem", "dopen")])
ws <- rdwinselect(x, Xc, wmin = 0.5, wstep = 0.125, nwindows = 16, reps = 1000, seed = 723)
print(ws$results)
cat("selected window:", ws$window, "\n")
for (wd in c(0.625, 0.75)) {
  ri <- rdrandinf(y, x, wl = -wd, wr = wd, reps = 5000, seed = 723)
  inw <- abs(x) <= wd
  cat("window +-", wd, ": n below/above =", sum(inw & x < 0), sum(inw & x >= 0),
      " means below/above =", round(mean(y[inw & x < 0]), 2), round(mean(y[inw & x >= 0]), 2),
      " diff =", round(ri$obs.stat, 2), " Fisher p =", ri$p.value, "\n")
}
## by hand: permute D within the window
inw <- abs(x) <= 0.625; yy <- y[inw]; DD <- as.integer(x[inw] >= 0)
obs <- mean(yy[DD == 1]) - mean(yy[DD == 0])
perm <- replicate(5000, { Dp <- sample(DD); mean(yy[Dp == 1]) - mean(yy[Dp == 0]) })
cat("by hand, window +-0.625: diff =", round(obs, 2), " share of 5000 permutations with |diff| >= observed:", mean(abs(perm) >= abs(obs)), "\n")
