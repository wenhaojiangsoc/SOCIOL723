## Week 6 -- exact (coarsened) matching on the GSS, by hand.
## Every number on the "A Real Application" frames comes from here.
##   Rscript figs/w6_matching_gss.R
set.seed(723)
g <- readRDS("../Data/gss_earnings.rds")
g$D <- g$ba; g$Y <- g$lnearn
n <- nrow(g)

## ---- 1. coarsen five covariates into cells -------------------------------
## bins chosen at rough terciles of the continuous ones (pareduc 12/14 years,
## exper 17/30 years, wordsum 0-5 / 6-7 / 8-10); female and black as they are.
g$pareduc_b <- cut(g$pareduc, c(-Inf, 11, 13, Inf), labels = c("<12", "12-13", "14+"))
g$wordsum_b <- cut(g$wordsum, c(-Inf, 5, 7, Inf),   labels = c("0-5", "6-7", "8-10"))
g$exper_b   <- cut(g$exper,   c(-Inf, 16, 29, Inf), labels = c("0-16", "17-29", "30+"))
g$cell <- interaction(g$female, g$black, g$pareduc_b, g$wordsum_b, g$exper_b, drop = FALSE)
cat("possible cells:", nlevels(g$cell), "\n")

## ---- 2. cell table ----------------------------------------------------------
tab <- aggregate(cbind(n1 = D, n0 = 1 - D, y1 = Y * D, y0 = Y * (1 - D)) ~ cell, g, sum)
tab$n  <- tab$n1 + tab$n0
tab$m1 <- ifelse(tab$n1 > 0, tab$y1 / tab$n1, NA)   # mean Y among treated in cell
tab$m0 <- ifelse(tab$n0 > 0, tab$y0 / tab$n0, NA)   # mean Y among controls in cell
tab$tau <- tab$m1 - tab$m0                          # tau-hat(x)
tab$both <- tab$n1 > 0 & tab$n0 > 0
cat("occupied cells:", nrow(tab), "; with both arms:", sum(tab$both),
    "; treated only:", sum(tab$n1 > 0 & tab$n0 == 0),
    "; controls only:", sum(tab$n0 > 0 & tab$n1 == 0), "\n")
cat("units in matched cells:", sum(tab$n[tab$both]), "of", n,
    "; treated dropped:", sum(tab$n1[!tab$both]), "of", sum(g$D),
    "; controls dropped:", sum(tab$n0[!tab$both]), "of", sum(1 - g$D), "\n")

## ---- 3. estimates -----------------------------------------------------------
naive <- mean(g$Y[g$D == 1]) - mean(g$Y[g$D == 0])
m <- tab[tab$both, ]
ate <- sum(m$tau * m$n)  / sum(m$n)    # weights: cell share among matched units
att <- sum(m$tau * m$n1) / sum(m$n1)   # weights: cell share among matched treated
atu <- sum(m$tau * m$n0) / sum(m$n0)   # weights: cell share among matched controls
ols <- coef(lm(Y ~ D + female + black + pareduc + wordsum + exper, g))["D"]
ols_cells <- coef(lm(Y ~ D + cell, g))["D"]     # saturated in the cells: tau_R
cat(sprintf("naive %.3f | ATE %.3f | ATT %.3f | ATU %.3f | OLS linear %.3f | OLS cell dummies %.3f\n",
            naive, ate, att, atu, ols, ols_cells))

## bootstrap SEs for the matched estimators (resample units, redo everything)
boot_one <- function() {
  b <- g[sample(n, replace = TRUE), ]
  t <- aggregate(cbind(n1 = D, n0 = 1 - D, y1 = Y * D, y0 = Y * (1 - D)) ~ cell, b, sum)
  t <- t[t$n1 > 0 & t$n0 > 0, ]
  tau <- t$y1 / t$n1 - t$y0 / t$n0
  c(ate = sum(tau * (t$n1 + t$n0)) / sum(t$n1 + t$n0), att = sum(tau * t$n1) / sum(t$n1))
}
bs <- replicate(500, boot_one())
cat("bootstrap SE: ATE", round(sd(bs["ate", ]), 3), " ATT", round(sd(bs["att", ]), 3), "\n")

## ---- 4. balance: standardized mean differences ------------------------------
## smd = (weighted mean treated - weighted mean control) / pooled SD in the full sample
covs <- c("pareduc", "wordsum", "exper", "female", "black", "otherace")
sd_pool <- sapply(covs, function(v) sqrt((var(g[g$D == 1, v]) + var(g[g$D == 0, v])) / 2))
wmean <- function(x, w) sum(x * w) / sum(w)
smd <- function(w1, w0) sapply(covs, function(v)
  (wmean(g[[v]], w1) - wmean(g[[v]], w0)) / sd_pool[v])
## raw: everyone weight 1 in their own arm
w1_raw <- g$D; w0_raw <- 1 - g$D
## ATE matching: both arms reweighted to the cell shares among matched units
cellw <- setNames(rep(0, nlevels(g$cell)), levels(g$cell))
cellw[as.character(m$cell)] <- m$n / sum(m$n)
share1 <- setNames(rep(0, nlevels(g$cell)), levels(g$cell)); share1[as.character(m$cell)] <- m$n1
share0 <- setNames(rep(0, nlevels(g$cell)), levels(g$cell)); share0[as.character(m$cell)] <- m$n0
cw <- as.character(g$cell)
w1_ate <- g$D * cellw[cw] / pmax(share1[cw], 1)          # each treated unit: cell share / n1 in cell
w0_ate <- (1 - g$D) * cellw[cw] / pmax(share0[cw], 1)
## ATT matching: treated kept (matched cells), controls reweighted to treated cell shares
att_w <- setNames(rep(0, nlevels(g$cell)), levels(g$cell)); att_w[as.character(m$cell)] <- m$n1 / sum(m$n1)
w1_att <- g$D * att_w[cw] / pmax(share1[cw], 1)
w0_att <- (1 - g$D) * att_w[cw] / pmax(share0[cw], 1)
bal <- rbind(raw = smd(w1_raw, w0_raw), ATE_matched = smd(w1_ate, w0_ate), ATT_matched = smd(w1_att, w0_att))
print(round(bal, 3))
## sanity: the weighted difference in Y reproduces the estimates
cat("check ATE via weights:", round(wmean(g$Y, w1_ate) - wmean(g$Y, w0_ate), 3),
    " ATT:", round(wmean(g$Y, w1_att) - wmean(g$Y, w0_att), 3), "\n")

## ---- 5. cross-check with MatchIt exact matching ------------------------------
suppressMessages(library(MatchIt))
for (est in c("ATE", "ATT")) {
  mm <- matchit(D ~ female + black + pareduc_b + wordsum_b + exper_b, data = g,
                method = "exact", estimand = est)
  md <- match.data(mm)
  fit <- lm(Y ~ D, data = md, weights = weights)
  cat("MatchIt exact", est, ":", round(coef(fit)["D"], 3), "; matched n =", nrow(md), "\n")
}

## ---- 6. regression on the matched sample, and standard errors -------------
## the weights w1_ate/w0_ate reproduce the matched ATE (checked above); combine them
g$w_ate <- w1_ate + w0_ate
g$w_att <- w1_att + w0_att
m_ate <- g[g$w_ate > 0, ]; m_att <- g[g$w_att > 0, ]
## (a) weighted regression on D alone = the weighted difference in means
wls_d  <- lm(Y ~ D, data = m_ate, weights = w_ate)
## (b) add the coarsened covariates as continuous controls: bias correction for
##     the within-band imbalance the cells left behind
wls_dx <- lm(Y ~ D + pareduc + wordsum + exper, data = m_ate, weights = w_ate)
cat(sprintf("\nWLS Y ~ D with ATE weights: %.3f | + continuous pareduc, wordsum, exper: %.3f\n",
            coef(wls_d)["D"], coef(wls_dx)["D"]))
wls_att_d  <- lm(Y ~ D, data = m_att, weights = w_att)
wls_att_dx <- lm(Y ~ D + pareduc + wordsum + exper, data = m_att, weights = w_att)
cat(sprintf("WLS Y ~ D with ATT weights: %.3f | + continuous controls: %.3f\n",
            coef(wls_att_d)["D"], coef(wls_att_dx)["D"]))
## (c) analytic SE of the stratified estimator: sum over cells of weight^2 * (s1^2/n1 + s0^2/n0)
cellv <- aggregate(cbind(v1 = Y * D, v0 = Y * (1 - D)) ~ cell, g, function(v) var(v[v != 0]))  # placeholder
v1 <- tapply(g$Y[g$D == 1], g$cell[g$D == 1], var); v0 <- tapply(g$Y[g$D == 0], g$cell[g$D == 0], var)
mm <- m; mm$v1 <- v1[as.character(mm$cell)]; mm$v0 <- v0[as.character(mm$cell)]
mm$v1[is.na(mm$v1)] <- 0; mm$v0[is.na(mm$v0)] <- 0
se_ate <- sqrt(sum((mm$n / sum(mm$n))^2 * (mm$v1 / mm$n1 + mm$v0 / mm$n0)))
se_att <- sqrt(sum((mm$n1 / sum(mm$n1))^2 * (mm$v1 / mm$n1 + mm$v0 / mm$n0)))
cat(sprintf("analytic stratified SE: ATE %.3f, ATT %.3f (bootstrap above: %.3f, %.3f)\n",
            se_ate, se_att, sd(bs["ate", ]), sd(bs["att", ])))
## (d) the practical route: sandwich SE on the weighted regression, clustered by cell
suppressMessages(library(sandwich))
se_cl <- sqrt(vcovCL(wls_d, cluster = m_ate$cell)["D", "D"])
se_hc <- sqrt(vcovHC(wls_d, type = "HC1")["D", "D"])
cat(sprintf("WLS Y ~ D: naive SE %.3f, HC1 %.3f, clustered by cell %.3f\n",
            sqrt(vcov(wls_d)["D", "D"]), se_hc, se_cl))
