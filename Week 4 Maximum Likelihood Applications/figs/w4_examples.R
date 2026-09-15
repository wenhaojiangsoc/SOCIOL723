## Week 4 slides: every real-data number and figure, from the GSS extract.
## Run from the week folder:  Rscript figs/w4_examples.R
suppressPackageStartupMessages({
  library(MASS); library(AER); library(pscl); library(nnet); library(brglm2)
  library(sandwich); library(marginaleffects)
})
gss <- readRDS("../Data/gss_earnings.rds")
n <- nrow(gss)
dukeblue <- rgb(1, 33, 105, maxColorValue = 255)
accent   <- rgb(200, 78, 0, maxColorValue = 255)
pr <- function(x, d = 3) print(round(x, d))

## ============ 1. Score test, one parameter, real data: childs ~ Poisson ====
y <- gss$childs; ybar <- mean(y); th0 <- 1.5
S  <- n * (ybar / th0 - 1);   I0 <- n / th0
LM <- S^2 / I0
W  <- n * (ybar - th0)^2 / ybar
LR <- 2 * n * (ybar * log(ybar / th0) - (ybar - th0))
cat("\n== Score test, Poisson childs: n", n, " ybar", round(ybar, 4), " theta0", th0, "\n")
cat(sprintf("S(theta0) = %.1f   I(theta0) = %.0f   LM = %.1f   W = %.1f   LR = %.1f\n", S, I0, LM, W, LR))

ll <- function(t) sum(dpois(y, t, log = TRUE))
grid <- seq(1.40, 1.80, length.out = 400)
lv <- sapply(grid, ll); lhat <- ll(ybar)
pdf("figs/score_childs.pdf", width = 5.6, height = 3.3)
par(mar = c(3.6, 3.8, 0.8, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.85)
plot(grid, lv - lhat, type = "l", lwd = 2.5, col = dukeblue,
     xlab = expression(theta), ylab = expression(l(theta) - l(hat(theta))), ylim = c(-40, 3))
abline(v = ybar, lty = 3, col = "gray50"); abline(v = th0, lty = 3, col = "gray50")
## tangent at theta0
l0 <- ll(th0) - lhat
segments(th0 - 0.07, l0 - 0.07 * S, th0 + 0.07, l0 + 0.07 * S, col = accent, lwd = 2.5)
points(th0, l0, pch = 16, col = accent)
text(th0, l0 - 8, sprintf("slope S(%.1f) = %.0f", th0, S), col = accent, adj = c(0.1, 1), cex = 0.9)
text(ybar, 1.5, expression(hat(theta) == bar(y)), adj = c(-0.1, 0), cex = 0.9)
text(th0, 1.5, expression(theta[0]), adj = c(1.2, 0), cex = 0.9)
dev.off()

## ============ 2. LR and BIC on nested logits: region block (Lab 3) ========
m_full <- glm(ba ~ pareduc + wordsum + female + black, data = gss, family = binomial)
m_reg  <- glm(ba ~ pareduc + wordsum + female + black + region, data = gss, family = binomial)
LR_reg <- 2 * as.numeric(logLik(m_reg) - logLik(m_full))
cat("\n== Region block: LR", round(LR_reg, 2), " df 8  p", signif(pchisq(LR_reg, 8, lower.tail = FALSE), 2), "\n")
cat("AIC no-region", round(AIC(m_full), 1), " with", round(AIC(m_reg), 1), " dAIC (no - with)", round(AIC(m_full) - AIC(m_reg), 1), "\n")
cat("BIC no-region", round(BIC(m_full), 1), " with", round(BIC(m_reg), 1), " dBIC (no - with)", round(BIC(m_full) - BIC(m_reg), 1),
    " check LR - 8 log n =", round(LR_reg - 8 * log(n), 1), "\n")
## black alone
m_nobl <- glm(ba ~ pareduc + wordsum + female, data = gss, family = binomial)
LR_bl <- 2 * as.numeric(logLik(m_full) - logLik(m_nobl))
cat("black: LR", round(LR_bl, 2), " dAIC(no-with)", round(AIC(m_nobl) - AIC(m_full), 2), " dBIC(no-with)", round(BIC(m_nobl) - BIC(m_full), 2), "\n")

## ============ 3. Logit: ba ~ pareduc + wordsum + female + black =============
cat("\n== Logit ba\n"); pr(coef(summary(m_full)))
cat("odds ratios:\n"); pr(exp(coef(m_full)))
m_lpm <- lm(ba ~ pareduc + wordsum + female + black, data = gss)
cat("LPM out of [0,1]:", sum(fitted(m_lpm) < 0 | fitted(m_lpm) > 1), "of", n, "\n")
cat("LPM coefs:\n"); pr(coef(m_lpm))
cat("AMEs (avg_slopes):\n"); print(avg_slopes(m_full))
cat("AME wordsum by hand:", round(mean(dlogis(predict(m_full)) * coef(m_full)["wordsum"]), 4), "\n")
cat("MEM wordsum:", round(dlogis(sum(colMeans(model.matrix(m_full)) * coef(m_full))) * coef(m_full)["wordsum"], 4), "\n")
cat("predicted Pr(ba) by wordsum (observed-value):\n")
print(avg_predictions(m_full, variables = list(wordsum = c(2, 4, 6, 8, 10))))
cat("predicted by female, black:\n"); print(avg_predictions(m_full, variables = "black"))
## probit
m_probit <- glm(ba ~ pareduc + wordsum + female + black, data = gss, family = binomial(link = "probit"))
cat("logit/probit ratios:\n"); pr(coef(m_full) / coef(m_probit))
cat("probit AMEs:\n"); print(avg_slopes(m_probit))
## figure: observed share with BA by wordsum, with logit and LPM fits (bivariate)
m1 <- glm(ba ~ wordsum, data = gss, family = binomial); l1 <- lm(ba ~ wordsum, data = gss)
obs <- tapply(gss$ba, gss$wordsum, mean); cnt <- table(gss$wordsum)
pdf("figs/logit_wordsum.pdf", width = 5.6, height = 3.3)
par(mar = c(3.6, 3.8, 0.8, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.85)
plot(as.numeric(names(obs)), obs, pch = 16, cex = 0.4 + 1.6 * sqrt(cnt / max(cnt)), col = adjustcolor(dukeblue, 0.7),
     xlab = "vocabulary score (wordsum)", ylab = "share with a bachelor's degree", ylim = c(-0.05, 1.05), xlim = c(-0.5, 10.5))
xx <- seq(-0.5, 10.5, length.out = 200)
lines(xx, plogis(coef(m1)[1] + coef(m1)[2] * xx), col = dukeblue, lwd = 2.5)
lines(xx, coef(l1)[1] + coef(l1)[2] * xx, col = accent, lwd = 2, lty = 2)
abline(h = c(0, 1), col = "gray70", lty = 3)
legend("topleft", c("logit", "linear probability"), col = c(dukeblue, accent), lwd = 2, lty = c(1, 2), bty = "n", cex = 0.85)
dev.off()
cat("bivariate logit:", round(coef(m1), 3), " LPM:", round(coef(l1), 3), "\n")

## ============ 4. Scale not identified: nested logits =======================
m_a <- glm(ba ~ pareduc, data = gss, family = binomial)
m_b <- glm(ba ~ pareduc + wordsum, data = gss, family = binomial)
cat("\n== pareduc coef: alone", round(coef(m_a)["pareduc"], 3), " with wordsum", round(coef(m_b)["pareduc"], 3),
    " cor(pareduc, wordsum)", round(cor(gss$pareduc, gss$wordsum), 3), "\n")
cat("AME pareduc alone / with:", round(avg_slopes(m_a)$estimate, 4), round(avg_slopes(m_b, variables = "pareduc")$estimate, 4), "\n")

## ============ 5. Ordered logit: degree ======================================
m_ord <- polr(degree ~ pareduc + wordsum + female + black, data = gss, Hess = TRUE)
cat("\n== Ordered logit degree\n"); pr(coef(summary(m_ord)))
cat("cutpoints:", round(m_ord$zeta, 3), "\n")
## proportional odds check: separate binary logits at each cut
cuts <- levels(gss$degree)[-5]
for (k in 1:4) {
  yk <- as.integer(as.integer(gss$degree) > k)
  mk <- glm(yk ~ pareduc + wordsum + female + black, data = gss, family = binomial)
  cat(sprintf("cut > %-15s  pareduc %.3f  wordsum %.3f  female %.3f  black %.3f\n", cuts[k],
              coef(mk)["pareduc"], coef(mk)["wordsum"], coef(mk)["female"], coef(mk)["black"]))
}
cat("predicted degree distribution at wordsum 4 vs 8 (observed-value):\n")
print(avg_predictions(m_ord, variables = list(wordsum = c(4, 8))))
## multinomial for contrast
m_mn <- multinom(degree ~ pareduc + wordsum + female + black, data = gss, trace = FALSE)
cat("multinomial: params", length(coef(m_mn)), " ordered: params", length(coef(m_ord)) + length(m_ord$zeta), "\n")
cat("multinomial logLik", round(as.numeric(logLik(m_mn)), 1), " ordered logLik", round(as.numeric(logLik(m_ord)), 1),
    " LR (ordered restricted)", round(2 * (as.numeric(logLik(m_mn)) - as.numeric(logLik(m_ord))), 1), " df", length(coef(m_mn)) - length(coef(m_ord)) - length(m_ord$zeta), "\n")
cat("multinomial coefs (rows = category vs < HS):\n"); pr(coef(m_mn))

## ============ 6. Poisson: childs ===========================================
f_c <- childs ~ educ + female + black + exper + evermar
m_pois <- glm(f_c, data = gss, family = poisson)
cat("\n== Poisson childs\n"); pr(coef(summary(m_pois)))
cat("exp(coef):\n"); pr(exp(coef(m_pois)))
cat("mean", round(mean(y), 3), " var", round(var(y), 3), "\n")
dt <- dispersiontest(m_pois, trafo = 1); cat("dispersiontest: z", round(dt$statistic, 2), " alpha", round(dt$estimate, 3), " p", signif(dt$p.value, 2), "\n")
m_qp <- glm(f_c, data = gss, family = quasipoisson)
m_nb <- glm.nb(f_c, data = gss)
cat("quasi dispersion phi:", round(summary(m_qp)$dispersion, 3), "  NB theta:", round(m_nb$theta, 2), " se", round(m_nb$SE.theta, 2), "\n")
se_tab <- cbind(poisson = sqrt(diag(vcov(m_pois))), quasi = sqrt(diag(vcov(m_qp))),
                negbin = sqrt(diag(vcov(m_nb)))[names(coef(m_pois))], robust = sqrt(diag(vcovHC(m_pois, type = "HC0"))))
cat("SE table:\n"); pr(se_tab, 4)
cat("NB vs Poisson: LR", round(2 * (as.numeric(logLik(m_nb)) - as.numeric(logLik(m_pois))), 2),
    " AIC pois", round(AIC(m_pois), 1), " nb", round(AIC(m_nb), 1), " BIC pois", round(BIC(m_pois), 1), " nb", round(BIC(m_nb), 1), "\n")
## zeros
obs_p <- as.numeric(table(factor(y, levels = 0:8))) / n
pois_p <- sapply(0:8, function(k) mean(dpois(k, fitted(m_pois))))
nb_p   <- sapply(0:8, function(k) mean(dnbinom(k, mu = fitted(m_nb), size = m_nb$theta)))
m_hur <- hurdle(f_c, data = gss, dist = "poisson")   # count part Poisson, as on the slide
m_zip <- zeroinfl(f_c, data = gss, dist = "negbin")
hur_p <- colMeans(predict(m_hur, type = "prob")[, 1:9])
zip_p <- colMeans(predict(m_zip, type = "prob")[, 1:9])
cat("share of zeros: observed", round(obs_p[1], 3), " poisson", round(pois_p[1], 3), " nb", round(nb_p[1], 3), " hurdle", round(hur_p[1], 3), " zinb", round(zip_p[1], 3), "\n")
cat("logLik: pois", round(as.numeric(logLik(m_pois)), 1), " nb", round(as.numeric(logLik(m_nb)), 1), " hurdle", round(as.numeric(logLik(m_hur)), 1), " zinb", round(as.numeric(logLik(m_zip)), 1), "\n")
cat("AIC: nb", round(AIC(m_nb), 1), " hurdle", round(AIC(m_hur), 1), " zinb", round(AIC(m_zip), 1), "\n")
cat("BIC: nb", round(BIC(m_nb), 1), " hurdle", round(BIC(m_hur), 1), " zinb", round(BIC(m_zip), 1), "\n")
cat("hurdle zero part (logit for any child):\n"); pr(coef(summary(m_hur))$zero)
cat("hurdle count part:\n"); pr(coef(summary(m_hur))$count)
pdf("figs/childs_dist.pdf", width = 5.6, height = 3.3)
par(mar = c(3.6, 3.8, 0.8, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.85)
bp <- barplot(obs_p, names.arg = 0:8, col = "gray85", border = NA, ylim = c(0, 0.36),
              xlab = "number of children", ylab = "share of respondents")
lines(bp, pois_p, type = "b", pch = 16, col = accent, lwd = 2)
lines(bp, nb_p, type = "b", pch = 17, col = dukeblue, lwd = 2, lty = 2)
lines(bp, hur_p, type = "b", pch = 15, col = "darkgreen", lwd = 2, lty = 3)
legend("topright", c("observed", "Poisson", "negative binomial", "hurdle"), fill = c("gray85", NA, NA, NA), border = NA,
       col = c(NA, accent, dukeblue, "darkgreen"), lwd = c(NA, 2, 2, 2), pch = c(NA, 16, 17, 15), lty = c(NA, 1, 2, 3), bty = "n", cex = 0.8)
dev.off()

## ============ 7. Separation: a graduate-degree dummy predicts ba perfectly ==
gss$grad <- as.integer(gss$degree == "Graduate")
print(table(grad = gss$grad, ba = gss$ba))
m_sep <- suppressWarnings(glm(ba ~ pareduc + wordsum + grad, data = gss, family = binomial))
cat("\n== Separation, glm():\n"); pr(coef(summary(m_sep)))
m_firth <- glm(ba ~ pareduc + wordsum + grad, data = gss, family = binomial, method = "brglmFit")
cat("Firth:\n"); pr(coef(summary(m_firth)))
m0 <- glm(ba ~ pareduc + wordsum, data = gss, family = binomial)
cat("LR for grad:", round(2 * (as.numeric(logLik(m_sep)) - as.numeric(logLik(m0))), 1), "\n")
cat("\nDONE\n")

## ============ 8. Newton by hand: logit ba ~ wordsum ========================
X2 <- cbind(1, gss$wordsum); yb <- gss$ba
b <- c(0, 0); cat("\n== Newton by hand, ba ~ wordsum\n")
for (t in 0:6) {
  eta <- as.vector(X2 %*% b); pp <- plogis(eta)
  ll  <- sum(yb * eta - log1p(exp(eta)))
  S   <- as.vector(crossprod(X2, yb - pp))
  I   <- crossprod(X2 * sqrt(pp * (1 - pp)))
  step <- as.vector(solve(I, S))
  cat(sprintf("t=%d  b0=%7.3f  b1=%6.3f  loglik=%9.2f  S=(%8.1f,%8.1f)  step=(%6.3f,%6.3f)\n",
              t, b[1], b[2], ll, S[1], S[2], step[1], step[2]))
  b <- b + step
}
cat("glm:", round(coef(glm(ba ~ wordsum, data = gss, family = binomial)), 3), "\n")

## ============ 9. Multinomial predictions at wordsum 4 vs 8 =================
cat("\n== multinomial predicted distribution, wordsum 4 vs 8\n")
print(avg_predictions(m_mn, variables = list(wordsum = c(4, 8))))

## ============ 10. figure: theta on the y axis ==============================
pdf("figs/logit_wordsum.pdf", width = 5.6, height = 3.3)
par(mar = c(3.6, 3.8, 0.8, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.85)
plot(as.numeric(names(obs)), obs, pch = 16, cex = 0.4 + 1.6 * sqrt(cnt / max(cnt)), col = adjustcolor(dukeblue, 0.7),
     xlab = "vocabulary score (wordsum)", ylab = expression(theta[i] == Pr(degree)), ylim = c(-0.05, 1.05), xlim = c(-0.5, 10.5))
lines(xx, plogis(coef(m1)[1] + coef(m1)[2] * xx), col = dukeblue, lwd = 2.5)
lines(xx, coef(l1)[1] + coef(l1)[2] * xx, col = accent, lwd = 2, lty = 2)
abline(h = c(0, 1), col = "gray70", lty = 3)
legend("topleft", c(expression(theta[i] == Lambda(beta[0] + beta[1] * x[i])), expression(theta[i] == beta[0] + beta[1] * x[i])),
       col = c(dukeblue, accent), lwd = 2, lty = c(1, 2), bty = "n", cex = 0.85)
dev.off()

## ============ 11. figure: predicted probabilities across parental education, ASR style
pe <- seq(6, 20, by = 1)
pr_f <- avg_predictions(m_full, variables = list(pareduc = pe), by = "pareduc")
pdf("figs/pred_pareduc.pdf", width = 5.6, height = 3.3)
par(mar = c(3.6, 3.8, 0.8, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.85)
plot(pr_f$pareduc, pr_f$estimate, type = "n", ylim = c(0, 1),
     xlab = "parents' education (years)", ylab = "predicted Pr(bachelor's degree)")
polygon(c(pr_f$pareduc, rev(pr_f$pareduc)), c(pr_f$conf.low, rev(pr_f$conf.high)), col = adjustcolor(dukeblue, 0.15), border = NA)
lines(pr_f$pareduc, pr_f$estimate, col = dukeblue, lwd = 2.5)
abline(v = c(12, 16), col = "gray70", lty = 3)
dev.off()
cat("\n== predicted Pr(degree) by parents' education (observed-value):\n")
print(as.data.frame(pr_f)[pr_f$pareduc %in% c(8, 12, 16, 20), c("pareduc", "estimate", "conf.low", "conf.high")])
cat("AME pareduc:", round(avg_slopes(m_full, variables = "pareduc")$estimate, 4), "\n")

## ============ 12. Multinomial on a nominal outcome: four census regions =====
gss$region4 <- factor(dplyr::case_when(
  gss$region %in% c("New England","Middle Atlantic") ~ "Northeast",
  gss$region %in% c("E. North Central","W. North Central") ~ "Midwest",
  gss$region %in% c("South Atlantic","E. South Central","W. South Central") ~ "South",
  TRUE ~ "West"), levels = c("Midwest","Northeast","South","West"))
m_reg4 <- multinom(region4 ~ educ + black + female + age, data = gss, trace = FALSE)
cat("\n== Multinomial: region4 on educ, black, female, age; exp(coef):\n"); pr(exp(coef(m_reg4)), 2)
print(avg_predictions(m_reg4, variables = "black"))
print(avg_predictions(m_reg4, variables = list(educ = c(12, 16))))
m_reg0 <- multinom(region4 ~ 1, data = gss, trace = FALSE)
cat("LR all covariates:", round(2 * (as.numeric(logLik(m_reg4)) - as.numeric(logLik(m_reg0))), 1), "\n")
## ordered-logit gates: two example people
b <- coef(m_ord); z <- m_ord$zeta
for (x in list(A = c(12, 5, 0, 0), B = c(16, 8, 1, 0))) {
  xb <- sum(b * x); cat("X beta =", round(xb, 2), " probs:", round(diff(c(0, plogis(z - xb), 1)), 2), "\n")
}

## ============ 13. Interaction on the probability scale ======================
m_int <- glm(ba ~ pareduc * female + wordsum + black, data = gss, family = binomial)
cat("\n== Interaction pareduc x female\n"); pr(coef(summary(m_int)))
print(avg_slopes(m_full, variables = "pareduc", by = "female"))   # no product
print(avg_slopes(m_int,  variables = "pareduc", by = "female"))   # with product
