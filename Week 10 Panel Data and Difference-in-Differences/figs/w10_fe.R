## Week 10 -- fixed effects without time: students in schools (HSB, the Week 4 data).
## FE two ways (school dummies, demeaning), what variation FE uses, between estimator,
## random effects as partial demeaning, Hausman and the Mundlak (cluster-robust) test.
##   Rscript figs/w10_fe.R > figs/w10_fe.out   (run from the week folder)
## writes figs/fe_pooled.pdf, fe_dummies.pdf, fe_demeaned.pdf, fe_between.pdf,
##        fe_weights.pdf, re_theta.pdf
suppressMessages({library(fixest); library(lme4); library(plm)})
data(MathAchieve, package = "nlme"); data(MathAchSchool, package = "nlme")
hsb <- merge(as.data.frame(MathAchieve), as.data.frame(MathAchSchool)[, c("School", "Sector")], by = "School")
hsb$School <- factor(as.character(hsb$School))
hsb$catholic <- as.integer(hsb$Sector == "Catholic")
hsb$ses_bar <- ave(hsb$SES, hsb$School); hsb$y_bar <- ave(hsb$MathAch, hsb$School)
hsb$ses_dm <- hsb$SES - hsb$ses_bar;      hsb$y_dm <- hsb$MathAch - hsb$y_bar
N <- nrow(hsb); J <- nlevels(hsb$School); nj <- as.vector(table(hsb$School))
cat("students:", N, " schools:", J, " students per school: min", min(nj), "median", median(nj), "max", max(nj), "\n")

## ---- 1. where the variation in SES is -------------------------------------------
cat("\n## 1. SES: total = between + within (sums of squares)\n")
tss <- sum((hsb$SES - mean(hsb$SES))^2); bss <- sum((hsb$ses_bar - mean(hsb$SES))^2); wss <- sum(hsb$ses_dm^2)
print(round(c(total = tss, between = bss, within = wss, share_within = wss / tss), 3))
cat("same for math:\n")
tssy <- sum((hsb$MathAch - mean(hsb$MathAch))^2); wssy <- sum(hsb$y_dm^2)
print(round(c(share_within_math = wssy / tssy), 3))

## ---- 2. pooled, between, FE two ways ----------------------------------------------
cat("\n## 2. math on SES\n")
m_pool <- lm(MathAch ~ SES, hsb)
sch <- aggregate(cbind(MathAch, SES, catholic) ~ School, hsb, mean)
m_betw <- lm(MathAch ~ SES, sch)
m_lsdv <- lm(MathAch ~ SES + School, hsb)
m_dm_naive <- lm(y_dm ~ 0 + ses_dm, hsb)
f_fe <- feols(MathAch ~ SES | School, hsb, cluster = ~School)
f_pool <- feols(MathAch ~ SES, hsb, cluster = ~School)
pr <- function(m, v) c(coef(m)[v], sqrt(diag(vcov(m)))[v])
tab <- rbind(pooled = pr(m_pool, "SES"), pooled_cluster = c(coef(f_pool)["SES"], se(f_pool)["SES"]),
             between_160_means = pr(m_betw, "SES"),
             FE_dummies = pr(m_lsdv, "SES"), FE_demeaned_lm_naive = pr(m_dm_naive, "ses_dm"),
             FE_cluster = c(coef(f_fe)["SES"], se(f_fe)["SES"]))
colnames(tab) <- c("est", "se"); print(round(tab, 4))
cat("naive demeaned se / dummies se =", round(tab["FE_demeaned_lm_naive", 2] / tab["FE_dummies", 2], 4),
    "; sqrt((N-1-1)/(N-1-J)) =", round(sqrt((N - 1 - 1) / (N - 1 - J)), 4), " [lm thinks df = N-1, truth N-1-J]\n")
cat("intercepts: first 3 school dummies give alpha_j = ybar_j - b*xbar_j? check:\n")
al <- coef(m_lsdv)[1] + c(0, coef(m_lsdv)[grep("^School", names(coef(m_lsdv)))])
names(al) <- levels(hsb$School)
chk <- sch$MathAch - coef(m_lsdv)["SES"] * sch$SES
print(round(cbind(dummy = al[1:3], ybar_minus_bxbar = chk[1:3]), 4))
cat("pooled OLS = mix of within and between; the identity b_pool = (W b_W + B b_B)/(W + B) with student-weighted between:\n")
m_betw_w <- lm(y_bar ~ ses_bar, hsb)
bw <- coef(m_lsdv)["SES"]; bb <- coef(m_betw_w)["ses_bar"]
print(round(c(within = bw, between_student_weighted = bb, mix = (wss * bw + bss * bb) / tss, pooled = coef(m_pool)["SES"]), 4))

## ---- 3. FE = weighted average of school slopes -------------------------------------
cat("\n## 3. FE slope as a weighted average of the 160 school slopes\n")
S <- aggregate(cbind(sxy = ses_dm * y_dm, sxx = ses_dm^2) ~ School, hsb, sum)
S$b <- S$sxy / S$sxx; S$w <- S$sxx / sum(S$sxx); S$n <- as.vector(table(hsb$School)[as.character(S$School)])
cat("weighted mean of school slopes:", round(sum(S$w * S$b), 4), "; unweighted:", round(mean(S$b), 4),
    "; range of school slopes:", round(range(S$b), 2), "\n")
cat("largest and smallest weights (x 160, so 1 = an equal share):\n")
o <- order(S$w); print(round(cbind(S[c(head(o, 3), tail(o, 3)), c("n", "sxx", "b")], w_x160 = 160 * S$w[c(head(o, 3), tail(o, 3))]), 2))

## ---- 4. a school-level variable under FE -------------------------------------------
cat("\n## 4. Catholic under FE\n")
m_lsdv_c <- lm(MathAch ~ SES + School + catholic, hsb)
cat("coefficient on catholic with school dummies:", coef(m_lsdv_c)["catholic"], "(NA = collinear with the dummies)\n")

## ---- 5. random effects ---------------------------------------------------------------
cat("\n## 5. random intercept (ML), the Week 4 model\n")
r1 <- lmer(MathAch ~ SES + (1 | School), hsb, REML = FALSE)
r2 <- lmer(MathAch ~ SES + catholic + (1 | School), hsb, REML = FALSE)
vc <- as.data.frame(VarCorr(r1)); tau2 <- vc$vcov[1]; sig2 <- vc$vcov[2]
theta <- 1 - sqrt(sig2 / (sig2 + nj * tau2))
cat("tau2:", round(tau2, 3), " sigma2:", round(sig2, 3), "\n")
cat("theta_j = 1 - sqrt(sigma2/(sigma2 + n_j tau2)): min", round(min(theta), 3), "median", round(median(theta), 3),
    "max", round(max(theta), 3), "\n")
cat("theta at n_j = 14, 47, 67:", round(1 - sqrt(sig2 / (sig2 + c(14, 47, 67) * tau2)), 3), "\n")
cat("RE as OLS on quasi-demeaned data, school-specific theta_j:\n")
th <- theta[match(hsb$School, levels(hsb$School))]
q <- lm(I(MathAch - th * y_bar) ~ 0 + I(1 - th) + I(SES - th * ses_bar), hsb)
print(round(c(lmer = fixef(r1)["SES"], quasi_demeaned_OLS = coef(q)[2]), 4))
tab2 <- rbind(pooled = c(coef(lm(MathAch ~ SES + catholic, hsb))[2:3]),
              RE = fixef(r2)[2:3], FE = c(coef(m_lsdv)["SES"], NA))
cat("SES and Catholic:\n"); print(round(tab2, 3))
print(round(rbind(RE_se = sqrt(diag(vcov(r2)))[2:3]), 3))

## beta(theta) curve: one common theta from 0 (pooled) to 1 (FE)
bt <- function(t) coef(lm(I(MathAch - t * y_bar) ~ I(SES - t * ses_bar), hsb))[2]
tg <- seq(0, 1, by = 0.01); bg <- sapply(tg, bt)
cat("beta at theta = 0, 0.5, 0.9, 1:", round(sapply(c(0, .5, .9, 1), bt), 3), "\n")

## ---- 6. Hausman and Mundlak -----------------------------------------------------------
cat("\n## 6a. Hausman, by hand (SES only; conventional variances)\n")
d <- coef(m_lsdv)["SES"] - fixef(r1)["SES"]
vf <- vcov(m_lsdv)["SES", "SES"]; vr <- vcov(r1)["SES", "SES"]
H <- d^2 / (vf - vr)
print(round(c(b_FE = coef(m_lsdv)[["SES"]], b_RE = fixef(r1)[["SES"]], diff = d[[1]], se_FE = sqrt(vf), se_RE = sqrt(vr),
              var_diff = vf - vr, H = H[[1]], p = pchisq(H[[1]], 1, lower.tail = FALSE)), 5))
cat("plm::phtest (Swamy-Arora RE):\n")
ph <- pdata.frame(as.data.frame(hsb)[, c("School", "MathAch", "SES")], index = "School")
pf <- plm(MathAch ~ SES, ph, model = "within"); prr <- plm(MathAch ~ SES, ph, model = "random")
print(phtest(pf, prr))

cat("\n## 6b. Mundlak: add school-mean SES; test gamma = 0 with clustered SEs\n")
f_mund <- feols(MathAch ~ SES + ses_bar, hsb, cluster = ~School)
print(round(coeftable(f_mund), 4))
tg2 <- (coef(f_mund)["ses_bar"] / se(f_mund)["ses_bar"])^2
cat("Wald (clustered) chi2_1 = t^2 =", round(tg2, 2), " p =", signif(pchisq(tg2, 1, lower.tail = FALSE), 3), "\n")
r_m <- lmer(MathAch ~ SES + ses_bar + (1 | School), hsb, REML = FALSE)
cat("random intercept + school-mean SES: SES", round(fixef(r_m)["SES"], 4), " ses_bar", round(fixef(r_m)["ses_bar"], 4), "\n")
cat("between slope minus within slope (unweighted between):", round(coef(m_betw)["SES"] - coef(m_lsdv)["SES"], 4), "\n")

## ---- figures --------------------------------------------------------------------------
dukeblue <- rgb(1, 33, 105, maxColorValue = 255); accent <- rgb(200, 78, 0, maxColorValue = 255)
pick <- c("8854", "6808", "2336", "6469")
cols <- c(rgb(0, 114, 178, maxColorValue = 255), rgb(0, 158, 115, maxColorValue = 255),
          rgb(213, 94, 0, maxColorValue = 255), rgb(204, 121, 167, maxColorValue = 255))
s4 <- droplevels(subset(hsb, School %in% pick)); s4$School <- factor(s4$School, levels = pick)
s4$xd <- s4$SES - ave(s4$SES, s4$School); s4$yd <- s4$MathAch - ave(s4$MathAch, s4$School)
p4 <- lm(MathAch ~ SES, s4); l4 <- lm(MathAch ~ 0 + School + SES, s4); b4 <- coef(l4)["SES"]
cat("\n## figures: four schools", pick, "\n")
mm <- aggregate(cbind(SES, MathAch) ~ School, s4, mean); mm$n <- as.vector(table(s4$School)); print(mm, digits = 3)
cat("four-school pooled slope", round(coef(p4)[2], 2), " FE slope", round(b4, 2),
    " demeaned slope", round(coef(lm(yd ~ 0 + xd, s4)), 2), "\n")
print(round(coef(l4), 2))
cc <- cols[as.integer(s4$School)]
xl <- range(s4$SES); yl <- range(s4$MathAch)
leg <- function() legend("topleft", paste("school", pick), col = cols, pch = 19, bty = "n", cex = 0.8)
base <- function(...) {
  par(mar = c(3.6, 3.8, 0.6, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.9)
  plot(s4$SES, s4$MathAch, pch = 19, col = adjustcolor(cc, 0.65), cex = 0.8, xlab = "student SES",
       ylab = "math achievement", xlim = xl, ylim = yl, ...)
}
pdf("figs/fe_pooled.pdf", width = 4.6, height = 3.4, family = "Times")
base(); abline(p4, lwd = 2.5); leg()
text(xl[2], yl[1] + 1, sprintf("one line, slope %.2f", coef(p4)[2]), adj = c(1, 0), cex = 0.85)
dev.off()
pdf("figs/fe_dummies.pdf", width = 4.6, height = 3.4, family = "Times")
base()
for (k in 1:4) {xr <- range(s4$SES[s4$School == pick[k]])
  segments(xr[1], coef(l4)[k] + b4 * xr[1], xr[2], coef(l4)[k] + b4 * xr[2], col = cols[k], lwd = 2.5)}
points(mm$SES, mm$MathAch, pch = 4, cex = 2, lwd = 3, col = "black")
leg(); text(xl[2], yl[1] + 1, sprintf("four intercepts, one slope %.2f", b4), adj = c(1, 0), cex = 0.85)
dev.off()
pdf("figs/fe_demeaned.pdf", width = 4.6, height = 3.4, family = "Times")
par(mar = c(3.6, 3.8, 0.6, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.9)
plot(s4$xd, s4$yd, pch = 19, col = adjustcolor(cc, 0.65), cex = 0.8,
     xlab = "SES minus school mean", ylab = "math minus school mean")
abline(h = 0, v = 0, lty = 3, col = "gray50"); abline(0, b4, lwd = 2.5)
points(0, 0, pch = 4, cex = 2, lwd = 3)
leg(); text(par("usr")[2], par("usr")[3] + 1, sprintf("one line through 0, slope %.2f", b4), adj = c(1, 0), cex = 0.85)
dev.off()

pdf("figs/fe_between.pdf", width = 4.8, height = 3.4, family = "Times")
par(mar = c(3.6, 3.8, 0.6, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.9)
plot(sch$SES, sch$MathAch, pch = 19, col = adjustcolor(dukeblue, 0.6), cex = 0.8,
     xlab = "school mean SES", ylab = "school mean math")
abline(m_betw, lwd = 2.5, col = dukeblue)
abline(a = mean(sch$MathAch) - coef(m_lsdv)["SES"] * mean(sch$SES), b = coef(m_lsdv)["SES"], lwd = 2.5, lty = 2, col = accent)
legend("topleft", c(sprintf("between schools: slope %.2f", coef(m_betw)["SES"]),
                    sprintf("within schools (FE): slope %.2f", coef(m_lsdv)["SES"])),
       col = c(dukeblue, accent), lwd = 2.5, lty = c(1, 2), bty = "n", cex = 0.8)
dev.off()

pdf("figs/fe_weights.pdf", width = 4.8, height = 3.4, family = "Times")
par(mar = c(3.6, 3.8, 0.6, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.9)
plot(160 * S$w, S$b, pch = 19, col = adjustcolor(dukeblue, 0.6), cex = 0.8,
     xlab = "weight of the school (1 = equal share of 160)", ylab = "school's own SES slope")
abline(h = sum(S$w * S$b), lwd = 2.5, col = accent); abline(h = 0, lty = 3, col = "gray50")
text(par("usr")[2], sum(S$w * S$b) + 0.5, sprintf("FE = weighted mean = %.2f", sum(S$w * S$b)), adj = c(1, 0), col = accent, cex = 0.85)
dev.off()

pdf("figs/re_theta.pdf", width = 4.8, height = 3.2, family = "Times")
par(mar = c(3.6, 3.8, 0.6, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.9)
plot(tg, bg, type = "l", lwd = 2.5, col = dukeblue, xlab = expression(paste("share of the school mean subtracted, ", theta)),
     ylab = "SES coefficient")
pts <- c(0, median(theta), 1); pv <- c(bg[1], fixef(r1)["SES"], bg[length(bg)])
points(pts, pv, pch = 19, cex = 1.3, col = c("black", accent, "black"))
text(pts, pv, c("pooled OLS", "random effects", "fixed effects"), pos = c(4, 3, 3), cex = 0.85)
dev.off()
