## Week 10 -- the panel section built slowly on wooldridge::wagepan (545 men, 1980-87):
## pooled -> first differences (2x2) -> man / year / both effects over calendar time -> two-way FE.
##   Rscript figs/w10_panel_slow.R > figs/w10_panel_slow.out
##   writes figs/pan_did_2x2.pdf, figs/pan_intercepts.pdf, figs/pan_intercepts_tw.pdf, figs/pan_paths_man.pdf, figs/pan_paths_year.pdf, figs/pan_twoway_paths.pdf, figs/pan_demeaned.pdf, figs/pan_time.pdf
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
if (!requireNamespace("wooldridge", quietly = TRUE))
  install.packages("wooldridge", lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
suppressMessages(library(fixest)); setFixest_notes(FALSE)
data(wagepan, package = "wooldridge"); w <- wagepan

## ---- 0. the data ---------------------------------------------------------------------
cat("## 0. data\nmen", length(unique(w$nr)), "; years", paste(range(w$year), collapse = "-"), "; rows", nrow(w), "\n")
pat <- tapply(w$married, w$nr, mean)
cat("never married", sum(pat == 0), "; always married", sum(pat == 1), "; status changes", sum(pat > 0 & pat < 1), "\n")
cat("share married by year:\n"); print(round(tapply(w$married, w$year, mean), 3))
cat("mean log wage by year:\n"); print(round(tapply(w$lwage, w$year, mean), 3))

## ---- 1. pooled ----------------------------------------------------------------------------
cat("\n## 1. pooled OLS, married only, clustered by man\n")
print(round(coeftable(feols(lwage ~ married, w, cluster = ~nr)), 4))

## ---- 2. first differences, 1980 -> 1987 ---------------------------------------------------
cat("\n## 2. first differences between 1980 and 1987\n")
d <- merge(w[w$year == 1980, c("nr", "lwage", "married")], w[w$year == 1987, c("nr", "lwage", "married")], by = "nr")
d$dy <- d$lwage.y - d$lwage.x; d$dD <- d$married.y - d$married.x
cat("change in married:\n"); print(table(d$dD))
cat("mean wage growth by change in married:\n"); print(round(tapply(d$dy, d$dD, mean), 3))
cat("no intercept:\n"); print(round(summary(lm(dy ~ 0 + dD, d))$coef, 4))
cat("with intercept (common growth):\n"); print(round(summary(lm(dy ~ dD, d))$coef, 4))
## slide version: drop the 17 men who left marriage, so dD is 0 (control) or 1 (treated): a 2x2 DiD
d01 <- d[d$dD >= 0, ]
cat("with intercept, dD in {0, 1}, n =", nrow(d01), ":\n"); print(round(summary(lm(dy ~ dD, d01))$coef, 4))
## the 2x2 picture: mean log wage by group, 1980 and 1987; dashed = married group with the control group's growth
m2 <- rbind(married = c(mean(d01$lwage.x[d01$dD == 1]), mean(d01$lwage.y[d01$dD == 1])),
            no_change = c(mean(d01$lwage.x[d01$dD == 0]), mean(d01$lwage.y[d01$dD == 0])))
colnames(m2) <- c("1980", "1987"); cat("2x2 group means:\n"); print(round(m2, 3))
## every man as a faint line from his 1980 to his 1987 wage (height = his own level, slope = his growth),
## jittered sideways by the same amount at both ends; bold = group means; dashed = married with no-change growth
set.seed(723); jx <- runif(nrow(d01), -0.35, 0.35)
gcol <- ifelse(d01$dD == 1, "firebrick", "navy"); gcol_a <- adjustcolor(gcol, alpha.f = 0.18)
pdf("figs/pan_did_2x2.pdf", width = 6.4, height = 4.4, family = "Times")
par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.2, cex.axis = 1.1)
plot(NA, xlim = c(1978.0, 1991.3), ylim = range(c(d01$lwage.x, d01$lwage.y)) + c(0, 0.5), xaxt = "n",
     xlab = "year", ylab = "log hourly wage")
axis(1, at = c(1980, 1987))
segments(1980 + jx, d01$lwage.x, 1987 + jx, d01$lwage.y, col = gcol_a, lwd = 0.6)
points(c(1980 + jx, 1987 + jx), c(d01$lwage.x, d01$lwage.y), pch = 16, cex = 0.35, col = c(gcol_a, gcol_a))
cf0 <- m2["married", 1] + diff(m2["no_change", ])
for (r in list(m2["married", ], m2["no_change", ], c(m2["married", 1], cf0))) lines(c(1980, 1987), r, lwd = 7, col = "white")
lines(c(1980, 1987), m2["married", ], lwd = 3.5, col = "firebrick")
lines(c(1980, 1987), m2["no_change", ], lwd = 3.5, col = "navy")
cf <- m2["married", 1] + diff(m2["no_change", ])
lines(c(1980, 1987), c(m2["married", 1], cf), lty = 2, lwd = 2.5, col = "firebrick")
text(1987.45, m2["married", 2] + 0.35, sprintf("married (251): %.3f", m2["married", 2]), pos = 4, cex = 0.9, col = "firebrick")
text(1987.45, cf + 0.02, sprintf("without marriage: %.3f", cf), pos = 4, cex = 0.9, col = "firebrick")
text(1987.45, m2["no_change", 2] - 0.3, sprintf("no change (277): %.3f", m2["no_change", 2]), pos = 4, cex = 0.9, col = "navy")
text(1979.6, m2[, 1] + c(0.2, -0.2), sprintf("%.3f", m2[, 1]), pos = 2, cex = 0.9, col = c("firebrick", "navy"))
legend("topleft", c("married between 1980 and 1987 (thin: one man)", "no change in status (thin: one man)",
       "married, with the no-change growth"), col = c("firebrick", "navy", "firebrick"), lwd = c(3.5, 3.5, 2.5),
       lty = c(1, 1, 2), bty = "n", cex = 0.95)
dev.off()
## the 2x2 sample in long form (528 men x 1980, 1987): pooled, man + year dummies, double demeaning
w2 <- w[w$year %in% c(1980, 1987) & w$nr %in% d01$nr, ]
dd2 <- function(v) v - ave(v, w2$nr) - ave(v, w2$year) + mean(v)
cat("2x2 sample: men", length(unique(w2$nr)), "; rows", nrow(w2), "\n")
print(round(rbind(pooled = coeftable(feols(lwage ~ married, w2, cluster = ~nr))["married", 1:2],
                  man_year_dummies = coeftable(feols(lwage ~ married | nr + year, w2, cluster = ~nr))["married", 1:2],
                  double_demeaned = c(coef(lm(dd2(w2$lwage) ~ 0 + dd2(w2$married)))[[1]], NA)), 4))

## ---- 3. man intercepts, all eight years: dummies = demeaning ---------------------------------
cat("\n## 3. man effects only\n")
fe1 <- feols(lwage ~ married | nr, w, cluster = ~nr)
w$yd <- w$lwage - ave(w$lwage, w$nr); w$Dd <- w$married - ave(w$married, w$nr)
print(round(c(dummies_lm = coef(lm(lwage ~ married + factor(nr), w))[["married"]],
              demeaned_lm = coef(lm(yd ~ 0 + Dd, w))[["Dd"]], feols = coef(fe1)[["married"]],
              se_cluster = se(fe1)[["married"]]), 4))
cat("men with Dd = 0 in every year (status never changes):", sum(tapply(w$Dd, w$nr, function(v) all(v == 0))), "\n")

## ---- 4. two-way: man and year effects; double demeaning ----------------------------------------
cat("\n## 4. man and year effects\n")
fe2 <- feols(lwage ~ married | nr + year, w, cluster = ~nr)
dd <- function(v) v - ave(v, w$nr) - ave(v, w$year) + mean(v)
print(round(c(dummies_lm = coef(lm(lwage ~ married + factor(nr) + factor(year), w))[["married"]],
              double_demeaned = coef(lm(dd(w$lwage) ~ 0 + dd(w$married)))[[1]],
              feols = coef(fe2)[["married"]], se_cluster = se(fe2)[["married"]]), 4))

## ---- 5. four men: separate intercepts, then demeaned ------------------------------------------
mw <- tapply(w$lwage, w$nr, mean); mp <- tapply(w$married, w$nr, function(v) paste(v, collapse = ""))
## smooth wage paths only (residual sd from the man's own line in year below the median), then a level quantile
rs <- tapply(seq_len(nrow(w)), w$nr, function(ix) sd(resid(lm(w$lwage[ix] ~ w$year[ix]))))
pick <- function(ids, q) { ids <- ids[rs[ids] < median(rs)]; ids[which.min(abs(mw[ids] - quantile(mw[ids], q)))] }
always <- names(pat)[pat == 1]; never <- names(pat)[pat == 0]
once4 <- names(mp)[mp == "00001111"]; once3 <- names(mp)[mp == "00011111"]
men <- c(always = pick(always, 0.9), switch1 = pick(once4, 0.55), switch2 = pick(once3, 0.3), never = pick(never, 0.1))
s <- w[w$nr %in% men, ]; s$who <- names(men)[match(s$nr, men)]
b_fe <- coef(lm(lwage ~ married + factor(nr), s))[["married"]]; b_po <- coef(lm(lwage ~ married, s))
cat("\n## 5. four men", men, ": pooled slope", round(b_po[2], 3), "; within slope", round(b_fe, 3), "\n")
cols <- c(always = "firebrick", switch1 = "darkorange", switch2 = "darkgreen", never = "navy")
lab <- c(always = "married all 8 years", switch1 = "marries in 1984", switch2 = "marries in 1983", never = "never married")
jit <- (s$year - 1983.5) * 0.025
pdf("figs/pan_intercepts.pdf", width = 6.4, height = 4.4, family = "Times")
par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.2, cex.axis = 1.1)
plot(s$married + jit, s$lwage, pch = 19, cex = 0.9, col = cols[s$who], xlim = c(-0.25, 1.25),
     xaxt = "n", xlab = "married", ylab = "log hourly wage")
axis(1, at = c(0, 1), labels = c("no", "yes"))
for (k in names(men)) { a <- mean(s$lwage[s$who == k]) - b_fe * mean(s$married[s$who == k])
  segments(-0.2, a - 0.2 * b_fe, 1.2, a + 1.2 * b_fe, col = cols[k], lwd = 2.5) }
abline(b_po, lty = 2, lwd = 2.5)
legend("topleft", c(lab, sprintf("pooled line, slope %.2f", b_po[2])), col = c(cols, "black"),
       lwd = 2.5, lty = c(1, 1, 1, 1, 2), bty = "n", cex = 1.05)
dev.off()
## four men with year effects out: log wage minus that year's mean over all 545 men. With years out the
## 1983 marrier above has a negative within slope (-0.12), so swap him for the smooth 1983 marrier whose
## four-man within slope is closest to the full two-way estimate (only switchers move the within slope)
w$ly <- w$lwage - ave(w$lwage, w$year)
b_all <- coef(fe2)[["married"]]
c3 <- once3[rs[once3] < median(rs)]
gap <- sapply(c3, function(id) { ss <- w[w$nr %in% c(men[-3], id), ]
  abs(coef(lm(ly ~ married + factor(nr), ss))[["married"]] - b_all) })
men_tw <- men; men_tw[["switch2"]] <- c3[which.min(gap)]
t4 <- w[w$nr %in% men_tw, ]; t4$who <- names(men_tw)[match(t4$nr, men_tw)]
jit4 <- (t4$year - 1983.5) * 0.025
cat("four men, years out:", men_tw, "\n")
b_tw <- coef(lm(ly ~ married + factor(nr), t4))[["married"]]; b_pt <- coef(lm(ly ~ married, t4))
cat("four men, year means out: pooled slope", round(b_pt[2], 3), "; within slope", round(b_tw, 3), "\n")
pdf("figs/pan_intercepts_tw.pdf", width = 6.4, height = 4.4, family = "Times")
par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.2, cex.axis = 1.1)
plot(t4$married + jit4, t4$ly, pch = 19, cex = 0.9, col = cols[t4$who], xlim = c(-0.25, 1.25),
     ylim = range(t4$ly) + c(0, 0.35), xaxt = "n", xlab = "married", ylab = "log wage minus the year's mean")
axis(1, at = c(0, 1), labels = c("no", "yes"))
for (k in names(men)) { a <- mean(t4$ly[t4$who == k]) - b_tw * mean(t4$married[t4$who == k])
  segments(-0.2, a - 0.2 * b_tw, 1.2, a + 1.2 * b_tw, col = cols[k], lwd = 2.5) }
abline(b_pt, lty = 2, lwd = 2.5)
legend("topleft", c(lab, sprintf("pooled line, slope %.2f", b_pt[2])), col = c(cols, "black"),
       lwd = 2.5, lty = c(1, 1, 1, 1, 2), bty = "n", cex = 1.05)
dev.off()
## man effects only, married on the x axis, for the same four men as the calendar-time figures:
## each man's own line (common within slope) vs the pooled line
b_m4 <- coef(lm(lwage ~ married + factor(nr), t4))[["married"]]; b_p4 <- coef(lm(lwage ~ married, t4))
cat("four men (calendar-time set), no year effects: pooled slope", round(b_p4[2], 3), "; within slope", round(b_m4, 3), "\n")
pdf("figs/pan_man_lines.pdf", width = 6.4, height = 4.4, family = "Times")
par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.2, cex.axis = 1.1)
plot(t4$married + jit4, t4$lwage, pch = 19, cex = 0.9, col = cols[t4$who], xlim = c(-0.25, 1.25),
     ylim = range(t4$lwage) + c(0, 0.45), xaxt = "n", xlab = "married", ylab = "log hourly wage")
axis(1, at = c(0, 1), labels = c("no", "yes"))
for (k in names(men_tw)) { a <- mean(t4$lwage[t4$who == k]) - b_m4 * mean(t4$married[t4$who == k])
  segments(-0.2, a - 0.2 * b_m4, 1.2, a + 1.2 * b_m4, col = cols[k], lwd = 2.5) }
abline(b_p4, lty = 2, lwd = 2.5)
legend("topleft", c(lab, sprintf("pooled line, slope %.2f", b_p4[2])), col = c(cols, "black"),
       lwd = 2.5, lty = c(1, 1, 1, 1, 2), bty = "n", cex = 1.0, ncol = 2)
dev.off()
## the same four men over calendar time, three models fit on all 545 men (same axes in all three figures):
##   man effects only   alpha_i + beta D_it           -> pan_paths_man.pdf
##   year effects only  a + lambda_t + beta D_it      -> pan_paths_year.pdf
##   both               alpha_i + lambda_t + beta D_it -> pan_twoway_paths.pdf
## dashed = the same man unmarried, so the solid-dashed gap after marriage is beta
fey <- feols(lwage ~ married | year, w, cluster = ~nr)
cat("year effects only: beta", round(coef(fey)[["married"]], 4), "(", round(se(fey)[["married"]], 4), ")\n")
key <- function(f, v) f[as.character(v)]
paths <- list(
  man  = list(fit = key(fixef(fe1)$nr, t4$nr) + coef(fe1)[["married"]] * t4$married,
              base = key(fixef(fe1)$nr, t4$nr), file = "figs/pan_paths_man.pdf"),
  year = list(fit = key(fixef(fey)$year, t4$year) + coef(fey)[["married"]] * t4$married,
              base = key(fixef(fey)$year, t4$year), file = "figs/pan_paths_year.pdf"),
  both = list(fit = key(fixef(fe2)$nr, t4$nr) + key(fixef(fe2)$year, t4$year) + coef(fe2)[["married"]] * t4$married,
              base = key(fixef(fe2)$nr, t4$nr) + key(fixef(fe2)$year, t4$year), file = "figs/pan_twoway_paths.pdf"))
lam <- fixef(fe2)$year; cat("year effects (both):", round(lam - lam[1], 3), "\n")
yl <- range(t4$lwage, unlist(lapply(paths, `[[`, "fit"))) + c(0, 0.45)
for (m in paths) {
  t4$fit <- m$fit; t4$base <- m$base
  pdf(m$file, width = 6.4, height = 4.4, family = "Times")
  par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.2, cex.axis = 1.1)
  plot(t4$year, t4$lwage, pch = ifelse(t4$married == 1, 19, 1), cex = 0.9, col = cols[t4$who],
       ylim = yl, xlim = c(1980, 1987.6), xaxt = "n", xlab = "year", ylab = "log hourly wage")
  axis(1, at = 1980:1987)
  if (identical(m$file, "figs/pan_paths_year.pdf")) {
    ## year effects only: every man has the same line, so draw it once (married and unmarried) in black
    yrs <- sort(unique(t4$year)); lt <- key(fixef(fey)$year, yrs)
    lines(yrs, lt + coef(fey)[["married"]], lwd = 2.5); lines(yrs, lt, lwd = 2.5, lty = 2)
    cat("year effects only, lambda_t - lambda_1980:", round(lt - lt[1], 3), "\n")
    arrows(1987.45, lt[1], 1987.45, lt[8], length = 0.07, lwd = 1.5, col = "gray30")
    segments(1980, lt[1], 1987.45, lt[1], lty = 3, col = "gray50")
    text(1987.35, lt[1] + 0.12, sprintf("common growth\n1980-87: +%.2f", lt[8] - lt[1]), pos = 2, cex = 0.9, col = "gray20")
    legend("topleft", c(lab, "every man, married", "every man, unmarried"), col = c(cols, "black", "black"),
           pch = c(19, 19, 19, 19, NA, NA), lwd = c(NA, NA, NA, NA, 2.5, 2.5), lty = c(NA, NA, NA, NA, 1, 2),
           bty = "n", cex = 1.0, ncol = 2)
  } else {
    for (k in names(men_tw)) { r <- t4[t4$who == k, ]; r <- r[order(r$year), ]
      lines(r$year, r$fit, col = cols[k], lwd = 2.5)
      if (any(r$married == 0) && any(r$married == 1)) lines(r$year, r$base, col = cols[k], lwd = 1.5, lty = 2) }
    legend("topleft", c(lab, "dashed: same man, unmarried"), col = c(cols, "gray40"), lwd = c(2.5, 2.5, 2.5, 2.5, 1.5),
           lty = c(1, 1, 1, 1, 2), bty = "n", cex = 1.0, ncol = 2)
  }
  dev.off()
}
s$yd <- s$lwage - ave(s$lwage, s$nr); s$Dd <- s$married - ave(s$married, s$nr)
pdf("figs/pan_demeaned.pdf", width = 6.4, height = 4.4, family = "Times")
par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.2, cex.axis = 1.1)
plot(s$Dd, s$yd, pch = 19, cex = 0.9, col = cols[s$who], xlim = c(-0.75, 0.75),
     xlab = "married minus the man's own mean", ylab = "log wage minus own mean")
abline(0, b_fe, lwd = 2.5); abline(h = 0, v = 0, col = "gray70", lty = 3)
legend("bottomright", c(lab, sprintf("one line, slope %.2f", b_fe)), col = c(cols, "black"),
       pch = c(19, 19, 19, 19, NA), lwd = c(NA, NA, NA, NA, 2.5), bty = "n", cex = 1.05)
dev.off()

## ---- 6. why year effects: wages and marriage both rise with calendar time -----------------------
pdf("figs/pan_time.pdf", width = 9, height = 3.1, family = "Times")
par(mfrow = c(1, 2), mar = c(4, 4.4, 2, 0.6), cex.lab = 1.15, cex.axis = 1.05, cex.main = 1.15)
yrs <- 1980:1987
plot(yrs, tapply(w$lwage, w$year, mean), type = "b", pch = 19, lwd = 2.5, col = "navy",
     xlab = "year", ylab = "mean log wage", main = "every man's wage grows")
plot(yrs, tapply(w$married, w$year, mean), type = "b", pch = 19, lwd = 2.5, col = "firebrick",
     ylim = c(0, 0.7), xlab = "year", ylab = "share married", main = "and more men are married")
dev.off()
