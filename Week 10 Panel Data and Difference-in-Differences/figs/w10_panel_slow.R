## Week 10 -- the panel section built slowly on wooldridge::wagepan (545 men, 1980-87):
## pooled -> first differences -> man intercepts -> demeaning -> why year effects -> two-way FE.
##   Rscript figs/w10_panel_slow.R > figs/w10_panel_slow.out
##   writes figs/pan_intercepts.pdf, figs/pan_demeaned.pdf, figs/pan_time.pdf
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
