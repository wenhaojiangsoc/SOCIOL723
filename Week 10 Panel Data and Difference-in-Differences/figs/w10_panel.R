## Week 10 -- fixed effects on real panel data: the male marriage wage premium.
## Data: wooldridge::wagepan (NLSY, 545 men observed every year 1980-1987; Vella and Verbeek 1998).
##   Rscript figs/w10_panel.R > figs/w10_panel.out ; writes figs/marriage_impact.pdf
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
if (!requireNamespace("wooldridge", quietly = TRUE))
  install.packages("wooldridge", lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
suppressMessages({library(fixest); library(plm)})
data(wagepan, package = "wooldridge")
w <- wagepan
cat("men:", length(unique(w$nr)), " years:", paste(range(w$year), collapse = "-"), " rows:", nrow(w), "\n")
cat("share married, 1980 and 1987:", round(mean(w$married[w$year == 1980]), 3), round(mean(w$married[w$year == 1987]), 3), "\n")
ch <- tapply(w$married, w$nr, function(m) length(unique(m)) > 1)
cat("men whose marital status changes:", sum(ch), "of", length(ch), "\n")
mar_pat <- tapply(w$married, w$nr, function(m) paste(m, collapse = ""))
cat("men who marry once and stay married (0...01...1):", sum(grepl("^0+1+$", mar_pat)),
    "; who ever leave marriage:", sum(grepl("10", mar_pat)), "\n")

## ---- 1. pooled, RE, FE, FD, Mundlak -----------------------------------------------
cat("\n## 1. log wage on married (+ union, year effects); SEs clustered by man\n")
f_pool <- feols(lwage ~ married + union + educ + black + hisp + exper + expersq | year, w, cluster = ~nr)
f_fe   <- feols(lwage ~ married + union + expersq | nr + year, w, cluster = ~nr)
pw <- pdata.frame(w, index = c("nr", "year"))
f_re   <- plm(lwage ~ married + union + educ + black + hisp + exper + expersq + factor(year), pw, model = "random")
f_fd   <- feols(d(lwage) ~ d(married) + d(union) + d(expersq) | year, w, panel.id = ~nr + year, cluster = ~nr)
w$m_bar <- ave(w$married, w$nr); w$u_bar <- ave(w$union, w$nr); w$e_bar <- ave(w$expersq, w$nr)
f_cre  <- feols(lwage ~ married + union + expersq + m_bar + u_bar + e_bar + educ + black + hisp | year, w, cluster = ~nr)
tab <- rbind(pooled_OLS = c(coef(f_pool)["married"], se(f_pool)["married"]),
             random_effects = c(coef(f_re)["married"], sqrt(diag(vcov(f_re)))["married"]),
             fixed_effects = c(coef(f_fe)["married"], se(f_fe)["married"]),
             first_difference = c(coef(f_fd)["d(married)"], se(f_fd)["d(married)"]),
             Mundlak_within = c(coef(f_cre)["married"], se(f_cre)["married"]),
             Mundlak_mean = c(coef(f_cre)["m_bar"], se(f_cre)["m_bar"]))
colnames(tab) <- c("est", "se"); print(round(tab, 4))
cat("Hausman (FE vs RE):\n"); f_fe_plm <- plm(lwage ~ married + union + expersq + factor(year), pw, model = "within")
f_re2 <- plm(lwage ~ married + union + expersq + factor(year), pw, model = "random"); print(phtest(f_fe_plm, f_re2))

## ---- 2. what FE averages: person slopes weighted by within-variance ----------------
cat("\n## 2a. FE as a variance-weighted average (married only, man effects only)\n")
w$md1 <- w$married - ave(w$married, w$nr); w$yd1 <- w$lwage - ave(w$lwage, w$nr)
S1 <- aggregate(cbind(sxy = md1 * yd1, sxx = md1^2) ~ nr, w, sum); S1 <- S1[S1$sxx > 0, ]
S1$b <- S1$sxy / S1$sxx; S1$wt <- S1$sxx / sum(S1$sxx)
S1$T1 <- tapply(w$married, w$nr, sum)[as.character(S1$nr)]
cat("FE slope:", round(sum(w$md1 * w$yd1) / sum(w$md1^2), 4), "= weighted mean of", nrow(S1), "person slopes:",
    round(sum(S1$wt * S1$b), 4), "; unweighted mean:", round(mean(S1$b), 4), "\n")
cat("sum of x-demeaned^2 for a man married m of 8 years = 8 (m/8)(1 - m/8); weight by m:\n")
print(round(tapply(S1$wt, S1$T1, sum), 3)); print(table(S1$T1))

cat("\n## 2b. FE as a variance-weighted average of person slopes (married only, man and year effects)\n")
dm <- demean(w[, c("lwage", "married")], f = w[, c("nr", "year")])   # two-way demeaned (FWL)
w$yd <- dm[, 1]; w$md <- dm[, 2]
fe1 <- sum(w$md * w$yd) / sum(w$md^2)
cat("check: feols coefficient", round(coef(feols(lwage ~ married | nr + year, w))[[1]], 4), "\n")
S <- aggregate(cbind(sxy = md * yd, sxx = md^2) ~ nr, w, sum)
S$changer <- ch[as.character(S$nr)]
S$b <- S$sxy / S$sxx; S$wt <- S$sxx / sum(S$sxx)
cat("FE slope:", round(fe1, 4), "= weighted mean of person slopes:", round(sum(S$wt * S$b), 4), "\n")
cat("weight carried by the", sum(!S$changer), "men whose status never changes:", round(sum(S$wt[!S$changer]), 3), "\n")
S$T1 <- tapply(w$married, w$nr, sum)[as.character(S$nr)]
cat("weight by years married (of 8), changers only:\n"); print(round(tapply(S$wt[S$changer], S$T1[S$changer], sum), 3))

## ---- 3. FE with individual slopes (FEIS) -----------------------------------------
cat("\n## 3. FEIS: each man his own intercept and his own experience slope\n")
f_feis <- feols(lwage ~ married + union | nr[exper] + year, w, cluster = ~nr)
print(round(rbind(FE = c(coef(f_fe)["married"], se(f_fe)["married"]), FEIS = c(coef(f_feis)["married"], se(f_feis)["married"])), 4))

## ---- 4. impact function: wage path around marriage (men who marry once in the panel) --
cat("\n## 4. impact function around marriage\n")
w$first_mar <- ave(ifelse(w$married == 1, w$year, NA), w$nr, FUN = function(v) suppressWarnings(min(v, na.rm = TRUE)))
w$first_mar[!is.finite(w$first_mar)] <- NA
once <- names(mar_pat)[grepl("^0+1+$", mar_pat)]; never <- names(mar_pat)[grepl("^0+$", mar_pat)]
s4 <- subset(w, nr %in% c(once, never))
s4$k <- ifelse(s4$nr %in% never, -1000, s4$year - s4$first_mar)
s4$kb <- pmax(pmin(s4$k, 4), -4); s4$kb[s4$nr %in% never] <- -1000
cat("men marrying once in 1981-87:", length(once), "; never married in panel:", length(never), "\n")
imp_fe <- feols(lwage ~ i(kb, ref = c(-1, -1000)) + expersq | nr + year, s4, cluster = ~nr)
imp_fs <- feols(lwage ~ i(kb, ref = c(-1, -1000)) | nr[exper] + year, s4, cluster = ~nr)
ct <- function(f) { k <- as.numeric(sub("kb::", "", names(coef(f))[grepl("kb::", names(coef(f)))]))
  data.frame(k = k, est = coef(f)[grepl("kb::", names(coef(f)))], se = se(f)[grepl("kb::", names(coef(f)))]) }
a <- ct(imp_fe); b <- ct(imp_fs)
print(round(cbind(k = a$k, FE = a$est, FE_se = a$se, FEIS = b$est[match(a$k, b$k)], FEIS_se = b$se[match(a$k, b$k)]), 3))
pdf("figs/marriage_impact.pdf", width = 9, height = 3.3, family = "Times")
par(mar = c(4, 4.2, 1, 1))
a <- rbind(a, data.frame(k = -1, est = 0, se = 0)); b <- rbind(b, data.frame(k = -1, est = 0, se = 0))
a <- a[order(a$k), ]; b <- b[order(b$k), ]
plot(a$k - 0.08, a$est, pch = 19, col = "navy", ylim = c(-0.12, 0.16), xlim = c(-4.3, 4.3),
     xlab = "years since marriage (end points binned)", ylab = "log wage, relative to k = -1")
segments(a$k - 0.08, a$est - 1.96 * a$se, a$k - 0.08, a$est + 1.96 * a$se, col = "navy")
abline(h = 0, lty = 3); abline(v = -0.5, lty = 2, col = "gray50")
dev.off()
