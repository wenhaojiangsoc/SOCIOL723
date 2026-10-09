## Week 10 -- staggered adoption on real data: unilateral divorce laws and female suicide.
## Data: bacondecomp::divorce (Stevenson and Wolfers 2006 QJE), women, 1964-1996.
##   outcome y = female suicides per million women (suiciderate_jag x 1e6)
##   treatment D = 1 from the year the state adopted unilateral divorce (divyear);
##   divyear 1950 = adopted before 1964 (always treated); divyear 2000 = never in sample (never treated)
##   Rscript figs/w10_divorce.R > figs/w10_divorce.out ; writes figs/divorce_es.pdf, figs/divorce_raw.pdf
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
for (p in c("bacondecomp", "HonestDiD", "didimputation", "etwfe"))
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
suppressMessages({library(fixest); library(did); library(bacondecomp); library(didimputation)
  library(HonestDiD); library(etwfe)})

data(divorce, package = "bacondecomp")
d <- subset(divorce, sex == 2)[, c("st", "year", "divyear", "suiciderate_jag")]   # bacon() trips on extra columns
d$y <- d$suiciderate_jag * 1e6
d$D <- as.integer(d$year >= d$divyear)
d$id <- as.integer(factor(d$st))
d$gcs <- ifelse(d$divyear == 2000, 0, d$divyear)        # did::att_gt: 0 = never treated
cat("states:", length(unique(d$st)), " years:", paste(range(d$year), collapse = "-"), "\n")
st <- d[!duplicated(d$st), ]
cat("always treated (pre-1964):", sum(st$divyear < 1964), " never treated:", sum(st$divyear == 2000),
    " adopters 1969-85:", sum(st$divyear >= 1964 & st$divyear < 2000), "\n")
print(table(st$divyear))
cat("mean outcome 1964:", round(mean(d$y[d$year == 1964]), 1), "\n")

## ---- 1. TWFE ---------------------------------------------------------------------
tw <- feols(y ~ D | st + year, d, cluster = ~st)
cat("\n## 1. TWFE: ", round(coef(tw), 3), " (se ", round(se(tw), 3), ")\n", sep = "")

## ---- 2. Goodman-Bacon decomposition --------------------------------------------------
cat("\n## 2. Bacon decomposition\n")
b <- bacon(y ~ post, data = data.frame(st = d$st, year = d$year, y = d$y, post = d$D),
           id_var = "st", time_var = "year", quietly = TRUE)   # bacon() needs a treatment not named D
agg <- aggregate(cbind(wt = weight, we = weight * estimate) ~ type, b, sum)
agg$avg <- agg$we / agg$wt
print(data.frame(type = agg$type, weight = round(agg$wt, 3), avg_est = round(agg$avg, 2)))
cat("weighted sum:", round(sum(b$weight * b$estimate), 3), " number of 2x2s:", nrow(b), "\n")

## ---- 3. the weights TWFE puts on treated cells (double-demeaned D) ---------------------
cat("\n## 3. TWFE weights on treated cells\n")
Dt <- d$D - ave(d$D, d$st) - ave(d$D, d$year) + mean(d$D)
tr <- d$D == 1; w <- Dt[tr] / sum(Dt[tr])
cat("treated cells:", sum(tr), " negative weights:", sum(w < 0), " sum of negative weights:", round(sum(w[w < 0]), 3), "\n")
neg <- d[tr, ][w < 0, ]
cat("negative-weight cells: adoption years", paste(sort(unique(neg$divyear)), collapse = ","), "; years",
    paste(range(neg$year), collapse = "-"), "\n")

## ---- 4. event studies: TWFE, Sun-Abraham, Callaway-Sant'Anna, imputation --------------
cat("\n## 4. event studies (k = year - adoption year)\n")
ds <- subset(d, divyear >= 1964)                      # drop always-treated (no pre-period)
ds$k <- ifelse(ds$divyear == 2000, -1000, ds$year - ds$divyear)
ds$kb <- pmin(pmax(ds$k, -10), 15); ds$kb[ds$divyear == 2000] <- -1000
es_tw <- feols(y ~ i(kb, ref = c(-1, -1000)) | st + year, ds, cluster = ~st)
ds$coh <- ifelse(ds$divyear == 2000, 10000, ds$divyear)
es_sa <- feols(y ~ sunab(coh, year, ref.p = c(-1, -10000)) | st + year, ds, cluster = ~st)
cs <- att_gt(yname = "y", tname = "year", idname = "id", gname = "gcs", data = ds,
             control_group = "nevertreated", base_period = "universal", bstrap = FALSE, cband = FALSE)
cs_es <- aggte(cs, type = "dynamic", min_e = -10, max_e = 15, bstrap = FALSE, cband = FALSE)
cs_ny <- att_gt(yname = "y", tname = "year", idname = "id", gname = "gcs", data = ds,
                control_group = "notyettreated", base_period = "universal", bstrap = FALSE, cband = FALSE)
ds$gimp <- ifelse(ds$divyear == 2000, 0, ds$divyear)
imp <- did_imputation(ds, yname = "y", gname = "gimp", tname = "year", idname = "id",
                      horizon = TRUE, pretrends = -10:-1)
pick <- function(ct, ks) { r <- ct[match(ks, ct$k), ]; r }
ks <- -10:15
tw_ct <- data.frame(k = as.numeric(sub("kb::", "", names(coef(es_tw)))), est = coef(es_tw), se = se(es_tw))
sa_ct <- data.frame(k = as.numeric(sub("year::", "", names(coef(es_sa)))), est = coef(es_sa), se = se(es_sa))
cs_ct <- data.frame(k = cs_es$egt, est = cs_es$att.egt, se = cs_es$se.egt)
im_ct <- data.frame(k = as.numeric(imp$term), est = imp$estimate, se = imp$std.error)
show <- c(-10, -5, -3, -2, 0, 2, 5, 10, 15)
tab <- sapply(list(TWFE = tw_ct, SunAbraham = sa_ct, CallawaySantAnna = cs_ct, Imputation = im_ct),
              function(ct) round(ct$est[match(show, ct$k)], 2))
rownames(tab) <- show; print(tab)

cat("\n## 5. overall ATTs\n")
cs_s <- aggte(cs, type = "simple", bstrap = FALSE); cs_g <- aggte(cs, type = "group", bstrap = FALSE)
cs_nys <- aggte(cs_ny, type = "simple", bstrap = FALSE)
imp_s <- did_imputation(ds, yname = "y", gname = "gimp", tname = "year", idname = "id")
et <- etwfe(fml = y ~ 0, tvar = year, gvar = gimp, data = ds, cgroup = "never", vcov = ~st)
et_s <- emfx(et, type = "simple")
et2 <- etwfe(fml = y ~ 0, tvar = year, gvar = gimp, data = ds, cgroup = "notyet", vcov = ~st)
et2_s <- emfx(et2, type = "simple")
sa_s <- summary(es_sa, agg = "ATT")
tw_s <- feols(y ~ D | st + year, ds, cluster = ~st)
res <- rbind(
  TWFE_all_states = c(coef(tw), se(tw)),
  TWFE_no_always = c(coef(tw_s), se(tw_s)),
  CS_simple_never = c(cs_s$overall.att, cs_s$overall.se),
  CS_simple_notyet = c(cs_nys$overall.att, cs_nys$overall.se),
  CS_group = c(cs_g$overall.att, cs_g$overall.se),
  SunAbraham_ATT = c(coef(sa_s)[["ATT"]], se(sa_s)[["ATT"]]),
  Imputation = c(imp_s$estimate, imp_s$std.error),
  ETWFE_never = c(et_s$estimate, et_s$std.error),
  ETWFE_notyet = c(et2_s$estimate, et2_s$std.error))
colnames(res) <- c("est", "se"); print(round(res, 2))
cat("CS group-specific ATTs:\n"); print(round(data.frame(g = cs_g$egt, att = cs_g$att.egt, se = cs_g$se.egt), 2))

## ---- 6. HonestDiD on the Sun-Abraham event study -----------------------------------
cat("\n## 6. HonestDiD, relative magnitudes, on the Callaway-Sant'Anna event study (k = -10..10)\n")
IF <- cs_es$inf.function$dynamic.inf.func.e
V <- crossprod(IF) / nrow(IF)^2
e <- cs_es$egt; keep <- e >= -10 & e <= 10 & e != -1        # e = -1 is the universal base, fixed at 0
bh <- cs_es$att.egt[keep]; Vh <- V[keep, keep]; ek <- e[keep]
npre <- sum(ek < 0); npost <- sum(ek >= 0)
lvec <- rep(1 / npost, npost)                                  # average effect over k = 0..10
cat("average over k = 0..10:", round(sum(lvec * bh[ek >= 0]), 2), "\n")
orig <- constructOriginalCS(betahat = bh, sigma = Vh, numPrePeriods = npre, numPostPeriods = npost, l_vec = lvec)
rm <- createSensitivityResults_relativeMagnitudes(betahat = bh, sigma = Vh, numPrePeriods = npre,
        numPostPeriods = npost, Mbarvec = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 1), l_vec = lvec)
print(orig); print(rm)

## ---- 7. figures ----------------------------------------------------------------------
pdf("figs/divorce_es.pdf", width = 9, height = 3.6, family = "Times")
par(mar = c(4, 4.2, 1, 1))
plot(NA, xlim = c(-10, 15), ylim = c(-32, 18), xlab = "years since unilateral divorce (k)",
     ylab = "female suicides per million")
abline(h = 0, lty = 3); abline(v = -0.5, lty = 2, col = "gray50")
add <- function(ct, col, off, pch) { ct <- ct[ct$k >= -10 & ct$k <= 15, ]
  segments(ct$k + off, ct$est - 1.96 * ct$se, ct$k + off, ct$est + 1.96 * ct$se, col = col)
  points(ct$k + off, ct$est, pch = pch, col = col, cex = 0.8) }
add(rbind(tw_ct, data.frame(k = -1, est = 0, se = 0)), "gray40", -0.2, 1)
add(rbind(cs_ct), "navy", 0, 19)
add(im_ct, "firebrick", 0.2, 17)
legend("topright", ncol = 3, c("TWFE event study", "Callaway-Sant'Anna (never treated)", "imputation (BJS)"),
       col = c("gray40", "navy", "firebrick"), pch = c(1, 19, 17), bty = "n", cex = 0.85)
dev.off()

pdf("figs/divorce_raw.pdf", width = 9, height = 3.4, family = "Times")
par(mar = c(4, 4.2, 1, 1))
grp <- cut(d$divyear, c(0, 1963, 1971, 1973, 1999, 2001), labels = c("before 1964", "1969-71", "1972-73", "1974-85", "never"))
m <- tapply(d$y, list(d$year, grp), mean)
matplot(as.numeric(rownames(m)), m, type = "l", lty = 1, lwd = 2,
        col = c("gray50", "navy", "steelblue", "firebrick", "black"), xlab = "year", ylab = "female suicides per million")
legend("topright", colnames(m), col = c("gray50", "navy", "steelblue", "firebrick", "black"), lty = 1, lwd = 2, bty = "n", cex = 0.85, title = "adopted")
dev.off()
