## Week 11 -- synthetic control on California's Proposition 99 (Abadie, Diamond, and Hainmueller 2010).
## Data: tidysynth::smoking (39 states, 1970-2000; cigsale = packs per capita). California treated from 1989.
##   Rscript figs/w11_prop99.R > figs/w11_prop99.out ; writes figs/prop99_paths.pdf, figs/prop99_placebos.pdf
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
for (p in c("tidysynth", "quadprog"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
## synthdid and augsynth are on GitHub only: remotes::install_github("synth-inference/synthdid"); ("ebenmichael/augsynth")
suppressMessages({library(tidysynth); library(dplyr); library(quadprog); library(synthdid); library(augsynth)})
data(smoking, package = "tidysynth")
sm <- as.data.frame(smoking)
Y <- reshape(sm[, c("state", "year", "cigsale")], idvar = "state", timevar = "year", direction = "wide")
rownames(Y) <- Y$state; Y <- as.matrix(Y[, -1]); colnames(Y) <- 1970:2000
pre <- as.character(1970:1988); post <- as.character(1989:2000)
y1 <- Y["California", ]; Y0 <- Y[rownames(Y) != "California", ]
cat("donors:", nrow(Y0), " pre-periods:", length(pre), " post-periods:", length(post), "\n")

## ---- 1. classic SC with the ADH predictors (tidysynth) ---------------------------------
sc <- sm %>% synthetic_control(outcome = cigsale, unit = state, time = year, i_unit = "California",
                               i_time = 1988, generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1980:1988, ln_income = mean(lnincome, na.rm = TRUE),
                     ret_price = mean(retprice, na.rm = TRUE), youth = mean(age15to24, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1984:1988, beer_sales = mean(beer, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1975, cigsale_1975 = cigsale) %>%
  generate_predictor(time_window = 1980, cigsale_1980 = cigsale) %>%
  generate_predictor(time_window = 1988, cigsale_1988 = cigsale) %>%
  generate_weights(optimization_window = 1970:1988, margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()
w <- grab_unit_weights(sc) %>% arrange(desc(weight))
cat("\n## 1. unit weights (> 0.001)\n"); print(as.data.frame(w %>% filter(weight > 0.001)), digits = 3)
cat("\npredictor balance\n"); print(as.data.frame(grab_balance_table(sc)), digits = 4)
sy <- grab_synthetic_control(sc)
gap <- sy$real_y - sy$synth_y; names(gap) <- sy$time_unit
cat("\npre-period RMSPE:", round(sqrt(mean(gap[pre]^2)), 2), " MSPE:", round(mean(gap[pre]^2), 2), "\n")
cat("average gap 1989-2000:", round(mean(gap[post]), 1), "; gap in 2000:", round(gap["2000"], 1), "\n")
cat("California 1988:", y1["1988"], "; 2000:", y1["2000"], "; synthetic 2000:", round(sy$synth_y[sy$time_unit == 2000], 1), "\n")
cat("average gap as % of synthetic, 1989-2000:", round(100 * mean(gap[post] / sy$synth_y[sy$time_unit >= 1989]), 1), "\n")

## ---- 2. placebo distribution --------------------------------------------------------------
cat("\n## 2. placebos: post/pre MSPE ratio\n")
sig <- grab_significance(sc)
print(as.data.frame(head(sig %>% select(unit_name, pre_mspe, post_mspe, mspe_ratio, rank, fishers_exact_pvalue), 5)), digits = 3)
pl <- grab_synthetic_control(sc, placebo = TRUE)
pl$gap <- pl$real_y - pl$synth_y
pm <- pl %>% filter(time_unit <= 1988) %>% group_by(.id) %>% summarise(pre_mspe = mean(gap^2))
camspe <- pm$pre_mspe[pm$.id == "California"]
cat("California pre-MSPE:", round(camspe, 2), "; median donor pre-MSPE:", round(median(pm$pre_mspe[pm$.id != "California"]), 2), "\n")
keep2 <- pm$.id[pm$pre_mspe <= 2 * camspe]
cat("units with pre-MSPE <= 2x California's:", length(keep2), "\n")

## ---- 3. in-time placebo (pretend 1980) and leave-one-out ---------------------------------
cat("\n## 3. robustness\n")
sc_w <- function(y1v, Y0m, tt) {          # outcome-only SC weights by quadratic programming on periods tt
  A <- Y0m[, tt, drop = FALSE]; J <- nrow(A)
  Dm <- A %*% t(A) + diag(1e-8, J); dv <- A %*% y1v[tt]
  Am <- cbind(rep(1, J), diag(J)); b0 <- c(1, rep(0, J))
  s <- solve.QP(Dm, dv, Am, b0, meq = 1)$solution; s[s < 1e-6] <- 0; s / sum(s) }
w_out <- sc_w(y1, Y0, pre); names(w_out) <- rownames(Y0)
cat("outcome-only SC weights (all 19 pre-years):\n"); print(round(sort(w_out[w_out > 0.001], decreasing = TRUE), 3))
g_out <- y1 - colSums(w_out * Y0)
cat("outcome-only: pre RMSPE", round(sqrt(mean(g_out[pre]^2)), 2), "; average post gap", round(mean(g_out[post]), 1), "\n")
w80 <- sc_w(y1, Y0, as.character(1970:1979)); g80 <- y1 - colSums(w80 * Y0)
cat("in-time placebo (treat 1980, fit 1970-79): average gap 1980-88 =", round(mean(g80[as.character(1980:1988)]), 2),
    "; pre RMSPE", round(sqrt(mean(g80[as.character(1970:1979)]^2)), 2), "\n")
top <- names(sort(w_out[w_out > 0.01], decreasing = TRUE))
loo <- sapply(top, function(d) { Yd <- Y0[rownames(Y0) != d, ]; wd <- sc_w(y1, Yd, pre); mean((y1 - colSums(wd * Yd))[post]) })
cat("leave-one-out average post gap:\n"); print(round(loo, 1))

## ---- 4. regression weights extrapolate ----------------------------------------------------
cat("\n## 4. regression (minimum-norm, sum to one) weights on the same pre-period outcomes\n")
A <- rbind(1, t(Y0[, pre])); bvec <- c(1, y1[pre])            # rows: adding-up + 19 pre-periods
w_reg <- as.vector(t(A) %*% solve(A %*% t(A), bvec)); names(w_reg) <- rownames(Y0)
cat("sum:", round(sum(w_reg), 3), "; negative weights:", sum(w_reg < 0), "of", length(w_reg),
    "; range:", round(range(w_reg), 2), "; pre-period fit exact:", round(max(abs(y1[pre] - colSums(w_reg * Y0[, pre]))), 6), "\n")
g_reg <- y1 - colSums(w_reg * Y0)
cat("regression-weight average post gap:", round(mean(g_reg[post]), 1), "\n")
print(round(head(sort(w_reg), 4), 2)); print(round(head(sort(w_reg, decreasing = TRUE), 4), 2))

## ---- 5. DiD, SC, synthetic DiD (synthdid), augmented SC ----------------------------------
cat("\n## 5. estimators on the same panel\n")
setup <- panel.matrices(as.data.frame(transform(sm[, c("state", "year", "cigsale")],
                        treated = as.integer(state == "California" & year >= 1989))))
e_sdid <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
e_sc <- sc_estimate(setup$Y, setup$N0, setup$T0)
e_did <- did_estimate(setup$Y, setup$N0, setup$T0)
se <- function(e) sqrt(vcov(e, method = "placebo"))
print(round(rbind(DiD = c(e_did, se(e_did)), SC = c(e_sc, se(e_sc)), SDiD = c(e_sdid, se(e_sdid))), 1))
ws <- attr(e_sdid, "weights")
om <- ws$omega; names(om) <- rownames(setup$Y)[1:setup$N0]
la <- ws$lambda; names(la) <- colnames(setup$Y)[1:setup$T0]
cat("SDiD unit weights > .05:\n"); print(round(sort(om[om > 0.05], decreasing = TRUE), 3))
cat("SDiD time weights > .05:\n"); print(round(la[la > 0.05], 3))
asc <- augsynth(cigsale ~ trt, state, year, transform(sm, trt = as.integer(state == "California" & year >= 1989)),
                progfunc = "Ridge", scm = TRUE)
sa <- summary(asc, inf_type = "jackknife+")
cat("augmented SC (ridge): average ATT", round(sa$average_att$Estimate, 1), "; L2 imbalance", round(sa$l2_imbalance, 2),
    "; scaled", round(sa$scaled_l2_imbalance, 3), "\n")

## ---- 6. conformal inference (Chernozhukov, Wuthrich, Zhu 2021), sharp null tau_t = 0 ------
cat("\n## 6. conformal test of H0: no effect in any post-year\n")
allp <- c(pre, post)
w0 <- sc_w(y1, Y0, allp)                         # under H0 the post-years are untreated: fit on all 31
u <- y1 - colSums(w0 * Y0)
S <- function(uu) mean(abs(uu[(length(pre) + 1):length(allp)]))
Sobs <- S(u); Tn <- length(allp)
Sperm <- sapply(0:(Tn - 1), function(k) S(u[((seq_len(Tn) - 1 + k) %% Tn) + 1]))   # moving-block permutations
cat("statistic:", round(Sobs, 2), "; p-value (31 cyclic shifts):", round(mean(Sperm >= Sobs), 3), "\n")

## ---- 7. figures ---------------------------------------------------------------------------
pdf("figs/prop99_paths.pdf", width = 9, height = 3.4, family = "Times")
par(mar = c(4, 4.2, 1, 1))
yrs <- 1970:2000
plot(yrs, y1, type = "l", lwd = 2.5, col = "firebrick", ylim = c(30, 140), xlab = "year", ylab = "packs per capita")
lines(yrs, colMeans(Y0), lwd = 2, lty = 3, col = "gray40")
lines(sy$time_unit, sy$synth_y, lwd = 2.5, lty = 2, col = "navy")
abline(v = 1988.5, lty = 2, col = "gray50")
legend("bottomleft", c("California", "synthetic California", "average of 38 donor states"),
       col = c("firebrick", "navy", "gray40"), lty = c(1, 2, 3), lwd = 2, bty = "n")
dev.off()
pdf("figs/prop99_placebos.pdf", width = 9, height = 3.4, family = "Times")
par(mar = c(4, 4.2, 1, 1))
plot(NA, xlim = c(1970, 2000), ylim = c(-35, 35), xlab = "year", ylab = "gap: actual - synthetic")
for (id in setdiff(keep2, "California")) { d <- pl[pl$.id == id, ]; lines(d$time_unit, d$gap, col = "gray70") }
d <- pl[pl$.id == "California", ]; lines(d$time_unit, d$gap, col = "firebrick", lwd = 3)
abline(v = 1988.5, lty = 2); abline(h = 0, lty = 3)
legend("bottomleft", c("California", "placebo states with pre-MSPE <= 2x California's"), col = c("firebrick", "gray70"), lwd = c(3, 1), bty = "n")
dev.off()
