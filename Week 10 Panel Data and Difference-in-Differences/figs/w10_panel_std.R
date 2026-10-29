## Week 10 -- textbook panel tools on wooldridge::wagepan (545 men, 1980-87):
## two-way demeaning, lead test of strict exogeneity, FD vs FE and the serial-correlation check,
## conventional vs clustered s.e., RE theta, dynamic panels (Nickell simulation, Anderson-Hsiao,
## Arellano-Bond).
##   Rscript figs/w10_panel_std.R > figs/w10_panel_std.out   (run from the week folder)
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
if (!requireNamespace("wooldridge", quietly = TRUE))
  install.packages("wooldridge", lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
suppressMessages({library(fixest); library(plm)}); setFixest_notes(FALSE)
data(wagepan, package = "wooldridge")
w <- wagepan
N <- length(unique(w$nr)); Tn <- length(unique(w$year))

## ---- 1. two-way demeaning = man and year dummies (balanced panel) --------------------
cat("## 1. two-way demeaning\n")
dd <- function(v) v - ave(v, w$nr) - ave(v, w$year) + mean(v)
w$y2 <- dd(w$lwage); w$m2 <- dd(w$married); w$u2 <- dd(w$union); w$e2 <- dd(w$expersq)
f_fe <- feols(lwage ~ married + union + expersq | nr + year, w)
print(round(rbind(dummies = coef(f_fe), double_demeaned_lm = coef(lm(y2 ~ 0 + m2 + u2 + e2, w))), 5))

## ---- 2. conventional vs clustered s.e. ------------------------------------------------
cat("\n## 2. FE s.e. for married: iid vs clustered by man\n")
print(round(c(est = coef(f_fe)[["married"]], se_iid = se(summary(f_fe, vcov = "iid"))[["married"]],
              se_cluster = se(summary(f_fe, cluster = ~nr))[["married"]]), 4))

## ---- 3. strict exogeneity: lead test ---------------------------------------------------
cat("\n## 3. lead of married added to the FE model (1980-86 rows)\n")
f_lead <- feols(lwage ~ married + f(married) + union + expersq | nr + year, w, panel.id = ~nr + year, cluster = ~nr)
print(round(coeftable(f_lead), 4))

## ---- 4. first differences, and FE vs FD efficiency -----------------------------------
cat("\n## 4. FD; serial correlation of FD residuals (FE.3 implies -0.5)\n")
f_fd <- feols(d(lwage) ~ d(married) + d(union) + d(expersq) | year, w, panel.id = ~nr + year, cluster = ~nr)
print(round(c(FE = coef(f_fe)[["married"]], FD = coef(f_fd)[["d(married)"]], FD_se = se(f_fd)[["d(married)"]]), 4))
w$e_fd <- NA; w$e_fd[obs(f_fd)] <- resid(f_fd)
ar <- feols(e_fd ~ l(e_fd), w, panel.id = ~nr + year, cluster = ~nr)
print(round(coeftable(ar), 4))
cat("check T = 2 (1986-87 only): FE and FD on the same two years\n")
w2 <- subset(w, year >= 1986)
print(round(c(FE = coef(feols(lwage ~ married + union | nr + year, w2))[["married"]],
              FD = coef(feols(d(lwage) ~ d(married) + d(union), w2, panel.id = ~nr + year))[["d(married)"]]), 5))

## ---- 5. random effects theta ---------------------------------------------------------------
cat("\n## 5. RE in the balanced panel: one theta\n")
pw <- pdata.frame(w, index = c("nr", "year"))
re <- plm(lwage ~ married + union + educ + black + hisp + exper + expersq + factor(year), pw, model = "random")
ec <- ercomp(re); print(ec)
s2e <- ec$sigma2[["idios"]]; s2a <- ec$sigma2[["id"]]
cat("theta = 1 - sqrt(s2e/(s2e + T s2a)) =", round(1 - sqrt(s2e / (s2e + Tn * s2a)), 4), "\n")
cat("share of residual variance from the man effect:", round(s2a / (s2a + s2e), 3), "\n")

## ---- 6. Nickell bias: simulation ----------------------------------------------------------
cat("\n## 6. Nickell bias, AR(1) with unit effects: Y_it = rho Y_it-1 + a_i + e_it, rho = 0.5, N = 500\n")
sim <- function(Tt, rho = 0.5, n = 500) {
  a <- rnorm(n); y <- matrix(0, n, Tt + 50)
  y[, 1] <- a / (1 - rho) + rnorm(n) / sqrt(1 - rho^2)
  for (t in 2:(Tt + 50)) y[, t] <- rho * y[, t - 1] + a + rnorm(n)
  y <- y[, 51:(Tt + 50)]
  d <- data.frame(id = rep(1:n, Tt), t = rep(1:Tt, each = n), y = as.vector(y))
  fe <- coef(feols(y ~ l(y) | id, d, panel.id = ~id + t))[[1]]
  ah <- coef(feols(d(y) ~ 1 | d(l(y)) ~ l(y, 2), d, panel.id = ~id + t))[[2]]
  c(FE = fe, AH = ah)
}
res <- t(sapply(c(4, 8, 16), function(Tt) { r <- replicate(200, sim(Tt)); c(T = Tt, FE = mean(r[1, ]), AH = mean(r[2, ]),
  AH_sd = sd(r[2, ]), nickell_approx = 0.5 - (1 + 0.5) / (Tt - 1)) }))
print(round(res, 3))

## ---- 7. dynamic wage equation on wagepan -------------------------------------------------
cat("\n## 7. lwage on its lag: pooled, FE, Anderson-Hsiao, Arellano-Bond\n")
d_pool <- feols(lwage ~ l(lwage) + married + union | year, w, panel.id = ~nr + year, cluster = ~nr)
d_fe <- feols(lwage ~ l(lwage) + married + union | nr + year, w, panel.id = ~nr + year, cluster = ~nr)
d_ah <- feols(d(lwage) ~ d(married) + d(union) | year | d(l(lwage)) ~ l(lwage, 2), w, panel.id = ~nr + year, cluster = ~nr)
ab <- pgmm(lwage ~ lag(lwage, 1) + married + union | lag(lwage, 2:99), data = pw,
           effect = "twoways", model = "onestep", transformation = "d")
sab <- summary(ab, robust = TRUE)
tb <- rbind(pooled = coeftable(d_pool)[c(1, 2), 1:2], FE = coeftable(d_fe)[c(1, 2), 1:2],
            AH = coeftable(d_ah)[c("fit_d(l(lwage))", "d(married)"), 1:2],
            AB = sab$coefficients[c("lag(lwage, 1)", "married"), 1:2])
print(round(tb, 3))
cat("AH first stage F:\n"); print(fitstat(d_ah, ~ ivf1))
cat("AB serial correlation tests of differenced residuals:\n"); print(sab$m1); print(sab$m2)
cat("number of instruments:", ncol(ab$W[[1]]), "\n")

## ---- 8. AR(2) rejects at lag-2 instruments: start the instruments at lag 3 ---------------
cat("\n## 8. instruments from lag 3 on\n")
ab3 <- pgmm(lwage ~ lag(lwage, 1) + married + union | lag(lwage, 3:99), data = pw,
            effect = "twoways", model = "onestep", transformation = "d")
sab3 <- summary(ab3, robust = TRUE)
d_ah3 <- feols(d(lwage) ~ d(married) + d(union) | year | d(l(lwage)) ~ l(lwage, 3), w, panel.id = ~nr + year, cluster = ~nr)
print(round(rbind(AH_lag3 = coeftable(d_ah3)[c("fit_d(l(lwage))", "d(married)"), 1:2],
                  AB_lag3 = sab3$coefficients[c("lag(lwage, 1)", "married"), 1:2]), 3))
cat("AH lag-3 first stage F:\n"); print(fitstat(d_ah3, ~ ivf1))
cat("AB lag 3+: m2 p =", round(sab3$m2$p.value, 3), "\n")
## with MA(1) errors the AR(2) test rejects even when lag-3 instruments are valid; AR(3) is the check
m3 <- mtest(ab3, order = 3, vcov = vcovHC(ab3))
cat("AB lag 3+: m3 z =", round(m3$statistic, 2), " p =", round(m3$p.value, 3), "\n")

## ---- 9. Nickell's exact fixed-T formula vs the large-T approximation ------------------
cat("\n## 9. Nickell (1981) fixed-T bias, T = periods in the regression (Tt - 1)\n")
nick <- function(rho, Tr) { A <- (1 - rho^Tr) / (Tr * (1 - rho))
  -((1 + rho) / (Tr - 1)) * (1 - A) / (1 - (2 * rho / ((1 - rho) * (Tr - 1))) * (1 - A)) }
print(round(sapply(c(4, 8, 16), function(Tt) c(T_data = Tt, rho_plus_exact_bias = 0.5 + nick(0.5, Tt - 1),
                                                rho_plus_approx = 0.5 - 1.5 / (Tt - 1))), 3))
