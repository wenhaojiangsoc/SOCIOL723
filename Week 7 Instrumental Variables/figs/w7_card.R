## Week 7 -- Card (1995) college proximity, on the lab's data (ivreg::SchoolingReturns, n = 3,010).
## Every number on the "Distance to College" frame comes from here.
##   Rscript figs/w7_card.R > figs/w7_card.out
## ivreg may live in a session library: Rscript -e 'install.packages("ivreg", lib = ...)'
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
if (!requireNamespace("ivreg", quietly = TRUE))
  install.packages("ivreg", lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
library(ivreg)
data("SchoolingReturns", package = "ivreg")
d <- transform(SchoolingReturns, lwage = log(wage), near4 = as.integer(nearcollege4 != "none"))
ctrl <- "experience + I(experience^2) + ethnicity + smsa + south"   # Card's Table 3 baseline controls
f_ols <- as.formula(paste("lwage ~ education +", ctrl))
f_fs  <- as.formula(paste("education ~ near4 +", ctrl))
f_rf  <- as.formula(paste("lwage ~ near4 +", ctrl))
f_iv  <- as.formula(paste("lwage ~ education +", ctrl, "| near4 +", ctrl))
ols <- lm(f_ols, d); fs <- lm(f_fs, d); rf <- lm(f_rf, d); iv <- ivreg(f_iv, data = d)
rob <- function(m, v) { s <- sqrt(sandwich::vcovHC(m, type = "HC1")[v, v]); c(coef(m)[v], s) }
cat("share near a 4-year college:", round(mean(d$near4), 3), "\n")
cat(sprintf("mean education near vs not: %.2f vs %.2f\n",
            mean(d$education[d$near4 == 1]), mean(d$education[d$near4 == 0])))
r <- rbind(OLS = rob(ols, "education"), first_stage = rob(fs, "near4"),
           reduced_form = rob(rf, "near4"), IV = rob(iv, "education"))
colnames(r) <- c("estimate", "HC1 se"); print(round(r, 4))
cat(sprintf("Wald = reduced form / first stage = %.4f / %.4f = %.4f\n",
            coef(rf)["near4"], coef(fs)["near4"], coef(rf)["near4"] / coef(fs)["near4"]))
F1 <- (coef(fs)["near4"] / sqrt(sandwich::vcovHC(fs, type = "HC1")["near4", "near4"]))^2
cat(sprintf("robust first-stage F = %.1f\n", F1))
## Lee et al. (2022) tF at this F: the printed |t| of the IV coefficient vs 1.96
cat(sprintf("IV t-ratio = %.2f\n", r["IV", 1] / r["IV", 2]))
## Who are the compliers? the first stage by region and by metro status
## (the subset variable is dropped from the controls inside its own subsets)
for (v in c("south", "smsa", "ethnicity")) {
  ctrl_v <- sub(paste0(" \\+ ", v), "", ctrl, fixed = FALSE)
  f_v <- as.formula(paste("education ~ near4 +", ctrl_v))
  for (lv in levels(d[[v]])) {
    sub_d <- d[d[[v]] == lv, ]
    cat(sprintf("first stage, %s = %s (n = %d): %.3f\n", v, lv, nrow(sub_d),
                coef(lm(f_v, sub_d))["near4"]))
  }
}

## ---- scalar IV standard error, no controls: one D (education), one Z (near4), one Y (lwage)
cat("\n==== bivariate case: the scalar IV standard error by hand ====\n")
y <- d$lwage; D <- d$education; Z <- d$near4; n <- length(y)
zt <- Z - mean(Z); dt <- D - mean(D)
b_iv <- sum(zt * y) / sum(zt * D)                       # Cov(Z,Y)/Cov(Z,D)
a_iv <- mean(y) - b_iv * mean(D)
e_iv <- y - a_iv - b_iv * D                             # residuals with the ACTUAL D
b_ols <- sum(dt * y) / sum(dt * D)
e_ols <- y - mean(y) - b_ols * dt
se_ols <- sqrt(sum(e_ols^2) / (n - 2) / sum(dt^2))
rho <- cor(Z, D)
se_iv_hom <- sqrt(sum(e_iv^2) / (n - 2) * sum(zt^2)) / abs(sum(zt * dt))   # homoskedastic formula
se_iv_hc  <- sqrt(sum(zt^2 * e_iv^2)) / abs(sum(zt * dt))                    # robust (HC0) formula
## two stages by hand: second-stage OLS on Dhat uses the WRONG residuals
Dhat <- fitted(lm(D ~ Z)); m2 <- lm(y ~ Dhat)
se_wrong <- summary(m2)$coefficients["Dhat", "Std. Error"]
m_pkg <- ivreg(lwage ~ education | near4, data = d)
cat(sprintf("corr(Z, D) = %.3f, so 1/|rho| = %.2f\n", rho, 1 / abs(rho)))
cat(sprintf("OLS  slope %.4f  se %.4f\n", b_ols, se_ols))
cat(sprintf("IV   slope %.4f  (ivreg: %.4f)\n", b_iv, coef(m_pkg)["education"]))
cat(sprintf("IV se, homoskedastic formula %.4f = OLS se / |rho| up to the residual variance: OLS se/|rho| = %.4f\n",
            se_iv_hom, se_ols / abs(rho)))
cat(sprintf("IV se, robust formula %.4f;  ivreg HC0 %.4f;  ivreg default %.4f\n", se_iv_hc,
            sqrt(sandwich::vcovHC(m_pkg, type = "HC0")["education", "education"]),
            summary(m_pkg)$coefficients["education", "Std. Error"]))
cat(sprintf("two stages by hand, second-stage printed se %.4f (uses residuals from Dhat, sd %.3f vs %.3f)\n",
            se_wrong, sd(resid(m2)), sd(e_iv)))
cat(sprintf("sd of residuals: OLS %.3f, IV %.3f\n", sd(e_ols), sd(e_iv)))
cat(sprintf("exact: rho %.4f, se_ols %.5f, sd ratio %.3f, se_ols * ratio / |rho| = %.5f, formula %.5f\n",
            rho, se_ols, sd(e_iv) / sd(e_ols), se_ols * sd(e_iv) / sd(e_ols) / abs(rho), se_iv_hom))

## ---- describing the compliers (binary D = schooling above a cutoff, Z = near a 4-year college)
cat("\n==== compliers via Abadie's kappa, written out ====\n")
Z <- d$near4
cov_list <- list(Black = as.integer(d$ethnicity == "afam"), South = as.integer(d$south == "yes"),
                 metro = as.integer(d$smsa == "yes"), experience = d$experience)
for (cut in c(12, 13, 15)) {
  Dc <- as.integer(d$education > cut)
  pAT <- mean(Dc[Z == 0]); pNT <- 1 - mean(Dc[Z == 1]); pC <- mean(Dc[Z == 1]) - mean(Dc[Z == 0])
  cat(sprintf("\nD = education > %d: Pr(AT) = %.3f  Pr(NT) = %.3f  Pr(C) = first stage = %.3f\n", cut, pAT, pNT, pC))
  for (nm in names(cov_list)) {
    x <- cov_list[[nm]]
    m_all <- mean(x); m_AT <- mean(x[Z == 0 & Dc == 1]); m_NT <- mean(x[Z == 1 & Dc == 0])
    m_C <- (m_all - pAT * m_AT - pNT * m_NT) / pC
    cat(sprintf("  %-11s all %6.3f  always-takers %6.3f  never-takers %6.3f  compliers %6.3f\n",
                nm, m_all, m_AT, m_NT, m_C))
  }
}
