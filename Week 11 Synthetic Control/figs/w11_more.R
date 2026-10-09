## Week 11 -- two more data sets.
##  (1) Basque Country and terrorism (Abadie and Gardeazabal 2003; Synth::basque), outcome-only SC.
##  (2) Election-day registration and turnout (Xu 2017; gsynth::turnout): TWFE vs interactive fixed effects.
##   Rscript figs/w11_more.R > figs/w11_more.out
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
for (p in c("Synth", "gsynth", "quadprog", "fixest"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE)
suppressMessages({library(quadprog); library(gsynth); library(fixest)})
sc_w <- function(y1, Y0) { J <- nrow(Y0); D <- Y0 %*% t(Y0) + diag(1e-8, J)
  s <- solve.QP(D, Y0 %*% y1, cbind(rep(1, J), diag(J)), c(1, rep(0, J)), meq = 1)$solution; s[s < 1e-6] <- 0; s / sum(s) }

## ---- 1. Basque -------------------------------------------------------------------
data(basque, package = "Synth")
b <- subset(basque, regionno != 1)                                 # drop Spain as a whole
Yb <- reshape(b[, c("regionname", "year", "gdpcap")], idvar = "regionname", timevar = "year", direction = "wide")
rownames(Yb) <- Yb$regionname; Yb <- as.matrix(Yb[, -1]); colnames(Yb) <- 1955:1997
bq <- "Basque Country (Pais Vasco)"; y1 <- Yb[bq, ]; Y0 <- Yb[rownames(Yb) != bq, ]
pre <- as.character(1955:1969); post <- as.character(1970:1997)
w <- sc_w(y1[pre], Y0[, pre]); names(w) <- rownames(Y0)
cat("## 1. Basque: donors", nrow(Y0), "\n"); print(round(sort(w[w > 0.001], decreasing = TRUE), 3))
g <- y1 - colSums(w * Y0)
cat("pre RMSPE (thousand 1986 USD):", round(sqrt(mean(g[pre]^2)), 3), "; average gap 1975-97:", round(mean(g[as.character(1975:1997)]), 3),
    "; as % of synthetic:", round(100 * mean(g[as.character(1975:1997)] / colSums(w * Y0)[as.character(1975:1997)]), 1), "\n")

## predictor-based SC, as in the Synth package example (Abadie and Gardeazabal's specification)
suppressMessages(library(Synth))
dp <- dataprep(foo = basque, predictors = c("school.illit", "school.prim", "school.med", "school.high", "school.post.high", "invest"),
  predictors.op = "mean", time.predictors.prior = 1964:1969,
  special.predictors = list(list("gdpcap", 1960:1969, "mean"), list("sec.agriculture", seq(1961, 1969, 2), "mean"),
    list("sec.energy", seq(1961, 1969, 2), "mean"), list("sec.industry", seq(1961, 1969, 2), "mean"),
    list("sec.construction", seq(1961, 1969, 2), "mean"), list("sec.services.venta", seq(1961, 1969, 2), "mean"),
    list("sec.services.nonventa", seq(1961, 1969, 2), "mean"), list("popdens", 1969, "mean")),
  dependent = "gdpcap", unit.variable = "regionno", unit.names.variable = "regionname", time.variable = "year",
  treatment.identifier = 17, controls.identifier = c(2:16, 18), time.optimize.ssr = 1960:1969, time.plot = 1955:1997)
so <- synth(data.prep.obj = dp, method = "BFGS", quietly = TRUE)
ws <- round(so$solution.w, 3); rownames(ws) <- dp$names.and.numbers$unit.names[match(as.numeric(rownames(ws)), dp$names.and.numbers$unit.numbers)]
cat("predictor-based weights (> 0.001):\n"); print(ws[ws[, 1] > 0.001, , drop = FALSE])
gp <- as.vector(dp$Y1plot - dp$Y0plot %*% so$solution.w); names(gp) <- 1955:1997
cat("pre RMSPE 1960-69:", round(sqrt(mean(gp[as.character(1960:1969)]^2)), 3), "; average gap 1975-97:", round(mean(gp[as.character(1975:1997)]), 3), "\n")

## ---- 2. EDR and turnout ----------------------------------------------------------------
tr <- gsynth::turnout
cat("\n## 2. EDR: states", length(unique(tr$abb)), "; elections", paste(range(tr$year), collapse = "-"),
    "; treated states", length(unique(tr$abb[tr$policy_edr == 1])), "\n")
tw <- feols(turnout ~ policy_edr + policy_mail_in + policy_motor | abb + year, tr, cluster = ~abb)
cat("TWFE:", round(coef(tw)["policy_edr"], 2), "(", round(se(tw)["policy_edr"], 2), ")\n")
g1 <- gsynth(turnout ~ policy_edr + policy_mail_in + policy_motor, data = tr, index = c("abb", "year"),
             force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 500, parallel = FALSE)
cat("IFE (gsynth): factors chosen r =", g1$r.cv, "; ATT =", round(g1$est.avg[1], 2), "(", round(g1$est.avg[2], 2), ")\n")
g0 <- gsynth(turnout ~ policy_edr + policy_mail_in + policy_motor, data = tr, index = c("abb", "year"),
             force = "two-way", CV = FALSE, r = 0, se = TRUE, inference = "parametric", nboots = 500, parallel = FALSE)
cat("imputation with r = 0 (TWFE-style imputation): ATT =", round(g0$est.avg[1], 2), "(", round(g0$est.avg[2], 2), ")\n")
