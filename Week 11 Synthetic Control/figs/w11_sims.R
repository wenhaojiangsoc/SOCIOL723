## Week 11 -- why the pre-period does the work: SC vs DiD under a factor model.
## Y_jt(0) = delta_t + lambda_t' mu_j + eps_jt, 2 factors, 20 donors; the treated unit's loadings are a
## convex combination of three donors but far from the donor average. True effect = 0.
##   Rscript figs/w11_sims.R > figs/w11_sims.out
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
suppressMessages(library(quadprog))
sc_w <- function(y1, Y0) { J <- nrow(Y0); D <- Y0 %*% t(Y0) + diag(1e-8, J)
  s <- solve.QP(D, Y0 %*% y1, cbind(rep(1, J), diag(J)), c(1, rep(0, J)), meq = 1)$solution; pmax(s, 0) / sum(pmax(s, 0)) }
J <- 20; Tpost <- 10; R <- 1000
one <- function(T0, sig) {
  Tn <- T0 + Tpost
  mu <- matrix(runif(2 * J), J, 2)
  mu1 <- c(0.6, 0.3) %*% mu[1:2, ] + 0.1 * mu[3, ]                 # in the convex hull of donors 1-3
  lam <- cbind(seq(0, 3, length.out = Tn) + rnorm(Tn, 0, 0.3), cumsum(rnorm(Tn, 0, 0.5)))
  dl <- cumsum(rnorm(Tn))
  Y0 <- t(sapply(1:J, function(j) dl + lam %*% mu[j, ] + rnorm(Tn, 0, sig)))
  y1 <- as.vector(dl + lam %*% t(mu1) + rnorm(Tn, 0, sig))
  pr <- 1:T0; po <- (T0 + 1):Tn
  w <- sc_w(y1[pr], Y0[, pr, drop = FALSE])
  sc <- mean(y1[po] - colSums(w * Y0[, po])) 
  did <- (mean(y1[po]) - mean(y1[pr])) - (mean(colMeans(Y0[, po])) - mean(colMeans(Y0[, pr])))
  c(sc = sc, did = did, fit = sqrt(mean((y1[pr] - colSums(w * Y0[, pr]))^2)))
}
for (sig in c(0.25, 1)) for (T0 in c(3, 5, 10, 20, 40)) {
  r <- t(replicate(R, one(T0, sig)))
  cat(sprintf("sigma %.2f  T0 %2d | SC mean abs error %.3f  RMSE %.3f | DiD mean abs error %.3f  RMSE %.3f | pre-fit RMSPE %.2f\n",
      sig, T0, mean(abs(r[, "sc"])), sqrt(mean(r[, "sc"]^2)), mean(abs(r[, "did"])), sqrt(mean(r[, "did"]^2)), mean(r[, "fit"])))
}
