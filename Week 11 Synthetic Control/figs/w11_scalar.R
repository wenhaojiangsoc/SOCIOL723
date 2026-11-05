## Week 11 -- scalar build-up of synthetic control: donor average (DiD), two donors and one weight, predictors and v, penalized toy.
## Data: tidysynth::smoking (39 states, 1970-2000; cigsale = packs per capita). California treated from 1989.
##   Rscript figs/w11_scalar.R > figs/w11_scalar.out   (run from the week folder); writes figs/two_donor.pdf, figs/prop99_naive.pdf
suppressMessages(library(dplyr))
data(smoking, package = "tidysynth")
sm <- as.data.frame(smoking)
Y <- reshape(sm[, c("state", "year", "cigsale")], idvar = "state", timevar = "year", direction = "wide")
rownames(Y) <- Y$state; Y <- as.matrix(Y[, -1]); colnames(Y) <- 1970:2000
pre <- as.character(1970:1988); post <- as.character(1989:2000)
y1 <- Y["California", ]; Y0 <- Y[rownames(Y) != "California", ]

## ---- 1. what most people do: the donor average plus the pre-period gap (DiD, w_j = 1/38) ----
dbar <- colMeans(Y0)
cat("## 1. donor average\n")
cat("California mean 1970-88:", round(mean(y1[pre]), 1), "; donors:", round(mean(dbar[pre]), 1),
    "; pre gap:", round(mean(y1[pre] - dbar[pre]), 1), "\n")
cat("California mean 1989-2000:", round(mean(y1[post]), 1), "; donors:", round(mean(dbar[post]), 1), "\n")
did <- (mean(y1[post]) - mean(y1[pre])) - (mean(dbar[post]) - mean(dbar[pre]))
cat("DiD = (CA post - CA pre) - (donor post - donor pre):", round(did, 1), "\n")
cat("1970 / 1988: California", y1["1970"], "/", y1["1988"], "; donors", round(dbar["1970"], 1), "/", round(dbar["1988"], 1), "\n")
cat("change 1970-88: California", round(y1["1988"] - y1["1970"], 1), "; donors", round(dbar["1988"] - dbar["1970"], 1), "\n")

## ---- 2. two donors, one weight: Yhat = w Utah + (1 - w) Nevada --------------------------------
cat("\n## 2. two donors, one weight\n")
A <- Y0["Utah", ]; B <- Y0["Nevada", ]
w_closed <- sum((y1[pre] - B[pre]) * (A[pre] - B[pre])) / sum((A[pre] - B[pre])^2)
grid <- seq(0, 1, by = 0.001)
ssr <- sapply(grid, function(w) sum((y1[pre] - w * A[pre] - (1 - w) * B[pre])^2))
cat("closed form w (Utah):", round(w_closed, 3), "; grid minimum:", grid[which.min(ssr)], "\n")
cat("numerator:", round(sum((y1[pre] - B[pre]) * (A[pre] - B[pre])), 1),
    "; denominator:", round(sum((A[pre] - B[pre])^2), 1), "\n")
fit <- w_closed * A + (1 - w_closed) * B
cat("pre-period RMSPE:", round(sqrt(mean((y1 - fit)[pre]^2)), 2), "; average gap 1989-2000:", round(mean((y1 - fit)[post]), 1), "\n")
cat("1988: CA", y1["1988"], " Utah", A["1988"], " Nevada", B["1988"], " synthetic", round(fit["1988"], 1), "\n")
cat("2000: CA", y1["2000"], " Utah", A["2000"], " Nevada", B["2000"], " synthetic", round(fit["2000"], 1), "\n")
cat("SSR at w = 0, .25, .5, closed, .75, 1:",
    round(sapply(c(0, .25, .5, w_closed, .75, 1), function(w) sum((y1[pre] - w * A[pre] - (1 - w) * B[pre])^2))), "\n")
cat("pre means: CA", round(mean(y1[pre]), 1), " Utah", round(mean(A[pre]), 1), " Nevada", round(mean(B[pre]), 1), "\n")

## ---- 2b. nested choice of v and w: two donors, two covariates (retail price 1980-88, beer 1984-88) ----
cat("\n## 2b. two donors, two predictors: inner w(v), outer v\n")
pv <- function(s, var, yrs) mean(sm[sm$state == s & sm$year %in% yrs, var], na.rm = TRUE)
X <- sapply(c("California", "Utah", "Nevada"), function(s) c(price = pv(s, "retprice", 1980:1988), beer = pv(s, "beer", 1984:1988)))
print(round(X, 2))
w_of_v <- function(v) {            # inner step: weighted least squares through the origin, clipped to [0, 1]
  a <- X[, "Utah"] - X[, "Nevada"]; b <- X[, "California"] - X[, "Nevada"]
  min(max(sum(v * a * b) / sum(v * a^2), 0), 1) }
ssr_w <- function(w) sum((y1[pre] - w * A[pre] - (1 - w) * B[pre])^2)
vg <- seq(0, 1, by = 0.001)        # v_price; v_beer = 1 - v_price
tab <- t(sapply(vg, function(v1) { w <- w_of_v(c(v1, 1 - v1)); c(v_price = v1, w = w, ssr = ssr_w(w)) }))
best <- tab[which.min(tab[, "ssr"]), ]
print(round(tab[tab[, "v_price"] %in% c(0, .5, best["v_price"], 1), ], 3))
cat("outer step picks v_price =", best["v_price"], "; w =", round(best["w"], 3), "; SSR =", round(best["ssr"], 1),
    "; RMSPE =", round(sqrt(best["ssr"] / 19), 2), "\n")
cat("for comparison, w fitted to outcomes directly: w =", round(w_closed, 3), "; SSR =", round(ssr_w(w_closed), 1), "\n")

## ---- 2c. the two steps on Prop 99: seven ADH predictors, 38 donors, several choices of v ------------
## Predictors standardized by their sd across the 39 states (as Synth/tidysynth do); inner step by quadratic programming.
## The last v is tidysynth's outer-step choice; it reproduces figs/w11_prop99.R section 1 (Utah .342, ...).
cat("\n## 2c. Prop 99: w(v) for several v\n")
suppressMessages({library(tidysynth); library(quadprog)})
sc <- sm %>% synthetic_control(outcome = cigsale, unit = state, time = year, i_unit = "California", i_time = 1988, generate_placebos = FALSE) %>%
  generate_predictor(time_window = 1980:1988, ln_income = mean(lnincome, na.rm = TRUE), ret_price = mean(retprice, na.rm = TRUE), youth = mean(age15to24, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1984:1988, beer_sales = mean(beer, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1975, cigsale_1975 = cigsale) %>%
  generate_predictor(time_window = 1980, cigsale_1980 = cigsale) %>%
  generate_predictor(time_window = 1988, cigsale_1988 = cigsale) %>%
  generate_weights(optimization_window = 1970:1988, margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>% generate_control()
pt <- grab_predictors(sc, type = "treated"); pc <- grab_predictors(sc, type = "controls")
XP <- cbind(California = pt$California, as.matrix(pc[, -1])); rownames(XP) <- pt$variable
XPs <- XP / apply(XP, 1, sd)
vstar <- grab_predictor_weights(sc)$weight; names(vstar) <- rownames(XP)
inner <- function(v) {             # inner step: min sum_k v_k (X_1k - sum_j w_j X_jk)^2, w >= 0, sum w = 1
  Am <- XPs[, -1]; J <- ncol(Am); Dm <- t(Am) %*% diag(v) %*% Am + diag(1e-10, J); dv <- t(Am) %*% diag(v) %*% XPs[, 1]
  s <- solve.QP(Dm, dv, cbind(rep(1, J), diag(J)), c(1, rep(0, J)), meq = 1)$solution; s[s < 1e-6] <- 0; s <- s / sum(s)
  names(s) <- colnames(Am); s }
ev <- function(v, lab) {
  w <- inner(v / sum(v)); g <- y1 - colSums(w * Y0[names(w), ])
  top <- sort(w[w > 0.001], decreasing = TRUE)
  cat(sprintf("%-22s outcome SSR %8.1f  RMSPE %5.2f  average gap %6.1f  donors > 0: %d\n", lab, sum(g[pre]^2), sqrt(mean(g[pre]^2)), mean(g[post]), length(top)))
  print(round(head(top, 4), 3)); invisible(w) }
ev(c(1, 1, 1, 1, 0, 0, 0), "covariates only")
ev(rep(1, 7), "all seven, equal v")
ev(c(0, 0, 0, 0, 1, 1, 1), "three sales years")
wstar <- ev(vstar, "outer-step v")
cat("outer-step v:\n"); print(round(vstar, 3))
cat("balance (chosen w): CA / synthetic / donor average\n")
print(round(cbind(CA = XP[, 1], synthetic = as.vector(XP[, names(wstar)] %*% wstar), donors = rowMeans(XP[, -1])), 3))

## ---- 2d. outcomes only, equal v: inner objective = outer objective, so no search is needed ----------------
cat("\n## 2d. outcomes only (19 pre-years, equal v)\n")
Am <- Y0[, pre]; J <- nrow(Am)
wo <- solve.QP(Am %*% t(Am) + diag(1e-8, J), Am %*% y1[pre], cbind(rep(1, J), diag(J)), c(1, rep(0, J)), meq = 1)$solution
wo[wo < 1e-6] <- 0; wo <- wo / sum(wo); names(wo) <- rownames(Y0); go <- y1 - colSums(wo * Y0)
cat("outcome SSR:", round(sum(go[pre]^2), 1), "; RMSPE:", round(sqrt(mean(go[pre]^2)), 2), "; average gap:", round(mean(go[post]), 1), "\n")
print(round(sort(wo[wo > 0.001], decreasing = TRUE), 3))

## ---- 2e. penalized SC toy (hypothetical) ------------------------------------------------------------
cat("\n## 2e. penalized SC toy: X1 = 5; donors A 4, B 6, C 0, D 10\n")
xd <- c(A = 4, B = 6, C = 0, D = 10); x1 <- 5
pen <- function(w) c(fit = (x1 - sum(w * xd))^2, penalty = sum(w * (x1 - xd)^2))
print(rbind(`1/2 A + 1/2 B` = pen(c(.5, .5, 0, 0)), `1/2 C + 1/2 D` = pen(c(0, 0, .5, .5))))
lam <- 0.1; Dm <- outer(xd, xd) + diag(1e-9, 4); dv <- x1 * xd - lam / 2 * (x1 - xd)^2
wp <- solve.QP(Dm, dv, cbind(rep(1, 4), diag(4)), c(1, rep(0, 4)), meq = 1)$solution
cat("penalized solution at lambda = 0.1:", round(wp, 3), "\n")

## ---- 4. figures -----------------------------------------------------------------------------------
pdf("figs/two_donor.pdf", width = 9, height = 3.4, family = "Times")
par(mar = c(4, 4.2, 1, 1))
yrs <- 1970:2000
plot(yrs, y1, type = "l", lwd = 2.5, col = "firebrick", ylim = c(30, 210), xlab = "year", ylab = "packs per capita")
lines(yrs, A, lwd = 1.5, col = "gray55"); lines(yrs, B, lwd = 1.5, lty = 3, col = "gray30")
lines(yrs, fit, lwd = 2.5, lty = 2, col = "navy")
abline(v = 1988.5, lty = 2, col = "gray50")
legend("topright", c("California", sprintf("%.3f Utah + %.3f Nevada", w_closed, 1 - w_closed), "Utah", "Nevada"),
       col = c("firebrick", "navy", "gray55", "gray30"), lty = c(1, 2, 1, 3), lwd = c(2.5, 2.5, 1.5, 1.5), bty = "n")
dev.off()

## prop99_naive.pdf: California and the 38-donor average only (the naive comparison, before any weights are fitted)
pdf("figs/prop99_naive.pdf", width = 9, height = 3.4, family = "Times")
par(mar = c(4, 4.2, 1, 1))
plot(1970:2000, y1, type = "l", lwd = 2.5, col = "firebrick", ylim = c(30, 140), xlab = "year", ylab = "packs per capita")
lines(1970:2000, dbar, lwd = 2, lty = 3, col = "gray40")
abline(v = 1988.5, lty = 2, col = "gray50")
legend("bottomleft", c("California", "average of 38 donor states"), col = c("firebrick", "gray40"), lty = c(1, 3), lwd = 2, bty = "n")
dev.off()
