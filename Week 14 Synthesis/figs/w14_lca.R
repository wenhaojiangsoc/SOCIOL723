## Week 14 -- latent classes by EM, on Bollen's (1989) democracy ratings
## (lavaan::PoliticalDemocracy: 75 developing countries, 1960). Each of the four ratings
## (A press freedom, B freedom of political opposition, C fairness of elections,
## D effectiveness of the legislature, the 1965 rating y8: see figs/w14_factor.R) is split at its median: 1 = high, 0 = low.
## Every number on the latent-class frames comes from here.
##   Rscript figs/w14_lca.R > figs/w14_lca.out
suppressMessages({library(poLCA); library(lavaan)})
raw <- PoliticalDemocracy[, c("y1", "y2", "y3", "y8")]; names(raw) <- c("A", "B", "C", "D")
cat("medians:", sapply(raw, median), "\n")
X <- sapply(raw, function(v) as.numeric(v > median(v)))      # 1 = above the median
values <- as.data.frame(X + 1)                               # poLCA wants 1/2
cat("n =", nrow(X), "; share rated high by item:", round(colMeans(X), 2), "\n")
pat <- expand.grid(A = 0:1, B = 0:1, C = 0:1, D = 0:1)          # all 16 patterns, also the empty ones
pat$n <- apply(pat, 1, function(x) sum(apply(X, 1, function(r) all(r == x))))
pat$k <- rowSums(pat[, 1:4]); cat("\n## observed patterns\n"); print(pat, row.names = FALSE)
cat("countries by number of high ratings:\n"); print(tapply(pat$n, pat$k, sum))
cat("correlations among the four answers:\n"); print(round(cor(X), 2))

## ---- EM by hand, two classes ---------------------------------------------------------------
## unknowns: share of class 1 (pi1) and, per class, the chance of a universalistic answer to each item
lik <- function(p) apply(X, 1, function(x) prod(p^x * (1 - p)^(1 - x)))   # Pr(answers | class)
em <- function(pi1, p1, p2, steps, show = integer(0)) {
  for (s in 0:steps) {
    l1 <- pi1 * lik(p1); l2 <- (1 - pi1) * lik(p2)
    if (s %in% show) cat(sprintf("step %3d: share of class 1 %.3f | class 1 %s | class 2 %s | logLik %.2f\n",
        s, pi1, paste(sprintf("%.2f", p1), collapse = " "), paste(sprintf("%.2f", p2), collapse = " "), sum(log(l1 + l2))))
    if (s == steps) break
    g <- l1 / (l1 + l2)                                   # E step: each person's chance of class 1
    pi1 <- mean(g); p1 <- colSums(g * X) / sum(g); p2 <- colSums((1 - g) * X) / sum(1 - g) }  # M step
  list(pi1 = pi1, p1 = p1, p2 = p2, g = l1 / (l1 + l2), ll = sum(log(l1 + l2))) }
cat("\n## EM from a rough guess: classes equal in size, class 1 rated high 60% of the time, class 2 40%\n")
st <- list(pi1 = .5, p1 = rep(.6, 4), p2 = rep(.4, 4))
f <- em(st$pi1, st$p1, st$p2, 500, show = c(0, 1, 2, 5, 10, 20, 50, 100, 500))

## the first E step and M step, for the slide
l1 <- st$pi1 * lik(st$p1); l2 <- (1 - st$pi1) * lik(st$p2); g <- l1 / (l1 + l2)
for (k in 0:4) { i <- which(rowSums(X) == k)[1]
  cat(sprintf("first E step, %d high ratings: Pr(class 1) = %.3f\n", k, g[i])) }
cat(sprintf("  all four: .5 x .6^4 = %.4f; .5 x .4^4 = %.4f; ratio %.3f\n", .5 * .6^4, .5 * .4^4, .5 * .6^4 / (.5 * .6^4 + .5 * .4^4)))
cat(sprintf("first M step: class share = mean of these = %.3f; item A in class 1 = %.3f, in class 2 = %.3f\n",
            mean(g), sum(g * X[, 1]) / sum(g), sum((1 - g) * X[, 1]) / sum(1 - g)))

## ---- result, and the classes person by person ---------------------------------------------
cat("\n## result\n"); cat(sprintf("class 1 share %.3f, class 2 share %.3f\n", f$pi1, 1 - f$pi1))
print(round(rbind(class1 = f$p1, class2 = f$p2), 3))
post <- function(x) { l1 <- f$pi1 * prod(f$p1^x * (1 - f$p1)^(1 - x)); l1 / (l1 + (1 - f$pi1) * prod(f$p2^x * (1 - f$p2)^(1 - x))) }
pat$post1 <- apply(pat[, 1:4], 1, post)
## fit: observed pattern counts against the counts the model implies
pr <- function(x) f$pi1 * prod(f$p1^x * (1 - f$p1)^(1 - x)) + (1 - f$pi1) * prod(f$p2^x * (1 - f$p2)^(1 - x))
pat$expected <- nrow(X) * apply(pat[, 1:4], 1, pr)
print(round(pat[order(-pat$k), ], 2), row.names = FALSE)
G2 <- 2 * sum(ifelse(pat$n > 0, pat$n * log(pat$n / pat$expected), 0)); X2 <- sum((pat$n - pat$expected)^2 / pat$expected)
cat(sprintf("15 free pattern shares - 9 unknowns = 6 df; G2 = %.2f (p %.2f), Pearson X2 = %.2f (p %.2f)\n",
            G2, 1 - pchisq(G2, 6), X2, 1 - pchisq(X2, 6)))
## one class only (answers unrelated): expected counts from the four margins
e1 <- nrow(X) * apply(pat[, 1:4], 1, function(x) prod(colMeans(X)^x * (1 - colMeans(X))^(1 - x)))
cat(sprintf("one class (independence): expected all-four %.1f (observed %d), none %.1f (observed %d); X2 = %.1f on 11 df\n",
            e1[pat$k == 4], pat$n[pat$k == 4], e1[pat$k == 0], pat$n[pat$k == 0], sum((pat$n - e1)^2 / e1)))

## ---- other starting values; poLCA ----------------------------------------------------------
set.seed(723); cat("\n## ten random starts: logLik at the end\n")
cat(round(sapply(1:10, function(i) em(runif(1, .2, .8), runif(4), runif(4), 2000)$ll), 3), "\n")
set.seed(723); pl <- poLCA(cbind(A, B, C, D) ~ 1, values, nclass = 2, nrep = 10, verbose = FALSE)
cat(sprintf("poLCA: class shares %s; logLik %.3f; G2 %.2f; Chisq %.2f; df %d\n",
            paste(round(pl$P, 3), collapse = " "), pl$llik, pl$Gsq, pl$Chisq, pl$resid.df))
print(lapply(pl$probs, round, 3)$A)

## ---- how many classes? ------------------------------------------------------------------------
cat("\n## one, two, three classes (poLCA, 20 random starts each)\n")
for (k in 1:3) { set.seed(723)
  m <- poLCA(cbind(A, B, C, D) ~ 1, values, nclass = k, nrep = 20, verbose = FALSE)
  cat(sprintf("%d class(es): unknowns %2d, logLik %.2f, X2 %.2f, G2 %.2f, df %d, BIC %.1f, AIC %.1f; shares %s\n",
              k, m$npar, m$llik, m$Chisq, m$Gsq, m$resid.df, m$bic, m$aic, paste(round(sort(m$P), 2), collapse = " "))) }
## the log likelihood by hand at the starting guess and at the end: sum over students of log(mixture)
cat(sprintf("Pr(1,1,1,1) at the estimates: %.3f x %.3f + %.3f x %.3f = %.3f; x n = %.1f\n",
            f$pi1, prod(f$p1), 1 - f$pi1, prod(f$p2), f$pi1 * prod(f$p1) + (1 - f$pi1) * prod(f$p2),
            nrow(X) * (f$pi1 * prod(f$p1) + (1 - f$pi1) * prod(f$p2))))
## a general-purpose optimizer on the same log likelihood (logit scale), for comparison with EM
nll <- function(th) { q <- plogis(th); -sum(log(q[1] * lik(q[2:5]) + (1 - q[1]) * lik(q[6:9]))) }
o <- optim(qlogis(c(.5, rep(.6, 4), rep(.4, 4))), nll, method = "BFGS", control = list(maxit = 1000))
cat(sprintf("quasi-Newton (optim BFGS) from the same start: logLik %.3f after %d function calls; share %.3f\n",
            -o$value, o$counts[1], plogis(o$par[1])))

## the types against the factor score of figs/w14_factor.R
d <- as.data.frame(scale(raw)); fs <- lavPredict(cfa("T =~ A + B + C + D", d, std.lv = TRUE))
cat(sprintf("\ncorrelation of Pr(type 1) with the factor score: %.2f; mean factor score by most likely type: %s\n",
            cor(f$g, fs), paste(round(tapply(fs, f$g > .5, mean), 2), collapse = " / ")))
cat("countries with Pr(type 1) between 0.1 and 0.9:", sum(f$g > .1 & f$g < .9), "\n")

## ---- from the nine numbers to a country's type: Bayes' rule, one pattern written out ---------
x <- c(1, 1, 0, 1)
t1 <- f$pi1 * prod(f$p1^x * (1 - f$p1)^(1 - x)); t2 <- (1 - f$pi1) * prod(f$p2^x * (1 - f$p2)^(1 - x))
cat(sprintf("\nratings (1,1,0,1): type 1 %.3f x %.3f x %.3f x %.3f x %.3f = %.4f; type 2 %.3f x %.3f x %.3f x %.3f x %.3f = %.4f; Pr(type 1) = %.3f\n",
            f$pi1, f$p1[1], f$p1[2], 1 - f$p1[3], f$p1[4], t1, 1 - f$pi1, f$p2[1], f$p2[2], 1 - f$p2[3], f$p2[4], t2, t1 / (t1 + t2)))
cat("countries assigned to the more likely type:", table(ifelse(f$g > .5, "type 1", "type 2")), "\n")
cat("countries with Pr of their assigned type above 0.9:", sum(pmax(f$g, 1 - f$g) > .9), "of", length(f$g), "\n")
