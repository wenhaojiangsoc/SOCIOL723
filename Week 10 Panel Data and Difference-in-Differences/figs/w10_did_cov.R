## Week 10 -- the two-store-type DiD example (hypothetical, no noise) used on the covariate slides.
## 100 stores per state. Untreated change: type A -3, type B -1. The law adds +2 to every NJ store.
## NJ: 20 type A, 80 type B. PA: 80 type A, 20 type B.
## Also: TWFE with a type dummy (absorbed by store effects) vs TWFE with type x period effects;
## and a second version where the effect differs by type (A: +4, B: +1), to show TWFE's cell weights.
##   Rscript figs/w10_did_cov.R > figs/w10_did_cov.out
suppressMessages(library(fixest)); setFixest_notes(FALSE)
build <- function(eff) {
  st <- data.frame(store = 1:200, nj = rep(c(1, 0), each = 100),
                   type = c(rep("A", 20), rep("B", 80), rep("A", 80), rep("B", 20)))
  d <- merge(st, data.frame(post = 0:1))
  trend <- ifelse(d$type == "A", -3, -1)
  d$D <- d$nj * d$post
  d$y <- 20 + trend * d$post + eff[d$type] * d$D
  d }
for (eff in list(c(A = 2, B = 2), c(A = 4, B = 1))) {
  d <- build(eff)
  att <- mean(eff[d$type[d$nj == 1 & d$post == 1]])
  plain  <- coef(feols(y ~ D | store + post, d))[["D"]]
  absorb <- coef(feols(y ~ D + i(type) | store + post, d))[["D"]]
  bytype <- coef(feols(y ~ D | store + type^post, d))[["D"]]
  ## regression (g-computation) on the change: untreated change by type from PA, predicted for NJ
  w <- reshape(d[, c("store", "post", "y", "nj", "type")], idvar = c("store", "nj", "type"),
               timevar = "post", direction = "wide")
  w$dy <- w$y.1 - w$y.0
  m0 <- tapply(w$dy[w$nj == 0], w$type[w$nj == 0], mean)
  greg <- mean(w$dy[w$nj == 1]) - mean(m0[w$type[w$nj == 1]])
  cat(sprintf("effects A=%g B=%g | true ATT %.2f | TWFE %.2f | TWFE + type dummy %.2f | TWFE + type x period %.2f | regression %.2f\n",
              eff["A"], eff["B"], att, plain, absorb, bytype, greg))
}

## Why TWFE + type x period gives 2.50, not 1.60, when effects differ (A +4, B +1).
## Within each type, OLS weights the type's effect by n_type * Var(D | type) = n_type * e(1 - e);
## the ATT weights by the number of NJ stores of that type.
n  <- c(A = 100, B = 100); e <- c(A = 0.2, B = 0.8)
vw <- n * e * (1 - e); vw <- vw / sum(vw)
aw <- n * e;           aw <- aw / sum(aw)
cat(sprintf("variance weights A %.2f B %.2f -> %.2f | ATT weights A %.2f B %.2f -> %.2f\n",
            vw["A"], vw["B"], sum(vw * c(4, 1)), aw["A"], aw["B"], sum(aw * c(4, 1))))

## Time-varying X affected by the law (bad control). Hypothetical, 100 stores per state, two periods.
## X_it = hours per worker; the law cuts hours by 1 in NJ. Employment rises by 1 directly and by 1 more
## because each worker works fewer hours: total effect of the law = 2.
set.seed(723)
b <- merge(data.frame(store = 1:200, nj = rep(c(1, 0), each = 100)), data.frame(post = 0:1))
b$D <- b$nj * b$post
b$hours <- 35 + rnorm(nrow(b), 0, 0.3) - 1 * b$D
b$y <- 20 + 0.5 * b$post + 1 * b$D - 1 * (b$hours - 35) + rnorm(nrow(b), 0, 0.3)
cat(sprintf("bad control: TWFE %.2f | TWFE + hours_it %.2f | truth (total effect) 2\n",
            coef(feols(y ~ D | store + post, b))[["D"]],
            coef(feols(y ~ D + hours | store + post, b))[["D"]]))
