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
