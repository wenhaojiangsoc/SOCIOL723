## Week 5: numbers for the Baron-Kenny frames. Run from the week folder.
set.seed(723); n <- 200000
d <- rnorm(n); u <- rnorm(n)
## clean world: no M-Y confounding
m0 <- 0.8 * d + rnorm(n);            y0 <- 1.0 * d + 1.5 * m0 + rnorm(n)
## confounded world: u -> M and u -> Y, u unmeasured; same a, b, c'
m1 <- 0.8 * d + u + rnorm(n);        y1 <- 1.0 * d + 1.5 * m1 + u + rnorm(n)
bk <- function(d, m, y) {
  c_tot <- coef(lm(y ~ d))["d"]; a <- coef(lm(m ~ d))["d"]
  f <- coef(lm(y ~ d + m)); c(c = c_tot, a = a, b = f["m"], c_prime = f["d"], ab = a * f["m"], share = a * f["m"] / c_tot)
}
print(round(rbind(clean = bk(d, m0, y0), confounded = bk(d, m1, y1)), 3))
cat("truth: c 2.2, a 0.8, b 1.5, c' 1.0, ab 1.2, share 0.545\n")
## GSS: parents' education -> own education -> log earnings
gss <- readRDS("../Data/gss_earnings.rds")
c_tot <- coef(lm(lnearn ~ pareduc + female + black + age, gss))["pareduc"]
a <- coef(lm(educ ~ pareduc + female + black + age, gss))["pareduc"]
f <- coef(lm(lnearn ~ pareduc + educ + female + black + age, gss))
print(round(c(c = c_tot, a = a, b = f["educ"], c_prime = f["pareduc"], ab = a * f["educ"], share = a * f["educ"] / c_tot), 4))
