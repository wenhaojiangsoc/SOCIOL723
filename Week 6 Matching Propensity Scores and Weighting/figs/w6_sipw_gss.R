## Week 6 -- stabilized IPW on the GSS. Every number on the "Stabilized Weights"
## frame comes from here.   Rscript figs/w6_sipw_gss.R
g <- readRDS("../Data/gss_earnings.rds")
g$D <- g$ba; g$Y <- g$lnearn
n <- nrow(g)

## ---- 1. propensity score: logit of BA on the five covariates ----------------
ps <- glm(D ~ female + black + pareduc + wordsum + exper, binomial, g)
g$e <- fitted(ps)
pi1 <- mean(g$D)                                   # marginal Pr(D = 1)
cat(sprintf("n = %d, treated share pi = %.3f, e-hat range %.3f to %.3f\n",
            n, pi1, min(g$e), max(g$e)))

## ---- 2. raw and stabilized ATE weights ---------------------------------------
g$w_raw <- ifelse(g$D == 1, 1 / g$e, 1 / (1 - g$e))          # 1/e, 1/(1-e)
g$w_stb <- ifelse(g$D == 1, pi1 / g$e, (1 - pi1) / (1 - g$e))  # Pr(D=d)/Pr(D=d|X)
summ <- function(w, d) c(mean = mean(w[g$D == d]), sd = sd(w[g$D == d]),
                         max = max(w[g$D == d]),
                         ess = sum(w[g$D == d])^2 / sum(w[g$D == d]^2))
out <- rbind(raw_treated = summ(g$w_raw, 1), raw_control = summ(g$w_raw, 0),
             stb_treated = summ(g$w_stb, 1), stb_control = summ(g$w_stb, 0))
print(round(out, 2))
cat(sprintf("group sizes: treated %d, control %d\n", sum(g$D), sum(1 - g$D)))
cat(sprintf("sum of raw weights: treated %.0f, control %.0f (each about n = %d)\n",
            sum(g$w_raw[g$D == 1]), sum(g$w_raw[g$D == 0]), n))
cat(sprintf("sum of stabilized weights: treated %.0f, control %.0f (about n1, n0)\n",
            sum(g$w_stb[g$D == 1]), sum(g$w_stb[g$D == 0])))

## ---- 3. four ATE estimates ------------------------------------------------------
ht  <- function(w) mean(w * g$D * g$Y) - mean(w * (1 - g$D) * g$Y)          # divide by n
haj <- function(w) sum(w * g$D * g$Y) / sum(w * g$D) -
                   sum(w * (1 - g$D) * g$Y) / sum(w * (1 - g$D))            # divide by sum w
cat(sprintf("Horvitz-Thompson: raw %.3f | stabilized %.3f\n", ht(g$w_raw), ht(g$w_stb)))
cat(sprintf("Hajek:            raw %.3f | stabilized %.3f\n", haj(g$w_raw), haj(g$w_stb)))
cat(sprintf("weighted regression lm(Y ~ D, weights = w): raw %.3f | stabilized %.3f\n",
            coef(lm(Y ~ D, g, weights = w_raw))["D"], coef(lm(Y ~ D, g, weights = w_stb))["D"]))
cat(sprintf("naive difference %.3f\n", mean(g$Y[g$D == 1]) - mean(g$Y[g$D == 0])))
