## Week 13 -- Week 6's AIPW on the NLSY79 with parametric models (OLS per arm, logit propensity),
## unweighted, to set beside the DML version (forests, cross-fitted) in figs/w13_designs.R, 1.
##   Rscript figs/w13_aipw_nlsy.R > figs/w13_aipw_nlsy.out
d <- readRDS("../Data/nlsy79_brand.rds")
xvars <- c("male","black","hispanic","south","rural","parinc","fatheduc","motheduc",
           "fatherwc","twoparent","siblings","ability","collegeprep","rotter","delinq",
           "expect","aspire","friendasp","schooldis","married18","child18",
           "parinc_miss","schooldis_miss","fatheduc_miss")
f <- as.formula(paste("~", paste(xvars, collapse = "+")))
Y <- d$lowwage; D <- d$college
e  <- fitted(glm(update(f, college ~ .), binomial, d))                       # logit propensity
m1 <- predict(lm(update(f, lowwage ~ .), d[D == 1, ]), d)                    # OLS among graduates
m0 <- predict(lm(update(f, lowwage ~ .), d[D == 0, ]), d)                    # OLS among non-graduates
cat("logit propensity: min", round(min(e), 4), "max", round(max(e), 4), "\n")
ate <- m1 - m0 + D * (Y - m1) / e - (1 - D) * (Y - m0) / (1 - e)
att <- (D * (Y - m0) - (1 - D) * e / (1 - e) * (Y - m0)) / mean(D)
cat("AIPW, OLS + logit: ATE", round(mean(ate), 3), "(", round(sd(ate) / sqrt(length(ate)), 3), ")\n")
cat("AIPW, OLS + logit: ATT", round(mean(att), 3), "(", round(sd(att - D * mean(att) / mean(D)) / sqrt(length(att)), 3), ")\n")
