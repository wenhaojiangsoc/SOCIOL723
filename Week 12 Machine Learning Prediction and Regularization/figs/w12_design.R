## Week 12 -- where the 243 columns of the expanded GSS design come from (same formulas as figs/w12_gss.R)
##   Rscript figs/w12_design.R > figs/w12_design.out   (run from the week folder)
d <- readRDS("../Data/gss_earnings.rds")
d <- d[complete.cases(d[, c("lnearn", "educ", "age", "female", "black", "otherace", "wordsum", "pareduc",
                            "hours", "childs", "evermar", "region", "year")]), ]
d$region <- factor(d$region); d$yr <- factor(d$year)
f_main <- lnearn ~ educ + age + female + black + otherace + wordsum + pareduc + hours + childs + evermar + region + yr
num <- c("educ", "age", "wordsum", "pareduc", "hours", "childs")
f_big <- as.formula(paste("lnearn ~ (educ + age + female + black + otherace + wordsum + pareduc + hours + childs + evermar + region + yr)^2 +",
                          paste0("I(", num, "^2)", collapse = " + ")))
Xm <- model.matrix(f_main, d)[, -1]; Xb0 <- model.matrix(f_big, d)[, -1]
cat("main-effect columns:", ncol(Xm), "= 10 single variables +", nlevels(d$region) - 1, "region dummies +", nlevels(d$yr) - 1, "year dummies\n")
cat("pairs of the 23 columns:", choose(ncol(Xm), 2), "; R builds pairs of the 12 variables, not of dummies within one factor:",
    choose(ncol(Xm), 2) - choose(nlevels(d$region) - 1, 2) - choose(nlevels(d$yr) - 1, 2), "\n")
cat("columns built: main", ncol(Xm), "+ squares", length(num), "+ interactions", ncol(Xb0) - ncol(Xm) - length(num), "=", ncol(Xb0), "\n")
const <- apply(Xb0, 2, var) == 0
cat("dropped, always zero (cannot vary):", sum(const), ":", paste(head(colnames(Xb0)[const], 6), collapse = ", "), "\n")
Xb1 <- Xb0[, !const]; dup <- duplicated(t(Xb1))
cat("dropped, exact copy of another column:", sum(dup), ":", paste(head(colnames(Xb1)[dup], 6), collapse = ", "), "\n")
cat("expanded design:", ncol(Xb1) - sum(dup), "columns\n")
