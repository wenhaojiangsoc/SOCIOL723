## Week 12 -- partial dependence for the GSS benchmark forest (same data, split, ranger call as
## figs/w12_importance.R). Every number on the partial-dependence frame comes from here.
##   Rscript figs/w12_pdp.R > figs/w12_pdp.out   (run from the week folder)
## writes figs/pdp_gss.pdf
set.seed(723)
suppressMessages(library(ranger))
d <- readRDS("../Data/gss_earnings.rds")
d <- d[complete.cases(d[, c("lnearn", "educ", "age", "female", "black", "otherace", "wordsum", "pareduc",
                            "hours", "childs", "evermar", "region", "year")]), ]
d$region <- factor(d$region); d$yr <- factor(d$year)
f_main <- lnearn ~ educ + age + female + black + otherace + wordsum + pareduc + hours +
  childs + evermar + region + yr
Xm <- model.matrix(f_main, d)[, -1]
y <- d$lnearn; n <- nrow(d)
te <- sample(n, round(0.3 * n)); tr <- setdiff(1:n, te)      # identical to w12_gss.R
dtr <- data.frame(y = y[tr], Xm[tr, ])
rf <- ranger(y ~ ., dtr, num.trees = 1000, seed = 723)
X <- dtr[, -1]
## partial dependence: set EVERYONE's value of v to a, predict, average; repeat over a grid
pd <- function(v, grid) sapply(grid, function(a) { z <- X; z[[v]] <- a; mean(predict(rf, z)$predictions) })
ages <- 25:64; edus <- 8:20
pa <- pd("age", ages); pe <- pd("educ", edus)
cat("partial dependence on age (2,456 training cases each):\n")
print(round(setNames(pa, ages)[as.character(c(25, 30, 40, 50, 60, 64))], 3))
cat("partial dependence on education:\n"); print(round(setNames(pe, edus)[as.character(c(8, 10, 12, 14, 16, 18, 20))], 3))
## the first value by hand: everyone aged 30
z <- X; z$age <- 30; p30 <- predict(rf, z)$predictions
cat("age 30 by hand: first three people's own ages", X$age[1:3], "-> predictions at age 30:",
    round(p30[1:3], 3), "; average", round(mean(p30), 3), "\n")
cat("ages in the training data:", range(X$age),
"\n")
pdf("figs/pdp_gss.pdf", width = 7, height = 2.7, family = "Times")
par(mfrow = c(1, 2), mar = c(3.2, 3.8, 0.8, 0.8), mgp = c(2.1, 0.55, 0))
plot(ages, pa, type = "l", lwd = 2.2, col = "navy", las = 1, xlab = "age (set for everyone)",
     ylab = "average prediction")
rug(jitter(X$age), col = "gray60")
plot(edus, pe, type = "s", lwd = 2.2, col = "navy", las = 1, xlab = "years of schooling (set for everyone)",
     ylab = "average prediction")
rug(jitter(X$educ), col = "gray60")
dev.off()
