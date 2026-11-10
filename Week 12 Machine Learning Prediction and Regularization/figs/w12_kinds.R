## Week 12 -- three kinds of machine learning, each on the GSS (slide 'Three Kinds of Machine Learning')
##   Rscript figs/w12_kinds.R > figs/w12_kinds.out   (run from the week folder)
## writes figs/kinds_gss.pdf
set.seed(723)
d <- readRDS("../Data/gss_earnings.rds")
d <- d[complete.cases(d[, c("lnearn", "educ", "age", "female", "wordsum", "pareduc", "sei", "prestige", "hours")]), ]
idx <- sample(nrow(d), 500)                                       # points shown
jit <- function(v, a) v + runif(length(v), -a, a)
pdf("figs/kinds_gss.pdf", width = 6.3, height = 1.95, family = "Times")
par(mfrow = c(1, 3), mar = c(2.7, 2.9, 1.5, 0.4), mgp = c(1.6, 0.45, 0), cex.axis = 0.95, cex.lab = 1.0)
## 1. supervised: X = schooling, recorded Y = log earnings; learn a rule
f1 <- lm(lnearn ~ educ, d)
plot(jit(d$educ[idx], 0.3), d$lnearn[idx], pch = 19, cex = 0.45, col = adjustcolor("gray40", 0.5), las = 1,
     xlab = "years of schooling (X)", ylab = "log earnings (Y)", main = "supervised: X and Y", font.main = 1, cex.main = 1.05)
abline(f1, col = "firebrick", lwd = 2)
## 2. unsupervised: X only (SEI, prestige); find the main direction
z <- scale(d[, c("sei", "prestige")])
plot(z[idx, 1], z[idx, 2], pch = 19, cex = 0.45, col = adjustcolor("gray40", 0.5), las = 1, asp = 1,
     xlab = "SEI (standardized)", ylab = "prestige (standardized)", main = "unsupervised: X only", font.main = 1, cex.main = 1.05)
abline(0, 1, col = "firebrick", lwd = 2)
## 3. self-supervised: hide schooling, predict it from the other columns; held-out half
h <- sample(nrow(d), nrow(d) / 2); f3 <- lm(educ ~ age + female + wordsum + pareduc + hours, d[h, ])
ph <- predict(f3, d[-h, ]); r <- cor(ph, d$educ[-h])
k <- sample(seq_along(ph), 500)
plot(jit(d$educ[-h][k], 0.3), ph[k], pch = 19, cex = 0.45, col = adjustcolor("gray40", 0.5), las = 1, xlim = c(4, 20), ylim = c(4, 20),
     xlab = "hidden schooling (actual)", ylab = "predicted from the rest", main = "self-supervised: hide, predict", font.main = 1, cex.main = 1.05)
abline(0, 1, col = "firebrick", lwd = 2)
dev.off()
cat("supervised slope", round(coef(f1)[2], 3), "; corr(SEI, prestige)", round(cor(d$sei, d$prestige), 2),
    "; self-supervised: corr(predicted, actual schooling) on the held-out half", round(r, 2), "\n")
