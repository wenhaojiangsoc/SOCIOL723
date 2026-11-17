## Week 12 -- Representations section (after the 690S Week 14 / SICSS NYU-Shanghai deck).
## Every number on the 'Representations' frames comes from here.
##   Rscript figs/w12_repr.R > figs/w12_repr.out   (run from the week folder)
## writes figs/pca_sei_prestige.pdf, figs/pca_image.pdf
set.seed(723)

## ---- A. PCA on two variables: SEI and occupational prestige -------------------
d <- readRDS("../Data/gss_earnings.rds")
v5 <- c("educ", "pareduc", "wordsum", "prestige", "sei")
d <- d[complete.cases(d[, c("lnearn", v5)]), ]
cat("n complete:", nrow(d), "\n")
z <- scale(d[, c("sei", "prestige")])
r <- cor(z)[1, 2]
p2 <- prcomp(z)
cat("\nA. corr(SEI, prestige):", round(r, 3), "\n")
cat("PC1 loadings:", round(p2$rotation[, 1], 3), "; variance share PC1:",
    round(p2$sdev[1]^2 / sum(p2$sdev^2), 3), " (1 + r)/2 =", round((1 + r) / 2, 3), "\n")
## one person, worked by hand
## a person off the diagonal: high SEI, middling prestige
i <- which.min((z[, 1] - 1.5)^2 + (z[, 2] - 0.2)^2)
zi <- z[i, ]
s1 <- sum(abs(p2$rotation[, 1]) * zi)
cat("person: SEI", d$sei[i], "prestige", d$prestige[i], "; z =", round(zi, 2),
    "; PC1 score =", round(s1, 2), "; reconstruction =", round(abs(p2$rotation[, 1]) * s1, 2), "\n")

pdf("figs/pca_sei_prestige.pdf", width = 4.4, height = 3.6, family = "Times")
par(mar = c(3.4, 3.6, 0.6, 0.6), mgp = c(2.1, 0.55, 0), tcl = -0.3)
idx <- sample(nrow(z), 600)
plot(z[idx, 1], z[idx, 2], type = "n", asp = 1, bty = "l", las = 1,
     xlab = "SEI (standardized)", ylab = "prestige (standardized)", xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5))
points(z[idx, 1], z[idx, 2], pch = 21, bg = adjustcolor("navy", 0.35), col = "white", lwd = 0.4, cex = 0.8)
abline(0, 1, col = "firebrick", lwd = 2.2)
abline(0, -1, col = "gray45", lwd = 1.2, lty = 2)
pr <- rep(s1 / sqrt(2), 2)
segments(zi[1], zi[2], pr[1], pr[2], col = "darkgreen", lwd = 2)
points(zi[1], zi[2], pch = 21, bg = "darkgreen", col = "white", cex = 1.4)
points(pr[1], pr[2], pch = 4, col = "darkgreen", cex = 1.3, lwd = 2)
text(2.3, 2.45, expression("PC1: " * c[1]), col = "firebrick", cex = 0.9, adj = 1)
text(-2.3, 2.45, expression("PC2: " * c[2]), col = "gray35", cex = 0.9, adj = 0)
dev.off()

## ---- B. PCA on five status variables ----------------------------------------
Z <- scale(d[, v5])
p5 <- prcomp(Z)
cat("\nB. correlations:\n"); print(round(cor(Z), 2))
cat("variance shares:", round(p5$sdev^2 / sum(p5$sdev^2), 3), "\n")
cat("cumulative:", round(cumsum(p5$sdev^2) / sum(p5$sdev^2), 3), "\n")
L <- p5$rotation; if (sum(L[, 1]) < 0) L[, 1] <- -L[, 1]
cat("PC1 loadings:\n"); print(round(L[, 1], 3))
cat("PC2 loadings:\n"); print(round(L[, 2], 3))
sc <- Z %*% L
cat("R2 of lnearn on all five:", round(summary(lm(d$lnearn ~ Z))$r.squared, 3),
    "; on PC1:", round(summary(lm(d$lnearn ~ sc[, 1]))$r.squared, 3),
    "; on PC1-2:", round(summary(lm(d$lnearn ~ sc[, 1:2]))$r.squared, 3), "\n")
## reconstruction error with K = 1 (mean squared, per entry)
recon1 <- sc[, 1] %o% L[, 1]
mse_pca <- mean((Z - recon1)^2)
cat("PCA K=1 reconstruction MSE per entry:", round(mse_pca, 4),
    " = 1 - share1 * (n-1)/n:", round((1 - p5$sdev[1]^2 / 5) * (nrow(Z) - 1) / nrow(Z), 4), "\n")

## ---- B2. A linear autoencoder (5 -> 1 -> 5) by gradient descent -------------
## encoder: E_i = sum_j a_j Z_ij ; decoder: Zhat_ij = b_j E_i ; loss = mean squared error
a <- rnorm(5, 0, 0.1); b <- rnorm(5, 0, 0.1); rho <- 0.05
n <- nrow(Z)
for (t in 1:3000) {
  E <- as.vector(Z %*% a)
  R <- Z - E %o% b                          # residuals
  gb <- -2 * colSums(R * E) / (n * 5)
  ga <- -2 * as.vector(t(Z) %*% (R %*% b)) / (n * 5)
  a <- a - rho * 5 * ga; b <- b - rho * 5 * gb
}
E <- as.vector(Z %*% a)
mse_ae <- mean((Z - E %o% b)^2)
cat("linear autoencoder reconstruction MSE:", round(mse_ae, 4),
    "; corr(code, PC1 score):", round(abs(cor(E, sc[, 1])), 4), "\n")
cat("encoder weights a (rescaled to unit length):", round(a / sqrt(sum(a^2)) * sign(sum(a)), 3), "\n")

## ---- C. PCA as image compression: a 120 x 160 picture of "SOC 723" ---------
tf <- tempfile(fileext = ".png")
png(tf, width = 160, height = 120)
par(mar = c(0, 0, 0, 0)); plot.new()
rect(0.03, 0.05, 0.97, 0.95, border = "black", lwd = 4)
text(0.5, 0.68, "SOC", cex = 3.6, font = 2); text(0.5, 0.3, "723", cex = 3.6, font = 2)
dev.off()
V <- png::readPNG(tf)[, , 1]                   # rows = 120 pixel rows, columns = 160 pixels
pv <- prcomp(V, center = TRUE, scale. = FALSE)
share <- cumsum(pv$sdev^2) / sum(pv$sdev^2)
Ks <- c(2, 5, 15)
cat("\nC. image", dim(V), "; cumulative variance K =", Ks, ":", round(share[Ks], 3), "\n")
recon <- function(K) sweep(pv$x[, 1:K, drop = FALSE] %*% t(pv$rotation[, 1:K, drop = FALSE]), 2, pv$center, "+")
pdf("figs/pca_image.pdf", width = 7.2, height = 1.75, family = "Times")
par(mfrow = c(1, 4), mar = c(0.3, 0.3, 1.5, 0.3))
g <- gray.colors(64, start = 0, end = 1)
show <- function(M, main) { image(t(pmin(pmax(M, 0), 1))[, nrow(M):1], col = g, axes = FALSE, main = main, cex.main = 1.1, font.main = 1); box() }
show(V, sprintf("original: %d numbers per row", ncol(V)))
for (K in Ks) show(recon(K), sprintf("K = %d (%.0f%% of variance)", K, 100 * share[K]))
dev.off()
for (K in Ks) cat("numbers stored, K =", K, ":", K * (nrow(V) + ncol(V)) + ncol(V), "of", prod(dim(V)), "\n")

## ---- D. Word vectors from a small co-occurrence table (hypothetical) ---------
C <- rbind(tesguino = c(4, 3, 1, 1), beer = c(5, 4, 0, 1), wine = c(4, 4, 0, 0),
           corn = c(1, 0, 4, 5), wheat = c(0, 0, 5, 4))
colnames(C) <- c("drink", "bottle", "field", "grow")
cosine <- function(x, y) sum(x * y) / sqrt(sum(x^2) * sum(y^2))
cat("\nD. cosine from raw counts: tesguino-beer", round(cosine(C["tesguino", ], C["beer", ]), 2),
    "; tesguino-corn", round(cosine(C["tesguino", ], C["corn", ]), 2),
    "; beer-wheat", round(cosine(C["beer", ], C["wheat", ]), 2), "\n")
pw <- prcomp(C, center = TRUE, scale. = FALSE)
W2 <- pw$x[, 1:2]; if (W2["beer", 1] < 0) W2[, 1] <- -W2[, 1]
cat("2-d word vectors (first two PCs):\n"); print(round(W2, 2))
cat("variance kept by 2 PCs:", round(sum(pw$sdev[1:2]^2) / sum(pw$sdev^2), 3), "\n")
cat("cosine in 2-d: tesguino-beer", round(cosine(W2["tesguino", ], W2["beer", ]), 2),
    "; tesguino-corn", round(cosine(W2["tesguino", ], W2["corn", ]), 2), "\n")

## ---- E. Skip-gram with negative sampling: one logistic term ------------------
dot <- 2.0
cat("\nE. sigma(2.0) =", round(plogis(dot), 3), "; sigma(-1.5) =", round(plogis(-1.5), 3), "\n")

## ---- F. A cultural dimension (Kozlowski et al. style), hypothetical vectors --
vec <- list(rich = c(2.0, 1.0, 0.5), poor = c(-1.6, 0.8, 0.4),
            golf = c(1.2, 0.3, 1.0), camping = c(-0.6, 0.2, 1.1))
dir <- vec$rich - vec$poor
cat("\nF. affluence direction rich - poor =", dir, "\n")
for (w in c("golf", "camping"))
  cat(w, ": cosine with direction =", round(cosine(vec[[w]], dir), 2), "\n")

## ---- G. Self-attention by hand, hypothetical 2-d vectors ---------------------
q_bank <- c(1, 1)
K <- rbind(the = c(0, 0), river = c(2, 0), loan = c(0, 2), bank = c(0.5, 0.5))
Vv <- rbind(the = c(0, 0), river = c(1, 0), loan = c(0, 1), bank = c(0.5, 0.5))
att <- function(words) {
  s <- as.vector(K[words, ] %*% q_bank) / sqrt(2)
  w <- exp(s) / sum(exp(s))
  out <- colSums(w * Vv[words, ])
  list(score = round(s, 2), exp = round(exp(s), 2), weight = round(w, 2), out = round(out, 2))
}
cat("\nG. 'the river bank':\n"); print(att(c("the", "river", "bank")))
cat("'the bank loan':\n"); print(att(c("the", "bank", "loan")))

## ---- H. PCA as an eigenvalue problem: the numbers for the derivation frames ----------
cat("\nH. two variables: eigenvalues of [[1, r], [r, 1]] = 1 + r, 1 - r:", round(1 + r, 3), round(1 - r, 3),
    "; eigen():", round(eigen(cor(z))$values, 3), "\n")
S5 <- cor(Z); ev <- eigen(S5)
c1 <- ev$vectors[, 1]; if (sum(c1) < 0) c1 <- -c1
cat("five status variables: eigenvalues", round(ev$values, 3), "; sum", round(sum(ev$values), 3), "\n")
cat("check S c1 = lambda1 c1:\n"); print(round(rbind(S_c1 = as.vector(S5 %*% c1), lambda1_c1 = ev$values[1] * c1), 3))
cat("first row of S times c1, worked:", paste(round(S5[1, ], 2), "x", round(c1, 3), collapse = " + "), "=",
    round(sum(S5[1, ] * c1), 3), "\n")
for (K in 1:2) cat("rebuild with K =", K, ": mean squared error per entry (n-1 denominator)",
                   round(sum(ev$values[-(1:K)]) / 5, 4), "= dropped eigenvalues / 5\n")
