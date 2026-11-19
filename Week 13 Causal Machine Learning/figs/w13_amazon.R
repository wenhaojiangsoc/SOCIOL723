## =============================================================================
## w13_amazon.R -- price elasticity of demand for Amazon toys, with the product
## description's BERT [CLS] vector as a high-dimensional control: OLS vs DML.
##   Rscript figs/w13_amazon.R > figs/w13_amazon.out      (run from the week folder)
## Data: ../Data/amazon_toys.rds (Data/build_amazon_toys_bert.py, build_amazon_toys.R)
## Y = ln_q (log share of all reviews: the demand proxy), D = ln_p (log price),
## W = 768 numbers: BERT's last-layer [CLS] vector for "name | manufacturer | description".
## A random sample of 2,000 products: 768 controls for 2,000 rows.
## =============================================================================
suppressMessages({library(glmnet); library(sandwich)})
full <- readRDS("../Data/amazon_toys.rds")
set.seed(723)
d <- full[sample(nrow(full), 2000), ]
E <- as.matrix(d[, grep("^e[0-9]+$", names(d))])
Y <- d$ln_q; D <- d$ln_p; n <- nrow(d)
cat("products in the file:", nrow(full), "; sampled:", n, "; embedding columns:", ncol(E), "\n")
cat("sd log price", round(sd(D), 2), "; sd log demand", round(sd(Y), 2), "\n")
cat("top categories:\n"); print(head(sort(table(d$category), decreasing = TRUE), 6))
show <- function(lbl, b, se) cat(sprintf("%-46s %7.3f (%5.3f)\n", lbl, b, se))
r2 <- function(y, yhat) 1 - mean((y - yhat)^2) / mean((y - mean(y))^2)

## ---- 1. OLS ------------------------------------------------------------------
cat("\n== 1. OLS ==\n")
f0 <- lm(Y ~ D);     show("no controls", coef(f0)[2], sqrt(vcovHC(f0, "HC1")[2, 2]))
f1 <- lm(Y ~ D + E); show("768 BERT numbers as controls", coef(f1)[2], sqrt(vcovHC(f1, "HC1")[2, 2]))
cat("in-sample R2 of the 768 numbers: price", round(summary(lm(D ~ E))$r.squared, 3),
    "; demand", round(summary(lm(Y ~ E))$r.squared, 3), "\n")

## ---- 2. held-out predictions: OLS vs lasso, 5 folds -----------------------------
cat("\n== 2. out-of-fold predictions from the 768 numbers ==\n")
set.seed(723); fold <- sample(rep(1:5, length.out = n))
pred <- list(ols = list(Y = numeric(n), D = numeric(n)), lasso = list(Y = numeric(n), D = numeric(n)))
for (k in 1:5) {
  tr <- fold != k; te <- fold == k
  for (v in c("Y", "D")) {
    y <- get(v)
    b <- coef(lm(y[tr] ~ E[tr, ])); b[is.na(b)] <- 0
    pred$ols[[v]][te]   <- cbind(1, E[te, ]) %*% b
    pred$lasso[[v]][te] <- predict(cv.glmnet(E[tr, ], y[tr]), E[te, ], s = "lambda.min")
  }
}
for (l in c("ols", "lasso"))
  cat(sprintf("%-6s out-of-fold R2: price %6.3f ; demand %6.3f\n", l, r2(D, pred[[l]]$D), r2(Y, pred[[l]]$Y)))

## ---- 3. DML: residual on residual, from the out-of-fold predictions -------------
cat("\n== 3. DML, partially linear model, 5 folds ==\n")
for (l in c("ols", "lasso")) {
  rY <- Y - pred[[l]]$Y; rD <- D - pred[[l]]$D
  b <- sum(rD * rY) / sum(rD^2); psi <- (rY - b * rD) * rD
  show(sprintf("DML, %s predictions", l), b, sqrt(mean(psi^2) / mean(rD^2)^2 / n))
}

## ---- 4. PCA of the embeddings ---------------------------------------------------
cat("\n== 4. principal components of the 768 numbers ==\n")
pc <- prcomp(E, center = TRUE, scale. = FALSE)
ve <- pc$sdev^2 / sum(pc$sdev^2)
cat("variance share: first 10", round(sum(ve[1:10]), 3), "; first 50", round(sum(ve[1:50]), 3), "\n")

## ---- 5. figures -----------------------------------------------------------------
navy <- rgb(1, 33, 105, maxColorValue = 255)
pdf("figs/amazon_scatter.pdf", width = 6.4, height = 3.6)
par(mar = c(4.2, 4.4, 1, 1), bty = "l", las = 1, cex.axis = .9)
plot(D, Y, pch = 16, cex = .45, col = adjustcolor(navy, .3),
     xlab = "log price (pounds)", ylab = "log share of reviews")
abline(f0, col = "firebrick", lwd = 2.5)
dev.off()
pdf("figs/amazon_pca.pdf", width = 6.4, height = 3.8)
par(mar = c(4.2, 4.4, 1, 1), bty = "l", las = 1, cex.axis = .9)
top <- names(sort(table(d$category), decreasing = TRUE))[1:4]
cols <- c("firebrick", navy, "darkgreen", "darkorange")
i <- d$category %in% top
plot(pc$x[i, 1], pc$x[i, 2], pch = 16, cex = .55, col = adjustcolor(cols[match(d$category[i], top)], .55),
     xlab = "first principal component of the BERT vectors", ylab = "second principal component")
legend("topright", top, col = cols, pch = 16, bty = "n", cex = .8)
dev.off()
cat("\nfigures written\n")
