## =============================================================================
## w13_amazon.R -- price elasticity of demand for Amazon toys, with the product
## description's BERT embedding as a high-dimensional control (DML).
##   Rscript figs/w13_amazon.R > figs/w13_amazon.out
## Data: ../Data/amazon_toys.rds (Data/build_amazon_toys_bert.py, build_amazon_toys.R)
## Y = ln_q (log share of all reviews: the demand proxy), D = ln_p (log price),
## W = 768 numbers from BERT for "name | manufacturer | description".
## =============================================================================
suppressMessages({library(glmnet); library(ranger)})
d <- readRDS("../Data/amazon_toys.rds")
E <- as.matrix(d[, grep("^e[0-9]+$", names(d))])
Y <- d$ln_q; D <- d$ln_p; n <- nrow(d)
cat("products:", n, "; embedding columns:", ncol(E), "\n")
cat("top categories:\n"); print(head(sort(table(d$category), decreasing = TRUE), 8))
cat("sd log price", round(sd(D), 2), "; sd log demand", round(sd(Y), 2), "\n")
show <- function(lbl, b, se) cat(sprintf("%-50s %7.3f (%5.3f)\n", lbl, b, se))
robust <- function(fit, v) sqrt(sandwich::vcovHC(fit, "HC1")[v, v])

## ---- 1. naive regressions ------------------------------------------------------
cat("\n== 1. regressions ==\n")
f0 <- lm(Y ~ D); show("log demand on log price", coef(f0)[2], robust(f0, "D"))
cat_f <- factor(d$category)
f1 <- lm(Y ~ D + cat_f); show("+ top-level category dummies", coef(f1)[2], robust(f1, "D"))
sub_f <- factor(d$subcategory)
f2 <- lm(Y ~ D + sub_f); show(sprintf("+ subcategory dummies (%d)", nlevels(sub_f)), coef(f2)[2], robust(f2, "D"))
f3 <- lm(Y ~ D + E); show("+ all 768 embedding numbers, OLS", coef(f3)[2], robust(f3, "D"))

## ---- 2. PCA of the embeddings -------------------------------------------------
cat("\n== 2. principal components of the embeddings ==\n")
pc <- prcomp(E, center = TRUE, scale. = FALSE)
ve <- pc$sdev^2 / sum(pc$sdev^2)
cat("variance share of PC1-5:", round(ve[1:5], 3), "; first 10:", round(sum(ve[1:10]), 3),
    "; first 50:", round(sum(ve[1:50]), 3), "\n")
for (K in c(10, 50)) {
  fk <- lm(Y ~ D + pc$x[, 1:K]); show(sprintf("+ first %d principal components, OLS", K), coef(fk)[2], robust(fk, "D"))
}

## ---- 3. DML: cross-fitted predictions of Y and D from the embedding ------------
cat("\n== 3. DML, partially linear model, 5 folds ==\n")
dml <- function(learner, W, lbl, K = 5, seed = 723) {
  set.seed(seed); fold <- sample(rep(1:K, length.out = n))
  lY <- mD <- numeric(n)
  for (k in 1:K) {
    tr <- fold != k; te <- fold == k
    if (learner == "lasso") {
      lY[te] <- predict(cv.glmnet(W[tr, ], Y[tr]), W[te, ], s = "lambda.min")
      mD[te] <- predict(cv.glmnet(W[tr, ], D[tr]), W[te, ], s = "lambda.min")
    } else {
      dfW <- data.frame(W)
      lY[te] <- predict(ranger(y = Y[tr], x = dfW[tr, ], num.trees = 500, seed = seed), dfW[te, ])$predictions
      mD[te] <- predict(ranger(y = D[tr], x = dfW[tr, ], num.trees = 500, seed = seed), dfW[te, ])$predictions
    }
  }
  rY <- Y - lY; rD <- D - mD
  b <- sum(rD * rY) / sum(rD^2)
  psi <- (rY - b * rD) * rD; se <- sqrt(mean(psi^2) / mean(rD^2)^2 / n)
  r2 <- c(Y = 1 - mean(rY^2) / var(Y), D = 1 - mean(rD^2) / var(D))
  show(sprintf("DML, %s on %s", learner, lbl), b, se)
  cat(sprintf("%50s R2 of l-hat(W) %.3f, of m-hat(W) %.3f\n", "", r2["Y"], r2["D"]))
  invisible(list(b = b, se = se, rY = rY, rD = rD))
}
r_lasso <- dml("lasso", E, "768 embedding numbers")
r_rf    <- dml("forest", pc$x[, 1:50], "50 principal components")
r_lpc   <- dml("lasso", pc$x[, 1:50], "50 principal components")
## categories + embedding together
Wc <- cbind(model.matrix(~ cat_f)[, -1], E)
r_both  <- dml("lasso", Wc, "category dummies + embedding")

## ---- 4. heterogeneity: elasticity by top category, from the DML residuals ------
cat("\n== 4. elasticity by category (lasso DML residuals) ==\n")
top <- names(sort(table(d$category), decreasing = TRUE))[1:6]
het <- t(sapply(top, function(cg) {
  i <- d$category == cg; rY <- r_lasso$rY[i]; rD <- r_lasso$rD[i]
  b <- sum(rD * rY) / sum(rD^2); psi <- (rY - b * rD) * rD
  c(n = sum(i), est = round(b, 3), se = round(sqrt(mean(psi^2) / mean(rD^2)^2 / sum(i)), 3))
}))
print(het)

## ---- 5. figures ----------------------------------------------------------------
navy <- rgb(1, 33, 105, maxColorValue = 255)
pdf("figs/amazon_scatter.pdf", width = 6.4, height = 3.6)
par(mar = c(4.2, 4.4, 1, 1), bty = "l", las = 1, cex.axis = .9)
plot(D, Y, pch = 16, cex = .35, col = adjustcolor(navy, .25),
     xlab = "log price (pounds)", ylab = "log share of reviews")
abline(f0, col = "firebrick", lwd = 2.5)
dev.off()
pdf("figs/amazon_pca.pdf", width = 6.4, height = 3.8)
par(mar = c(4.2, 4.4, 1, 1), bty = "l", las = 1, cex.axis = .9)
show_cat <- top[1:4]; cols <- c("firebrick", navy, "darkgreen", "darkorange")
i <- d$category %in% show_cat
plot(pc$x[i, 1], pc$x[i, 2], pch = 16, cex = .45, col = adjustcolor(cols[match(d$category[i], show_cat)], .5),
     xlab = "first principal component of the BERT vectors", ylab = "second principal component")
legend("topright", show_cat, col = cols, pch = 16, bty = "n", cex = .8)
dev.off()
cat("\nfigures written\n")
