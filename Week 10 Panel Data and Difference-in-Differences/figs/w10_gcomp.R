## Week 10 -- g-computation (regression adjustment) in a DiD with a continuous covariate. Hypothetical.
## 60 PA stores and 60 NJ stores; X = store size in February (workers). NJ stores are larger.
## Untreated change in employment falls with size: m0(x) = 2 - 0.2 x + noise. The law adds +2 to every NJ store.
## Plain DiD mixes the size difference into the effect; g-computation compares each NJ store with the
## untreated change predicted for a PA store of its size.
##   Rscript figs/w10_gcomp.R > figs/w10_gcomp.out   (from the week folder; writes figs/gcomp_picture.pdf)
set.seed(723)
d <- data.frame(nj = rep(c(0, 1), each = 60))
d$size <- ifelse(d$nj == 1, runif(120, 15, 35), runif(120, 5, 25))
d$dy <- 2 - 0.2 * d$size + 2 * d$nj + rnorm(120, 0, 0.8)    ## change in employment, Feb -> Nov

## plain DiD: NJ's mean change minus PA's mean change
plain <- mean(d$dy[d$nj == 1]) - mean(d$dy[d$nj == 0])
## g-computation, four steps
m0 <- lm(dy ~ size, data = subset(d, nj == 0))           ## 1-2. untreated change, learned from PA only
d$dy0_hat <- predict(m0, newdata = d)                      ## 3. predicted untreated change, every store
att <- with(subset(d, nj == 1), mean(dy - dy0_hat))        ## 4. average (actual - predicted) over NJ
cat("mean size PA", round(mean(d$size[d$nj == 0]), 1), "NJ", round(mean(d$size[d$nj == 1]), 1), "\n")
cat("PA line: intercept", round(coef(m0)[1], 2), "slope", round(coef(m0)[2], 3), "\n")
cat("plain DiD", round(plain, 2), "; g-computation ATT", round(att, 2), "; truth 2\n")
cat("NJ actual mean change", round(mean(d$dy[d$nj == 1]), 2), "; NJ predicted untreated change",
    round(mean(d$dy0_hat[d$nj == 1]), 2), "\n")

pdf("figs/gcomp_picture.pdf", width = 6.4, height = 4.4, family = "Times")
par(mar = c(4, 4.4, 0.6, 0.6), cex.lab = 1.2, cex.axis = 1.1)
plot(d$size, d$dy, type = "n", xlab = "store size in February (workers)",
     ylab = "change in employment, Feb to Nov", ylim = c(-6, 4.5))
## shade sizes with no PA store: there the line extrapolates
xmax <- max(d$size[d$nj == 0])
rect(xmax, -7, 40, 5, col = "gray92", border = NA)
text(xmax + 0.3, -5.6, "no PA stores this large:\nthe line extrapolates", pos = 4, cex = 0.85, col = "gray30")
cat("largest PA store", round(xmax, 1), "; NJ stores larger than that:", sum(d$size[d$nj == 1] > xmax), "of 60\n")
nj <- d[d$nj == 1, ]
segments(nj$size, nj$dy0_hat, nj$size, nj$dy, col = adjustcolor("firebrick", 0.45), lwd = 1.2)
points(d$size[d$nj == 0], d$dy[d$nj == 0], pch = 1, col = "navy", cex = 0.9)
points(nj$size, nj$dy, pch = 19, col = "firebrick", cex = 0.9)
abline(m0, lwd = 2.5, col = "navy")
legend("topright", c("PA store (no law)", "NJ store (law)", "PA line: untreated change by size",
                     "NJ store minus the line: its effect"),
       pch = c(1, 19, NA, NA), lty = c(NA, NA, 1, 1), lwd = c(NA, NA, 2.5, 1.2),
       col = c("navy", "firebrick", "navy", adjustcolor("firebrick", 0.6)), bty = "n", cex = 0.9)
dev.off()
## the usual fix: add size to the regression. Size never changes, so it cannot move the estimate.
suppressMessages(library(fixest)); setFixest_notes(FALSE)
d$store <- seq_len(nrow(d))
d$y_pre <- 15 + 0.5 * d$size                                 ## any February level works
long <- rbind(data.frame(store = d$store, nj = d$nj, size = d$size, post = 0, y = d$y_pre),
              data.frame(store = d$store, nj = d$nj, size = d$size, post = 1, y = d$y_pre + d$dy))
long$D <- long$nj * long$post
cat("treated x post regression:", round(coef(lm(y ~ nj * post, long))[["nj:post"]], 2),
    "; plus size:", round(coef(lm(y ~ nj * post + size, long))[["nj:post"]], 2), "\n")
cat("TWFE:", round(coef(feols(y ~ D | store + post, long))[["D"]], 2),
    "; plus size (absorbed by store effects):", round(coef(feols(y ~ D + size | store + post, long))[["D"]], 2), "\n")
## the repair: let size shift the trend (size x post), i.e. regress the change on nj and size
cat("TWFE + size x post:", round(coef(feols(y ~ D + size:post | store + post, long))[["D"]], 2),
    "; same as lm(dy ~ nj + size):", round(coef(lm(dy ~ nj + size, d))[["nj"]], 2), "\n")
