## Week 4, multilevel: random-intercept logit on HSB and the predicted-probability
## figure for the "Random-Intercept Logit" frame. Run from the week folder.
suppressPackageStartupMessages(library(lme4))
data(MathAchieve, package = "nlme"); data(MathAchSchool, package = "nlme")
hsb <- merge(as.data.frame(MathAchieve), as.data.frame(MathAchSchool)[, c("School", "Sector")], by = "School")
hsb$School <- factor(as.character(hsb$School))
hsb$top <- as.integer(hsb$MathAch > quantile(hsb$MathAch, 0.75))
g1 <- glmer(top ~ SES + Sector + (1 | School), data = hsb, family = binomial, nAGQ = 10)
b <- fixef(g1); tau2 <- as.data.frame(VarCorr(g1))$vcov[1]; tau <- sqrt(tau2)
cat("glmer coefficients:\n"); print(round(summary(g1)$coefficients, 3)); cat("tau2:", round(tau2, 3), " tau:", round(tau, 3), "\n")
## public school (Catholic = 0), SES from -2 to 2
ses <- seq(-2, 2, by = 0.05)
eta <- b["(Intercept)"] + b["SES"] * ses
p_lo <- plogis(eta - tau); p_0 <- plogis(eta); p_hi <- plogis(eta + tau)
## population average: integrate plogis(eta + u) over u ~ N(0, tau2) on a fine grid
u <- seq(-4 * tau, 4 * tau, length.out = 401); w <- dnorm(u, 0, tau); w <- w / sum(w)
p_avg <- sapply(eta, function(e) sum(plogis(e + u) * w))
cat("at SES = 0: school u=0", round(plogis(b[1]), 3), " population average", round(p_avg[ses == 0], 3), "\n")
cat("slope at SES = 0 (per unit SES): school u=0", round(b["SES"] * plogis(b[1]) * (1 - plogis(b[1])), 3),
    " population average", round((p_avg[which(ses == 0) + 1] - p_avg[which(ses == 0) - 1]) / 0.1, 3), "\n")
dukeblue <- rgb(0, 83, 155, maxColorValue = 255); accent <- rgb(200, 78, 0, maxColorValue = 255)
pdf("figs/glmer_pred.pdf", width = 5.6, height = 2.9)
par(mar = c(3.6, 3.8, 0.8, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.85)
plot(ses, p_hi, type = "l", lwd = 1.5, col = dukeblue, lty = 2, ylim = c(0, 0.8),
     xlab = "student SES", ylab = "Pr(top quartile in math)")
lines(ses, p_0, lwd = 1.5, col = dukeblue); lines(ses, p_lo, lwd = 1.5, col = dukeblue, lty = 3)
lines(ses, p_avg, lwd = 3, col = accent)
legend("topleft", c(expression(paste("school with ", u[j], " = +", tau)), expression(paste("school with ", u[j], " = 0")),
                    expression(paste("school with ", u[j], " = -", tau)), "average over schools"),
       col = c(dukeblue, dukeblue, dukeblue, accent), lwd = c(1.5, 1.5, 1.5, 3), lty = c(2, 1, 3, 1), bty = "n", cex = 0.8)
dev.off()
