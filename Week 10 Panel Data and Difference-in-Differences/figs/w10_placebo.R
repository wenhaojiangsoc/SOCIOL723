## Week 10 -- Bertrand, Duflo, and Mullainathan's experiment on our data: placebo laws.
## Real outcome (female suicides per million, 51 states, 1964-1996, bacondecomp::divorce);
## a FAKE law is given to 25 random states from a random year 1970-1990. True effect = 0.
##   Rscript figs/w10_placebo.R > figs/w10_placebo.out
set.seed(723)
lib <- Sys.getenv("R_LIBS_SCRATCH", unset = file.path(tempdir(), "Rlib"))
dir.create(lib, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
suppressMessages(library(fixest))
data(divorce, package = "bacondecomp")
d <- subset(divorce, sex == 2)[, c("st", "year", "suiciderate_jag")]
d$y <- d$suiciderate_jag * 1e6
sts <- unique(d$st); R <- 1000
ac <- feols(y ~ 1 | st + year, d); r <- resid(ac)
lag1 <- sapply(split(data.frame(r = r, st = d$st), d$st), function(z) cor(z$r[-1], z$r[-nrow(z)]))
cat("mean within-state autocorrelation of TWFE residuals (lag 1):", round(mean(lag1), 2), "\n")
out <- t(replicate(R, {
  tr <- sample(sts, 25); g <- sample(1970:1990, 1)
  d$P <- as.integer(d$st %in% tr & d$year >= g)
  f_iid <- feols(y ~ P | st + year, d, vcov = "iid")
  f_cl  <- feols(y ~ P | st + year, d, cluster = ~st)
  c(iid = pvalue(f_iid)[[1]] < 0.05, cl = pvalue(f_cl)[[1]] < 0.05)
}))
cat("share of", R, "placebo laws 'significant' at 5%: iid SEs", round(mean(out[, "iid"]), 3),
    "; clustered by state", round(mean(out[, "cl"]), 3), "\n")
## few treated clusters: only 3 states get the fake law
out3 <- t(replicate(R, {
  tr <- sample(sts, 3); g <- sample(1970:1990, 1)
  d$P <- as.integer(d$st %in% tr & d$year >= g)
  f_cl <- feols(y ~ P | st + year, d, cluster = ~st)
  c(cl = pvalue(f_cl)[[1]] < 0.05)
}))
cat("3 treated states, clustered SEs: rejection rate", round(mean(out3), 3), "\n")
