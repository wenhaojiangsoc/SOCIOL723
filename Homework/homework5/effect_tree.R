## effect_tree(): the tree for effects from the Week 13 lecture.
## Yt, Dt = residuals of the outcome and the treatment (after predicting each from X).
## A leaf's effect is the OLS slope of Yt on Dt among the people in it.
## Every cut of every covariate is tried; the cut kept is the one with the largest
##   Delta = n_L n_R / n x (slope_L - slope_R)^2.
## Returns an rpart object: draw it with rpart.plot(tree, type = 2, digits = 2).
effect_tree <- function(Yt, Dt, X, depth = 2, min_leaf = 1000) {
  slope <- function(y) sum(y[, 1] * y[, 2]) / sum(y[, 2]^2)
  method <- list(
    init = function(y, offset, parms, wt) list(
      y = y, parms = parms, numresp = 1, numy = 2,
      summary = function(yval, dev, wt, ylevel, digits)
        paste("effect", signif(yval, digits))),
    ## the number shown in a node: its slope
    eval = function(y, wt, parms) {
      b <- slope(y)
      list(label = b, deviance = sum((y[, 1] - b * y[, 2])^2))
    },
    ## all cuts of one covariate (rows arrive sorted by it)
    split = function(y, wt, x, parms, continuous) {
      n <- nrow(y); nL <- 1:(n - 1); nR <- n - nL
      sL <- cumsum(y[, 1] * y[, 2])[nL]; dL <- cumsum(y[, 2]^2)[nL]
      tL <- sL / dL
      tR <- (sum(y[, 1] * y[, 2]) - sL) / (sum(y[, 2]^2) - dL)
      Delta <- nL * nR / n * (tL - tR)^2
      Delta[nL < parms$min_leaf | nR < parms$min_leaf] <- 0
      list(goodness = Delta, direction = rep(-1, n - 1))
    })
  rpart::rpart(cbind(Yt, Dt) ~ ., data = data.frame(Yt, Dt, X),
               method = method, parms = list(min_leaf = min_leaf),
               control = rpart::rpart.control(
                 maxdepth = depth, cp = -1, xval = 0,
                 maxcompete = 0, maxsurrogate = 0))
}
