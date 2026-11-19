## Week 13: attention from word vectors to a prediction, on a three-word toy.
## All weights are hypothetical and set by hand; every number on the slides is printed here.
sink("w13_attention.out", split = TRUE)

## ---- A. hand-set weights -------------------------------------------------
## starting vector = row of the word table + row of the position table
E <- rbind(the = c(0.1, 0.1), river = c(0.9, -0.1), loan = c(-0.1, 0.9), bank = c(0.2, 0.2),
           MASK = c(0.2, 0.2))   # [MASK]'s row is set equal to bank's
P <- rbind(slot1 = c(-0.1, -0.1), slot2 = c(0.1, 0.1), slot3 = c(0.05, 0.05))
slot <- c(the = 1, river = 2, loan = 2, bank = 3, MASK = 3)      # where each word stands in the toy
h <- E + P[slot, ]
cat("word table:\n"); print(E); cat("position table:\n"); print(P)
cat("starting vectors h:\n"); print(h)
WQ <- matrix(2, 2, 2)            # query weights
WK <- diag(2, 2)                 # key weights
WV <- diag(1, 2)                 # value weights
U  <- rbind(flooded = c(3, 0), defaulted = c(0, 3), the = c(0, 0))  # output vectors

attend <- function(words, h, WQ, WK, WV, U) {
  H <- h[words, , drop = FALSE]
  Q <- H %*% t(WQ); K <- H %*% t(WK); V <- H %*% t(WV)
  i <- length(words)                      # the [MASK] slot is last
  score  <- as.vector(K %*% Q[i, ]) / sqrt(2)
  weight <- exp(score) / sum(exp(score))
  a   <- as.vector(weight %*% V)          # weighted average of the values
  new <- H[i, ] + a                       # add the word's own vector back
  s   <- as.vector(U %*% new)
  p   <- exp(s) / sum(exp(s))
  list(Q = Q, K = K, V = V, score = score, expscore = exp(score), weight = weight,
       a = a, new = new, vocab_score = setNames(s, rownames(U)),
       p = setNames(p, rownames(U)))
}
for (sent in list(c("the", "river", "bank"), c("the", "loan", "bank"),
                  c("the", "river", "MASK"), c("the", "loan", "MASK"))) {
  cat("\n== sentence:", sent, "==\n")
  print(lapply(attend(sent, h, WQ, WK, WV, U), round, 3))
}
r <- attend(c("the", "river", "MASK"), h, WQ, WK, WV, U)
cat("loss, -log P(flooded):", round(-log(r$p["flooded"]), 3), "\n")

## ---- B. all three words at once, as matrix products ----------------------
H <- h[c("the", "river", "MASK"), ]
Q <- H %*% t(WQ); K <- H %*% t(WK); V <- H %*% t(WV)
S <- Q %*% t(K) / sqrt(2)                      # row i: query of word i meets every key
A <- exp(S) / rowSums(exp(S))                  # softmax within each row
cat("\n== matrix form ==\nscores S = Q K' / sqrt(2):\n"); print(round(S, 2))
cat("attention weights A (rows sum to 1):\n"); print(round(A, 2))
cat("A V:\n"); print(round(A %*% V, 2))
cat("new H = H + A V:\n"); print(round(H + A %*% V, 2))

sink()
