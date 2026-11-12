## Week 12 -- language models by hand: a tiny word2vec (skip-gram) trained by stochastic gradient descent,
## and next-word prediction with a softmax (the last step of a GPT-style model). Hypothetical text.
## Every number on 'Word2Vec by Hand' and 'Generating Text by Hand' comes from here.
##   Rscript figs/w12_lang.R > figs/w12_lang.out   (run from the week folder)
## writes figs/w2v_toy.pdf, figs/w2v_loss.pdf

## ---- A. skip-gram on six three-word sentences, 2 numbers per word ------------------
sents <- list(c("drink", "beer", "cold"), c("drink", "wine", "cold"), c("drink", "tesguino", "cold"),
              c("drink", "wine", "red"), c("grow", "corn", "field"), c("grow", "wheat", "field"),
              c("grow", "corn", "yellow"))
vocab <- sort(unique(unlist(sents))); V <- length(vocab); id <- setNames(seq_len(V), vocab)
## training pairs: each word and every word next to it (window 1)
pairs <- do.call(rbind, lapply(sents, function(s) do.call(rbind, lapply(seq_along(s), function(i) {
  nb <- s[c(i - 1, i + 1)[c(i - 1, i + 1) >= 1 & c(i - 1, i + 1) <= length(s)]]
  cbind(w = id[s[i]], c = id[nb]) }))))
cat("A. vocabulary:", vocab, "; V =", V, "; training pairs:", nrow(pairs), "\n")
cat("pairs for tesguino:", vocab[pairs[pairs[, "w"] == id["tesguino"], "c"]], "\n")
K <- 2
set.seed(723)
U <- matrix(rnorm(V * K, 0, 0.5), V, K, dimnames = list(vocab, NULL))   # word vectors (input weights)
W <- matrix(rnorm(V * K, 0, 0.5), V, K, dimnames = list(vocab, NULL))   # context vectors (output weights)
U_start <- U; W_start <- W
softmax <- function(z) exp(z - max(z)) / sum(exp(z - max(z)))
## loss for one pair: 1/2 sum over the 11 words of (y_c - p_c)^2, y = 1 for the observed context word
sqloss <- function(p, c) { y <- numeric(V); y[c] <- 1; 0.5 * sum((y - p)^2) }
loss <- function() mean(apply(pairs, 1, function(r) sqloss(softmax(as.vector(W %*% U[r[1], ])), r[2])))
cat("loss before training (average over 28 pairs):", round(loss(), 3), "\n")
rho <- 0.5; lam <- 0.001                     # learning rate; weight decay
## one stochastic-gradient step on ONE pair (w, c)
step <- function(w, c, show = FALSE) {
  u <- U[w, ]; sc <- as.vector(W %*% u); p <- softmax(sc)
  y <- numeric(V); y[c] <- 1
  d <- p - y                                 # prediction minus observed, word by word
  gs <- p * (d - sum(d * p))                 # slope of the loss for each score (chain rule through the softmax)
  gu <- as.vector(t(W) %*% gs)               # slope for the word's vector u_w
  if (show) {
    cat(sprintf("\nA1. one step on the pair (%s, %s), learning rate %.1f\n", vocab[w], vocab[c], rho))
    cat("u_w =", round(u, 2), "\n")
    for (cc in c("drink", "cold", "corn")) cat(sprintf("  %-6s score = %5.2f  prob = %.3f  y = %d  1/2(y - p)^2 = %.3f\n",
      cc, sc[id[cc]], p[id[cc]], y[id[cc]], 0.5 * (y[id[cc]] - p[id[cc]])^2))
    cat("loss for the pair (sum over 11 words):", round(sqloss(p, c), 3), "\n")
  }
  U[w, ] <<- u - rho * (gu + lam * u)
  W <<- W - rho * (outer(gs, u) + lam * W)
  if (show) { p2 <- softmax(as.vector(W %*% U[w, ]))
    cat("new u_w =", round(U[w, ], 2), "; new prob", round(p2[c], 3), "; new loss", round(sqloss(p2, c), 3), "\n") }
}
step(id["tesguino"], id["drink"], show = TRUE)
U <- U_start; W <- W_start                   # undo the demonstration step
## training: 300 epochs; each epoch = the 28 pairs in a new random order, one step per pair
L <- loss()
for (ep in 1:300) { for (r in sample(nrow(pairs))) step(pairs[r, 1], pairs[r, 2]); L <- c(L, loss()) }
cat("\naverage loss over the 28 pairs after epoch 0, 1, 10, 50, 300:", round(L[c(1, 2, 11, 51, 301)], 3), "\n")
## the lowest possible average loss: each word's neighbours predicted at their observed shares
Lmin <- mean(apply(pairs, 1, function(r) { cw <- pairs[pairs[, 1] == r[1], 2]
  ph <- tabulate(cw, V) / length(cw); sqloss(ph, r[2]) }))
cat("lowest possible average loss:", round(Lmin, 3), "\n")
pdf("figs/w2v_loss.pdf", width = 4.2, height = 2.7, family = "Times")
par(mar = c(3.2, 3.6, 0.6, 0.8), mgp = c(2, 0.55, 0))
plot(0:300, L, type = "l", lwd = 2, col = "navy", las = 1, xlab = "epoch (28 one-pair steps each)",
     ylab = "average loss, 28 pairs", ylim = c(0, max(L) * 1.05))
abline(h = Lmin, col = "gray60", lty = 3); text(250, Lmin - 0.025, "lowest possible", col = "gray45", cex = 0.85)
dev.off()
cosine <- function(x, y) sum(x * y) / sqrt(sum(x^2) * sum(y^2))
cat("\nlearned word vectors u_w:\n"); print(round(U, 2))
for (pr in list(c("tesguino", "beer"), c("tesguino", "wine"), c("beer", "wine"), c("tesguino", "corn"), c("beer", "corn"), c("corn", "wheat")))
  cat("cosine", pr[1], "-", pr[2], round(cosine(U[pr[1], ], U[pr[2], ]), 2), "\n")
p_t <- softmax(as.vector(W %*% U["tesguino", ])); names(p_t) <- vocab
cat("Pr(neighbour | tesguino):\n"); print(round(sort(p_t, decreasing = TRUE), 2))

pdf("figs/w2v_toy.pdf", width = 3.6, height = 3.2, family = "Times")
par(mar = c(3.2, 3.4, 0.6, 0.6), mgp = c(2, 0.55, 0))
lim <- c(-2.6, 2.6)
plot(U, type = "n", xlim = lim, ylim = lim, las = 1, xlab = "first number", ylab = "second number", asp = 1)
abline(h = 0, v = 0, col = "gray88")
col <- setNames(ifelse(vocab %in% c("beer", "wine", "tesguino"), "firebrick",
              ifelse(vocab %in% c("corn", "wheat"), "darkgreen", "gray45")), vocab)
points(U, pch = 19, col = col)
## words within about 0.6 of each other share one label
grp <- cutree(hclust(dist(U)), h = 0.6)
for (g in unique(grp)) { w <- vocab[grp == g]; m <- colMeans(U[w, , drop = FALSE])
  text(m[1], m[2], paste(w, collapse = ", "), pos = if (m[2] > 0) 3 else 1, cex = 0.85, col = col[w[1]]) }
dev.off()

## ---- B. next-word prediction: scores -> softmax -> sample ----------------------------
cat("\nB. context: 'The survey asked parents about their' (hypothetical scores)\n")
sc <- c(children = 2.0, income = 1.5, school = 1.0, banana = -3.0)
for (Tt in c(1, 0.5, 2)) {
  p <- exp(sc / Tt) / sum(exp(sc / Tt))
  cat(sprintf("temperature %.1f: exp(score/T) %s; prob %s\n", Tt, paste(sprintf("%.2f", exp(sc / Tt)), collapse = " "),
              paste(sprintf("%.3f", p), collapse = " ")))
}
p1 <- exp(sc) / sum(exp(sc))
cat("loss if the true next word is 'income': -log", round(p1["income"], 3), "=", round(-log(p1["income"]), 3), "\n")

## ---- A2. reading the trained word2vec: scores and probabilities for tesguino -----------
cat("\nA2. output (context) vectors v_c:\n"); print(round(W[c("drink", "cold", "corn", "beer", "tesguino", "wine"), ], 2))
cat("cosine of OUTPUT vectors: beer-tesguino", round(cosine(W["beer", ], W["tesguino", ]), 2), "; beer-wine", round(cosine(W["beer", ], W["wine", ]), 2), "; beer-corn", round(cosine(W["beer", ], W["corn", ]), 2), "\n")
for (cw in c("drink", "cold", "corn")) cat(sprintf("score(tesguino, %s) = %.2f x %.2f + %.2f x %.2f = %.2f\n", cw,
  U["tesguino", 1], W[cw, 1], U["tesguino", 2], W[cw, 2], sum(U["tesguino", ] * W[cw, ])))
sc_t <- as.vector(W %*% U["tesguino", ]); names(sc_t) <- vocab
cat("sum over all 11 words of exp(score):", round(sum(exp(sc_t)), 2), "; Pr(drink | tesguino) =",
    round(exp(sc_t["drink"]) / sum(exp(sc_t)), 3), "\n")
## at the start: small random weights, every probability near 1/11
set.seed(723)
U0 <- U_start; W0 <- W_start
p0 <- softmax(as.vector(W0 %*% U0["tesguino", ])); names(p0) <- vocab
cat("start: u_tesguino =", round(U0["tesguino", ], 3), "; Pr(drink | tesguino) =", round(p0["drink"], 3),
    "(1/11 =", round(1 / 11, 3), "); -log =", round(-log(p0["drink"]), 3), "\n")
## one gradient step for the single pair (tesguino, drink): predicted minus observed
e <- p0; e["drink"] <- e["drink"] - 1
cat("one pair (tesguino, drink): predicted minus observed for drink", round(e["drink"], 3),
    "; for cold", round(e["cold"], 3), "\n")

## ---- C. attention, simplest form: compare with own word vectors, normalize, average -----
cat("\nC. simple attention with the word vectors themselves (hypothetical 2-number vectors)\n")
wv <- rbind(the = c(0, 0), river = c(2, 0), loan = c(0, 2), bank = c(0.5, 0.5))
simple <- function(words, who = "bank") {
  s <- as.vector(wv[words, ] %*% wv[who, ]); w <- exp(s) / sum(exp(s))
  cat(paste(words, collapse = " "), ": scores", round(s, 2), "; exp", round(exp(s), 2), "; weights", round(w, 2),
      "; new bank", round(colSums(w * wv[words, ]), 2), "\n")
}
simple(c("the", "river", "bank")); simple(c("the", "bank", "loan"))

## ---- D. BERT's training task: fill the blank (hypothetical scores) ---------------------
cat("\nD. 'The survey was [MASK] in 2022'\n")
sb <- c(conducted = 2.2, fielded = 1.8, cancelled = 0.5, eaten = -2.0)
pb <- exp(sb) / sum(exp(sb)); print(round(rbind(score = sb, exp = exp(sb), prob = pb), 3))
cat("sum exp", round(sum(exp(sb)), 2), "; loss if the hidden word was 'fielded': -log", round(pb["fielded"], 3), "=",
    round(-log(pb["fielded"]), 3), "\n")

## ---- E. GPT's training task: every word is a test question (hypothetical probabilities) --
cat("\nE. 'the survey asked parents about their children'\n")
toks <- c("the", "survey", "asked", "parents", "about", "their", "children")
pt <- c(0.01, 0.04, 0.08, 0.35, 0.60, 0.50)   # model's probability for the TRUE next word
for (i in 1:6) cat(sprintf("  context '%s' -> true next '%s': prob %.2f, -log %.2f\n",
                           paste(toks[1:i], collapse = " "), toks[i + 1], pt[i], -log(pt[i])))
cat("average loss", round(mean(-log(pt)), 3), "\n")
