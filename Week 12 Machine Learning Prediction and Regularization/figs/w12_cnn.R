## Week 12 -- a convolution by hand (toy 6 x 6 image), and parameter counts for an image input.
## Every number on the convolutional-network frames comes from here.
##   Rscript figs/w12_cnn.R > figs/w12_cnn.out   (run from the week folder)

## ---- A. toy grayscale image: dark left half (0), bright right half (9) -----------------
img <- matrix(c(0, 0, 0, 9, 9, 9), 6, 6, byrow = TRUE)
cat("A. image (6 x 6 pixels):\n"); print(img)
## a 3 x 3 filter that responds to a dark-to-bright vertical edge
filt <- matrix(c(-1, 0, 1), 3, 3, byrow = TRUE)
cat("filter:\n"); print(filt)
## slide it over every 3 x 3 window: weighted sum of the 9 pixels, then ReLU
conv <- matrix(NA, 4, 4)
for (r in 1:4) for (c in 1:4) conv[r, c] <- max(0, sum(img[r:(r + 2), c:(c + 2)] * filt))
cat("feature map (4 x 4), after ReLU:\n"); print(conv)
cat("window at row 1, column 2: pixels", img[1, 2:4], "; sum = 3 x (-1 x 0 + 0 x 0 + 1 x 9) =", sum(img[1:3, 2:4] * filt), "\n")
cat("window at row 1, column 1: all dark ->", sum(img[1:3, 1:3] * filt), "\n")

## ---- B. why share weights: parameter counts for a 224 x 224 colour photo ----------------
inputs <- 224 * 224 * 3
cat("\nB. inputs in a 224 x 224 colour photo:", inputs, "\n")
cat("one ordinary hidden layer of 64 units:", format(inputs * 64 + 64, big.mark = ","), "weights\n")
cat("one convolutional layer of 64 filters, 3 x 3 x 3 each:", 64 * (3 * 3 * 3 + 1), "weights\n")
