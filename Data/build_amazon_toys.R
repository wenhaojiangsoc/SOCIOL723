## =============================================================================
## build_amazon_toys.R -- Data/amazon_toys_bert.csv.gz -> Data/amazon_toys.rds
## The csv.gz is written by Data/build_amazon_toys_bert.py (BERT embeddings).
## Output: one row per product: ln_p (log price), ln_q (log share of reviews,
## the demand proxy), category, subcategory path, the text (first 300
## characters), and the 768 embedding columns e1 ... e768.
## =============================================================================
d <- read.csv(gzfile("Data/amazon_toys_bert.csv.gz"), stringsAsFactors = FALSE)
d$text <- substr(d$text, 1, 300)
names(d)[names(d) == "amazon_category_and_sub_category"] <- "subcategory"
d$uniq_id <- NULL
saveRDS(d, "Data/amazon_toys.rds", compress = "xz")
cat("rows", nrow(d), "; columns", ncol(d), "\n")
