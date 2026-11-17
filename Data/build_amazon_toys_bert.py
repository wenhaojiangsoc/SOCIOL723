"""
build_amazon_toys_bert.py -- BERT embeddings of Amazon toy product descriptions.

Data: 10,000 Amazon UK toy listings (PromptCloud via Kaggle, CC BY-SA 4.0), as posted
in the CausalAIBook MetricsMLNotebooks repository (data/amazon_toys.csv). Cleaning
follows that repository's notebook "DoubleML_and_Feature_Engineering_with_BERT":
  price -> log price (ln_p); number of reviews -> log market share (ln_q), the demand proxy;
  text = product name | manufacturer | product description.

Embedding: bert-base-uncased, text truncated at 128 tokens, mean of the last-layer token
vectors over non-padding tokens (768 numbers per product). No fine-tuning.

Run once (needs torch + transformers):
  python Data/build_amazon_toys_bert.py Data/amazon_toys_raw.csv Data/amazon_toys_bert.csv.gz
Then Data/build_amazon_toys.R turns the output into Data/amazon_toys.rds for the lab.
"""
import sys
import numpy as np
import pandas as pd
import torch
from transformers import AutoTokenizer, AutoModel

src, out = sys.argv[1], sys.argv[2]
d = pd.read_csv(src)

# --- cleaning, as in the CausalAIBook notebook ---------------------------------
d["number_of_reviews"] = pd.to_numeric(d["number_of_reviews"].astype(str)
                                       .str.replace(",", "").str.replace(r"\D+", "", regex=True),
                                       errors="coerce")
d["price"] = d["price"].astype(str).str.extract(r"(\d+\.*\d+)").astype(float)
d = d[d["number_of_reviews"] > 0]
d["ln_p"] = np.log(d["price"])
d["ln_q"] = np.log(d["number_of_reviews"] / d["number_of_reviews"].sum())
d["text"] = d[["product_name", "manufacturer", "product_description"]].fillna("").astype(str).agg(" | ".join, axis=1)
d["category"] = d["amazon_category_and_sub_category"].astype(str).str.split(" > ").str[0]
d = d[["uniq_id", "ln_p", "ln_q", "category", "amazon_category_and_sub_category", "text"]].dropna()
d = d[np.isfinite(d["ln_p"])].reset_index(drop=True)
print("products:", len(d))

# --- BERT, mean-pooled last layer ----------------------------------------------
torch.manual_seed(723)
tok = AutoTokenizer.from_pretrained("bert-base-uncased")
bert = AutoModel.from_pretrained("bert-base-uncased").eval()
emb = []
with torch.no_grad():
    for i in range(0, len(d), 64):
        batch = tok(list(d["text"].iloc[i:i + 64]), padding=True, truncation=True,
                    max_length=128, return_tensors="pt")
        h = bert(**batch).last_hidden_state                     # (batch, tokens, 768)
        m = batch["attention_mask"].unsqueeze(-1).float()       # 1 for real tokens
        emb.append(((h * m).sum(1) / m.sum(1)).numpy())         # mean over real tokens
        if i % 1280 == 0:
            print(i, flush=True)
E = np.vstack(emb)
E = pd.DataFrame(np.round(E, 4), columns=[f"e{j + 1}" for j in range(E.shape[1])])
pd.concat([d.drop(columns="text"), d[["text"]], E], axis=1).to_csv(out, index=False, compression="gzip")
print("wrote", out, E.shape)
