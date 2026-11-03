## Week 10 -- TWFE on the marriage data is a staggered DiD.
## Timing groups by first year married; Goodman-Bacon decomposition on men whose marriage is absorbing.
##   Rscript figs/w10_marriage_timing.R   (from the week folder)
suppressMessages({library(dplyr); library(fixest); library(bacondecomp)})
data(wagepan, package = "wooldridge")
s <- wagepan |> arrange(nr, year) |> group_by(nr) |>
  summarise(first = if (any(married == 1)) min(year[married == 1]) else NA_integer_,
            always = all(married == 1), never = all(married == 0),
            off = any(diff(married) < 0), .groups = "drop")
cat("men:", nrow(s), "\n")
cat("never married 1980-87:", sum(s$never), "; married all 8 years:", sum(s$always),
    "; marry and later leave marriage:", sum(s$off), "\n")
cat("marry once and stay married, by year of marriage:\n")
print(table(s$first[!s$always & !s$never & !s$off]))

## absorbing sample: drop men who leave marriage (bacon needs D to switch on once)
keep <- s$nr[!s$off]
ab <- wagepan |> filter(nr %in% keep)
cat("\nTWFE, all men:", round(coef(feols(lwage ~ married | nr + year, wagepan))[["married"]], 3),
    "; absorbing sample:", round(coef(feols(lwage ~ married | nr + year, ab))[["married"]], 3), "\n")
b <- bacon(lwage ~ married, data = as.data.frame(ab), id_var = "nr", time_var = "year", quietly = TRUE)
out <- b |> group_by(type) |> summarise(avg_2x2 = weighted.mean(estimate, weight), total_weight = sum(weight))
print(as.data.frame(out) |> mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("weighted sum:", round(sum(b$weight * b$estimate), 3), "\n")
