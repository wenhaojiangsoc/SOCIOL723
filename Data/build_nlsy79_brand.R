## =============================================================================
## build_nlsy79_brand.R
## Rebuilds the analysis sample of Brand, Xu, Koch, and Geraldo (2021,
## Sociological Methodology 51(2): 189-223) from a public NLSY79 extract.
##
## Input : Data/nlsy79_brand/defaultbrand2021.csv  (NLS Investigator extract,
##         tagset Data/nlsy79_brand/defaultbrand2021.NLSY79, codebook .cdb)
## Output: Data/nlsy79_brand.rds   (one row per respondent)
##
## Sample (paper, p. 200): age 14-17 at the 1979 interview, completed at least
## 12th grade, non-missing outcome. Treatment: 4-year degree by age 25
## (here: highest grade completed >= 16 by the survey year in which R turns 25).
## Outcome: share of 1990-2014 survey years in which the hourly wage of the
## main job is below 2/3 of that year's median hourly wage.
##
## Differences from the paper, all forced by the public file or by simplicity:
##  - "Southern residence at age 14" -> region of residence at the 1979 interview.
##  - Missing covariates are imputed by the sample median with a missing dummy
##    for the three variables with many missing values (parental income,
##    school disadvantage, father's education).
##  - Delinquency = any of 20 illegal-activity items in 1980 reported > 0.
##  - The yearly median wage is the weighted median among NLSY79 respondents
##    with a positive wage that year (the paper does not say which median).
## =============================================================================

library(dplyr)

d <- read.csv("Data/nlsy79_brand/defaultbrand2021.csv")
neg_na <- function(x) ifelse(x < 0, NA, x)        # -1 refusal ... -5 noninterview
d[] <- lapply(d, neg_na)

## ---- identifiers, timing ----------------------------------------------------
out <- data.frame(
  id      = d$R0000100,
  byear   = 1900 + d$R0000500,                  # birth year
  bmonth  = d$R0000300,
  weight  = d$R0216100 / 100,                   # 1979 sampling weight (2 implied decimals)
  sampid  = d$R0173600
)
## age at the 1979 interview (most interviews ran in early spring 1979; use March 31,
## which gives 5,537 respondents aged 14-17 against the paper's 5,582)
out$age79 <- 1979 - out$byear - (out$bmonth > 3)

## ---- treatment: highest grade completed by age 25 ---------------------------
hgc_years <- c(`1986` = "R2258001", `1987` = "R2445401", `1988` = "R2871101",
               `1989` = "R3074801", `1990` = "R3401501", `1991` = "R3656901")
hgc <- sapply(hgc_years, function(v) ifelse(d[[v]] == 95, NA, d[[v]]))  # 95 = ungraded
## survey year in which R turns 25; carry the last observed value forward
yr25 <- out$byear + 25
out$hgc25 <- sapply(seq_len(nrow(d)), function(i) {
  cols <- as.integer(colnames(hgc)) <= yr25[i]
  x <- hgc[i, cols]
  if (all(is.na(x))) NA else max(x, na.rm = TRUE)
})
out$college <- as.integer(out$hgc25 >= 16)
## "completed at least the 12th grade": highest grade ever reported through 1991
out$hgcmax <- apply(hgc, 1, function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE))

## ---- outcome: share of 1990-2014 waves in low-wage work ---------------------
wage_vars <- c(`1990` = "R3341500", `1991` = "R3605800", `1992` = "R3956000",
               `1993` = "R4416900", `1994` = "R5079900", `1996` = "R5165200",
               `1998` = "R6478000", `2000` = "R7005700", `2002` = "R7702900",
               `2004` = "R8495200", `2006` = "T0986800", `2008` = "T2209100",
               `2010` = "T3106900", `2012` = "T4110900", `2014` = "T5019700")
wmed <- function(x, w) {                          # weighted median
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}
low <- sapply(wage_vars, function(v) {
  wage <- d[[v]] / 100                            # cents -> dollars
  wage[wage <= 0] <- NA
  ok <- !is.na(wage)
  cut <- (2 / 3) * wmed(wage[ok], out$weight[ok])
  ifelse(ok, as.integer(wage < cut), NA)
})
out$n_waves  <- rowSums(!is.na(low))
out$lowwage  <- ifelse(out$n_waves > 0, rowSums(low, na.rm = TRUE) / out$n_waves, NA)

## ---- covariates (Brand et al. Table 1) --------------------------------------
out <- out %>% mutate(
  male       = as.integer(d$R0214800 %in% 1),
  black      = as.integer(d$R0214700 %in% 2),
  hispanic   = as.integer(d$R0214700 %in% 1),
  south      = as.integer(d$R0216400 %in% 3),
  rural      = as.integer(d$R0001800 %in% c(2, 3)),
  parinc     = d$R0217900 / 100,                 # $100s
  fatheduc   = ifelse(d$R0007900 == 95, NA, d$R0007900),
  motheduc   = ifelse(d$R0006500 == 95, NA, d$R0006500),
  fatherwc   = as.integer(!is.na(d$R0002500) & d$R0002500 >= 1 & d$R0002500 <= 245),
  twoparent  = as.integer(d$R0001900 %in% 11),
  siblings   = d$R0009100,
  ability    = as.numeric(scale(d$R0618301 / 1000)),   # AFQT percentile, standardized
  collegeprep = as.integer(d$R0019600 %in% 3),
  rotter     = d$R0153710,
  expect     = as.integer(d$R0023500 %in% 16:20),
  aspire     = as.integer(d$R0023400 %in% 16:20),
  friendasp  = as.integer(d$R0023600 %in% 16:20),
  schooldis  = d$R0017366
)
delin <- d[, sprintf("R03%05d", seq(4600, 6500, by = 100))]
out$delinq <- as.integer(rowSums(delin > 0, na.rm = TRUE) > 0)

## married by 18: marital status in the survey year R turns 18
mar <- cbind(`1979` = d$R0217501, `1980` = d$R0405601, `1981` = d$R0618601,
             `1982` = d$R0898401, `1983` = d$R1144901, `1984` = d$R1520101)
yr18 <- pmax(1979, out$byear + 18)
out$married18 <- sapply(seq_len(nrow(d)), function(i) {
  m <- mar[i, as.character(yr18[i])]
  as.integer(!is.na(m) && m > 0)
})
## child by 18: first birth before the 18th birthday
c1 <- (d$R9900002 - out$byear) * 12 + (d$R9900001 - out$bmonth)
out$child18 <- as.integer(!is.na(c1) & c1 < 18 * 12)

## ---- sample restrictions ----------------------------------------------------
s <- out %>%
  filter(age79 >= 14, age79 <= 17)                # n = 5,582 in the paper
cat("age 14-17 at 1979:", nrow(s), "\n")
s <- s %>% filter(!is.na(hgc25), !is.na(hgcmax), hgcmax >= 12)   # n = 4,548
cat("12th grade or more:", nrow(s), "\n")
s <- s %>% filter(!is.na(lowwage))                # n = 4,382
cat("non-missing outcome:", nrow(s), "\n")

## missing-value handling: median + missing dummy for the three sparse ones
for (v in c("parinc", "schooldis", "fatheduc")) {
  s[[paste0(v, "_miss")]] <- as.integer(is.na(s[[v]]))
}
for (v in c("parinc", "fatheduc", "motheduc", "siblings", "ability", "rotter", "schooldis")) {
  s[[v]][is.na(s[[v]])] <- median(s[[v]], na.rm = TRUE)
}

s <- s %>% select(id, weight, college, lowwage, n_waves,
                  male, black, hispanic, south, rural,
                  parinc, fatheduc, motheduc, fatherwc, twoparent, siblings,
                  ability, collegeprep, rotter, delinq, expect, aspire, friendasp,
                  schooldis, married18, child18,
                  parinc_miss, schooldis_miss, fatheduc_miss)
saveRDS(s, "Data/nlsy79_brand.rds")
cat("saved", nrow(s), "rows;", mean(s$college), "college\n")
