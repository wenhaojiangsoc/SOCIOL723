## ---------------------------------------------------------------
## SOCIOL 723 -- build the GSS analytic extract used in Labs 1-2
##
## Source : General Social Survey Cumulative File 1972-2022 (NORC),
##          accessed through Kieran Healy's `gssr` package.
## Output : Data/gss_earnings.rds  (one row per respondent)
##
## Run once; students receive the .rds and never need `gssr`.
## ---------------------------------------------------------------

library(gssr)
library(dplyr)
library(haven)

data(gss_all)

years_keep <- c(2010, 2012, 2014, 2016, 2018, 2022)   # years carrying WORDSUM

gss_earnings <- gss_all %>%
  filter(year %in% years_keep) %>%
  transmute(
    year     = as.integer(year),
    id       = as.integer(id),
    realrinc = zap_labels(realrinc),          # personal income, constant 1986 $
    educ     = zap_labels(educ),              # years of schooling completed
    paeduc   = zap_labels(paeduc),
    maeduc   = zap_labels(maeduc),
    age      = zap_labels(age),
    sex      = zap_labels(sex),
    race     = zap_labels(race),
    wordsum  = zap_labels(wordsum),           # 0-10 vocabulary test
    prestige = zap_labels(prestg10),
    sei      = zap_labels(sei10),
    hours    = zap_labels(hrs1),
    wrkstat  = zap_labels(wrkstat),
    degree   = zap_labels(degree),            # 0 = LT HS ... 4 = graduate
    childs   = zap_labels(childs),            # number of children (count)
    marital  = zap_labels(marital),
    region   = zap_labels(region),
    vpsu     = zap_labels(vpsu),
    vstrat   = zap_labels(vstrat),
    wt       = zap_labels(wtssps)
  ) %>%
  filter(
    wrkstat == 1,                             # working full time
    age >= 25, age <= 64,
    !is.na(realrinc), realrinc > 0,
    !is.na(educ), !is.na(hours)
  ) %>%
  mutate(
    lnearn   = log(realrinc),
    exper    = pmax(age - educ - 6, 0),       # potential labour-market experience
    female   = as.integer(sex == 2),
    black    = as.integer(race == 2),
    otherace = as.integer(race == 3),
    paeduc   = ifelse(paeduc > 20, NA, paeduc),
    maeduc   = ifelse(maeduc > 20, NA, maeduc),
    pareduc  = pmax(paeduc, maeduc, na.rm = TRUE),
    region   = factor(region,
                      levels = 1:9,
                      labels = c("New England", "Middle Atlantic", "E. North Central",
                                 "W. North Central", "South Atlantic", "E. South Central",
                                 "W. South Central", "Mountain", "Pacific")),
    stratum  = as.integer(vstrat),
    psu      = as.integer(paste0(vstrat, vpsu)),
    ba       = as.integer(degree >= 3),       # binary: bachelor's or more
    degree   = factor(degree, levels = 0:4, ordered = TRUE,
                      labels = c("< HS", "HS", "Junior college",
                                 "Bachelor", "Graduate")),
    evermar  = as.integer(marital != 5)       # ever married
  ) %>%
  select(year, id, lnearn, realrinc, educ, degree, ba, exper, age, female, black,
         otherace, wordsum, pareduc, prestige, sei, hours, childs, evermar,
         region, stratum, psu, wt) %>%
  ## complete cases on the analysis variables, so that every model in the labs
  ## is estimated on exactly the same n (WORDSUM is a ballot item, so this is
  ## the binding restriction)
  filter(complete.cases(.)) %>%
  as.data.frame()

saveRDS(gss_earnings, "Data/gss_earnings.rds")

cat("n =", nrow(gss_earnings), "\n")
print(summary(gss_earnings[, c("lnearn", "educ", "exper", "wordsum", "pareduc", "hours")]))
print(table(gss_earnings$year))
cat("clusters: psu =", length(unique(gss_earnings$psu)),
    " stratum =", length(unique(gss_earnings$stratum)),
    " region =", nlevels(droplevels(gss_earnings$region)), "\n")
