## Build Data/sexed_county.csv for Problem Set 4, Part 1.
##
## Treatment and covariates: Mark and Wu (2022, PNAS) public replication data,
##   https://github.com/NicholasDEMark/sex_ed (branch "data", data1.zip ->
##   analytic_sample.dta). Their birth counts come from restricted NCHS natality
##   files and are NOT public.
## Outcome (public substitute): NCHS model-based teen birth rates, ages 15-19, by
##   county, 2003-2020, https://data.cdc.gov/d/3h58-x6cd (births per 1,000 women).
##
## Run from the repo root: Rscript Data/build_sexed_extract.R

library(haven)
library(dplyr)

tmp <- tempfile(); dir.create(tmp)

## 1. Mark and Wu's county-year file (treatment, weights, 2010 covariates)
download.file("https://github.com/NicholasDEMark/sex_ed/raw/data/data1.zip",
              file.path(tmp, "data1.zip"), mode = "wb")
unzip(file.path(tmp, "data1.zip"), files = "analytic_sample.dta", exdir = tmp)
mw <- read_dta(file.path(tmp, "analytic_sample.dta")) |>
  zap_labels() |>
  transmute(fips = county_res, year,
            funded = comp_fund1,          ## TPP grant in the previous year (their lag)
            teen_pop = pop,               ## population aged 14-19 (their weight)
            pop_2010, med_hhinc_2010 = med_hhinc_i_2010,
            pct_pov_2010 = pct_pov_i_2010,
            pct_black_2010 = pct_blk_2010, pct_hisp_2010)

## first year a county is coded funded (0 = never funded); no county switches off
first <- mw |> filter(funded == 1) |> group_by(fips) |>
  summarise(first_funded = min(year))
mw <- mw |> left_join(first, by = "fips") |>
  mutate(first_funded = ifelse(is.na(first_funded), 0L, as.integer(first_funded)))

## 2. CDC model-based teen birth rates
download.file("https://data.cdc.gov/api/views/3h58-x6cd/rows.csv?accessType=DOWNLOAD",
              file.path(tmp, "teen.csv"), mode = "wb")
tb <- read.csv(file.path(tmp, "teen.csv")) |>
  transmute(fips = Combined.FIPS.Code, year = Year, state = State,
            county = County, birth_rate = Birth.Rate)

## 3. Merge on county and year: 2003 (CDC starts) to 2017 (Mark and Wu end)
out <- tb |> inner_join(mw, by = c("fips", "year")) |>
  mutate(state_fips = fips %/% 1000) |>
  select(fips, state_fips, state, county, year, birth_rate, funded,
         first_funded, teen_pop, pop_2010, med_hhinc_2010, pct_pov_2010,
         pct_black_2010, pct_hisp_2010) |>
  arrange(fips, year)

## keep a balanced panel (every county observed in all 15 years)
keep <- out |> count(fips) |> filter(n == 15) |> pull(fips)
out <- out |> filter(fips %in% keep)

stopifnot(!anyNA(out))
cat("rows", nrow(out), "| counties", n_distinct(out$fips),
    "| funded", n_distinct(out$fips[out$first_funded > 0]), "\n")
print(table(out$first_funded[out$year == 2003]))
write.csv(out, "Data/sexed_county.csv", row.names = FALSE)
