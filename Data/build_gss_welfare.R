## Builds Data/gss_welfare.csv: the GSS spending-question wording experiment, 1986-2010
## ("welfare" vs. "assistance to the poor"), used in Problem Set 5, Part 3.
##
## Source: the processed General Social Survey file distributed with Athey and Wager's
## causal-forest tutorials (gsbDBI/ExperimentData, Welfare/ProcessedData/welfarenolabel3.csv).
## In that file w = 1 is the "assistance to the poor" wording and y = 1 is the answer
## "too much"; missing values are coded -999.
##
## Run from the repository root:  Rscript Data/build_gss_welfare.R

src <- paste0("https://raw.githubusercontent.com/gsbDBI/ExperimentData/master/",
              "Welfare/ProcessedData/welfarenolabel3.csv")
raw <- read.csv(src)
na <- function(v) ifelse(v == -999, NA, v)

g <- data.frame(
  year      = raw$year,
  id        = raw$id,
  welfare   = 1 - raw$w,                 ## 1 = asked about "welfare", 0 = "assistance to the poor"
  toomuch   = raw$y,                     ## 1 = "too much" is being spent
  age       = na(raw$age),
  educ      = na(raw$educ),              ## years of schooling
  female    = as.numeric(raw$sex == 2),
  black     = as.numeric(raw$race == 2),
  otherrace = as.numeric(raw$race == 3),
  partyid   = ifelse(raw$partyid %in% 0:6, raw$partyid, NA),    ## 0 strong Democrat ... 6 strong Republican
  polviews  = ifelse(raw$polviews %in% 1:7, raw$polviews, NA),  ## 1 extremely liberal ... 7 extremely conservative
  income    = na(raw$income),            ## GSS family income category, 1-12
  childs    = na(raw$childs),            ## number of children
  married   = as.numeric(raw$marital == 1),
  fulltime  = as.numeric(raw$wrkstat == 1))

g <- g[complete.cases(g), ]              ## 28,159 of 36,501 respondents
cat("rows:", nrow(g), "\n")
print(round(tapply(g$toomuch, g$welfare, mean), 3))
write.csv(g, "Data/gss_welfare.csv", row.names = FALSE)
