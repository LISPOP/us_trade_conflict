# ------------------------------------------------------------------
# County-level models, using Autor, Dorn, Hanson and Majlesi (2020,
# AER) as the starting guide.
#
# STEPS
#   1. Reproduce the paper's Table 5 (trade exposure and the change in
#      the county Republican presidential vote share) from its own
#      replication data, to confirm we have the design right.
#      Design: county-level change in vote share ~ trade exposure
#      + controls, weighted by county votes, clustered SEs.
#   2. Apply the same design to 2026: change from the 2024
#      presidential margin to the 2026 House margin, by county.
#      FIRST LOOK ONLY. No county measure of Canada's 2026
#      counter-tariffs exists yet, so exposure is proxied two ways:
#        a. tariffedPercent, the state-level counter-tariff exposure
#           used in script 3, assigned to every county in the state;
#        b. the county-level 2018 Canadian retaliation shock from
#           Blanchard, Bown and Chor (old tariff list).
#      The outcome is the Republican margin (R minus D, in points of
#      all votes), because the 2026 file has no 2024 two-party shares.
#      2026 counts are unofficial (civicAPI, 9 Nov 2026).
#
# INPUT   Data/county/adhm_2020/ADHM-PoliticalPolarization-FileArchive.zip
#         Data/county/bbc_2019/Replication_BBC_15Oct19.zip
#         Data/county/lake_nie_2023/estimationDataset.dta  (2016 controls)
#         Data/county/county_results_2026_combined.csv     (script 3b)
#         Data/tariffedImportsByState.csv
#         (the two zips are not in git; links in Data/county_data_sources.md)
# OUTPUT  Output/county_models.md
# ------------------------------------------------------------------

suppressMessages({
  library(haven); library(jsonlite); library(dplyr); library(here)
  library(sandwich); library(knitr)
})
here::i_am("R_Scripts/4_county_autor_guide.R")   # root = project, not R_Scripts/

read_dta_from_zip <- function(zip, member) {
  dir <- tempfile(); dir.create(dir)
  read_dta(unzip(zip, files = member, exdir = dir))
}
stars <- function(t) ifelse(abs(t) > 3.29, "***", ifelse(abs(t) > 2.58, "**",
                     ifelse(abs(t) > 1.96, "*", ifelse(abs(t) > 1.645, "+", ""))))
cell <- function(b, se) sprintf("%.2f%s (%.2f)", b, stars(b / se), se)

# Weighted 2SLS with cluster-robust SEs (no small-sample correction,
# as in Stata's ivreg2). Returns coefficient and SE on `endog`.
tsls <- function(data, y, endog, instr, controls, weight, cluster) {
  d <- data[complete.cases(data[, c(y, endog, instr, controls, weight, cluster)]), ]
  w <- d[[weight]]
  X <- cbind(1, as.matrix(d[, c(endog, controls)]))
  Z <- cbind(1, as.matrix(d[, c(instr, controls)]))
  Xhat <- Z %*% solve(crossprod(Z, w * Z), crossprod(Z, w * X))
  bread <- solve(crossprod(Xhat, w * X))
  beta <- bread %*% crossprod(Xhat, w * d[[y]])
  u <- as.numeric(d[[y]] - X %*% beta)
  score <- rowsum(Xhat * (w * u), d[[cluster]])
  V <- bread %*% crossprod(score) %*% bread
  c(b = beta[2], se = sqrt(V[2, 2]), n = nrow(d))
}

# --- 1. Reproduce Autor et al. Table 5 ------------------------------
adhm <- read_dta_from_zip(
  here("Data/county/adhm_2020/ADHM-PoliticalPolarization-FileArchive.zip"),
  "2-FinalDataPackage/dta/president_2000_2016.dta"
) %>%
  mutate(d0008 = shnr_pres2008 - shnr_pres2000,
         d0016 = shnr_pres2016 - shnr_pres2000)

indocc <- c("l_shind_manuf_cbp", "l_sh_routine33", "l_task_outsource")
region <- grep("^reg_", names(adhm), value = TRUE)
demog  <- grep("^l_sh_pop|^l_sh_fborn", names(adhm), value = TRUE)
lagvote <- c("shnr_pres1992", "shnr_pres1996")
specs <- list(character(0), indocc, c(indocc, region),
              c(indocc, region, demog), c(indocc, region, demog, lagvote))

table5 <- sapply(c(d0008 = "d0008", d0016 = "d0016"), function(y)
  sapply(specs, function(ctrl) {
    r <- tsls(adhm, y, "d_imp_usch_pd_0008", "d_imp_otch_lag_pd_0008",
              ctrl, "totvote_2000pres", "czone")
    cell(r["b"], r["se"])
  }))
table5 <- data.frame(
  Panel = c("A. Change in Republican share, 2000-2008",
            "B. Change in Republican share, 2000-2016",
            "Published, panel A", "Published, panel B"),
  rbind(t(table5),
        c("1.54 (0.73)", "5.60 (1.41)", "2.38 (1.24)", "1.75 (0.86)", "1.59 (0.85)"),
        c("3.86 (1.48)", "3.98 (1.69)", "1.72 (1.71)", "1.99 (0.97)", "1.71 (0.90)"))
)
names(table5) <- c("", "(1)", "(2) + industry", "(3) + region",
                   "(4) + demography", "(5) + past vote")

# --- 2. Same design, 2026 -------------------------------------------
# Cleaned county results from script 3b (Alaska, DC, uncontested races and
# races with a strong independent are already removed there).
res <- read.csv(here("Data/county/county_results_2026_combined.csv"),
                colClasses = c(fips = "character")) %>%
  transmute(fips, st = state, rmargin24, rmargin20, votes24,
            rmargin26_house = house_rmargin, rmargin26_senate = senate_rmargin)

tariff <- read.csv(here("Data/tariffedImportsByState.csv")) %>%
  mutate(State = ifelse(State == "Washington, State", "Washington", State),
         st = state.abb[match(State, state.name)]) %>%
  select(st, tariffedPercent)

bbc <- read_dta_from_zip(
  here("Data/county/bbc_2019/Replication_BBC_15Oct19.zip"),
  "Replication_BBC_15Oct19/workfile_12Jun19.dta"
) %>%
  transmute(fips = sprintf("%05d", as.integer(fips)), can_retal_2018 = cty_can3_TS)

ctrl16 <- read_dta(here("Data/county/lake_nie_2023/estimationDataset.dta")) %>%
  transmute(fips = county_fips, college = educ_collGrad2016, white = racwht2016,
            manuf = empManufacturingShare2016, agmining = empAgMiningShare2016,
            age6574 = Age6574share2016)

cty <- res %>%
  left_join(tariff, by = "st") %>%
  left_join(bbc, by = "fips") %>%
  left_join(ctrl16, by = "fips") %>%
  mutate(d_house  = rmargin26_house - rmargin24,
         d_senate = rmargin26_senate - rmargin24,
         trend2024 = rmargin24 - rmargin20,
         # standardise both exposure measures so coefficients are per 1 SD
         z_state  = as.numeric(scale(tariffedPercent)),
         z_county = as.numeric(scale(can_retal_2018)))

controls <- c("rmargin24", "trend2024", "college", "white", "manuf",
              "agmining", "age6574")
fit <- function(y, x, ctrl) {
  d <- cty[complete.cases(cty[, c(y, x, ctrl, "votes24", "st")]), ]
  m <- lm(reformulate(c(x, ctrl), y), data = d, weights = votes24)
  se <- sqrt(diag(vcovCL(m, cluster = d$st)))[x]
  c(cell(coef(m)[x], se), nrow(d), length(unique(d$st)))
}
models <- expand.grid(y = c("d_house", "d_senate"), x = c("z_state", "z_county"),
                      stringsAsFactors = FALSE)
table2026 <- do.call(rbind, lapply(seq_len(nrow(models)), function(i) {
  y <- models$y[i]; x <- models$x[i]
  a <- fit(y, x, character(0)); b <- fit(y, x, controls)
  data.frame(
    Outcome = ifelse(y == "d_house", "House 2026 vs. president 2024",
                     "Senate 2026 vs. president 2024"),
    Exposure = ifelse(x == "z_state", "State counter-tariff share (current)",
                      "County Canadian retaliation shock (2018)"),
    `No controls` = a[1], `With controls` = b[1],
    Counties = b[2], States = b[3], check.names = FALSE)
}))

# --- Output ---------------------------------------------------------
out <- c(
  "# County-level models, following Autor et al. (2020)",
  "",
  "Generated by `R_Scripts/4_county_autor_guide.R`.",
  "",
  "## Table 1. Reproduction of Autor et al. Table 5",
  "",
  "2SLS coefficient on the 2000-2008 change in Chinese import penetration.",
  "Outcome: change in the county Republican two-party presidential share.",
  "3,107 counties, weighted by 2000 votes, SEs clustered by commuting zone.",
  "",
  kable(table5, format = "pipe", row.names = FALSE),
  "",
  "## Table 2. First look at 2026 (exploratory)",
  "",
  "OLS. Outcome: change in the county Republican margin (R minus D, points of",
  "all votes) from the 2024 presidential vote to the 2026 vote. Coefficients",
  "are per one standard deviation of exposure. Weighted by 2024 votes, SEs",
  "clustered by state. Controls: 2024 margin, 2020-2024 change in margin, and",
  "2016 college, white, manufacturing, agriculture/mining and age 65-74 shares.",
  "",
  kable(table2026, format = "pipe", row.names = FALSE),
  "",
  "*+ p < 0.1, \\* p < 0.05, \\*\\* p < 0.01, \\*\\*\\* p < 0.001*",
  "",
  "Caveats: 2026 counts are unofficial. Neither exposure measure is the one we",
  "want: the first varies only across states, the second uses Canada's 2018",
  "tariff list. Alaska, DC, uncontested races and races with a strong",
  "independent are excluded (see `R_Scripts/3b_build_county_results_2026.R`).",
  "California was still counting on 9 Nov, so its figures are incomplete."
)
writeLines(out, here("Output/county_models.md"))
cat(out, sep = "\n")
