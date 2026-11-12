# ------------------------------------------------------------------
# Build one cleaned county file of 2026 results.
#
# Sources
#   aggregator: Data/county/results_2026_civicapi/counties.json
#               (civicAPI via jcalepos146/Electoral-Path-2026, 9 Nov 2026)
#   official:   Data/county/official_state_feeds_2026/county_results_2026_official.csv
#               (state election office feeds via pushbuild, 6 Nov 2026;
#                Senate and governor, 18 states)
#
# For each race (senate, governor, house) the output has
#   *_r2p      Republican share of Republican + Democratic votes, 0-100
#   *_rmargin  Republican minus Democratic share of all votes, points
#              (aggregator only)
#   *_source   "official", "aggregator", or
#              "aggregator (official differs)"
# plus the 2024 and 2020 presidential margins and 2024 total votes.
#
# CLEANING RULES (each one found by a sanity check, 2026-11)
#   1. Alaska and DC rows are dropped: the aggregator reports state
#      house districts and wards under county-style FIPS codes.
#   2. A race is blanked in a county when either major party has
#      under 5% of the vote (uncontested or same-party contest).
#   3. A race is blanked for a whole state when Republicans and
#      Democrats together have under 90% of the state's vote (an
#      independent or third candidate was a main contender).
#   4. The official figure is used only when it is within 1 point of
#      the aggregator. Otherwise the aggregator is used and flagged:
#      it is three days newer, and the official file has four North
#      Carolina counties under the wrong names.
#
# OUTPUT  Data/county/county_results_2026_combined.csv
# ------------------------------------------------------------------

suppressMessages({ library(jsonlite); library(dplyr); library(here) })
here::i_am("R_Scripts/3b_build_county_results_2026.R")

name_key <- function(x) {
  x <- tolower(x)
  x <- gsub("saint |st\\. ", "st ", x)
  x <- gsub("\\b(county|parish)\\b", "", x)
  gsub("[^a-z]", "", x)
}

agg <- fromJSON(here("Data/county/results_2026_civicapi/counties.json")) %>%
  filter(!st %in% c("AK", "DC")) %>%                                 # rule 1
  mutate(key = name_key(county))

off <- read.csv(here("Data/county/official_state_feeds_2026/county_results_2026_official.csv")) %>%
  filter(state != "AK") %>%
  transmute(st = state, key = name_key(county), race, off_r2p = r_share_2p)

races <- c(senate = "sen26", governor = "gov26", house = "house26")
out <- agg %>% transmute(fips, state = st, county, key,
                         rmargin24 = -m24, rmargin20 = -m20, votes24 = t24)
dropped <- list()

for (race in names(races)) {
  p <- races[[race]]
  r <- agg[[paste0(p, "_r")]]; d <- agg[[paste0(p, "_d")]]; tot <- agg[[paste0(p, "_total")]]

  # rule 3: state-level major-party share
  major <- tapply(tot * (r + d) / 100, agg$st, sum, na.rm = TRUE) /
           tapply(ifelse(is.na(r + d), NA, tot), agg$st, sum, na.rm = TRUE) * 100
  indep_states <- names(major)[!is.na(major) & major < 90]

  bad <- is.na(r) | is.na(d) | pmin(r, d) < 5 |                       # rule 2
         agg$st %in% indep_states                                     # rule 3
  r2p <- ifelse(bad, NA, 100 * r / (r + d))
  rmargin <- ifelse(bad, NA, r - d)

  o <- off$off_r2p[match(paste(agg$st, agg$key, race), paste(off$st, off$key, off$race))]
  agree <- !is.na(o) & !is.na(r2p) & abs(o - r2p) <= 1                # rule 4
  source <- ifelse(is.na(r2p), "",
            ifelse(agree, "official",
            ifelse(!is.na(o), "aggregator (official differs)", "aggregator")))

  out[[paste0(race, "_r2p")]]     <- round(ifelse(agree, o, r2p), 2)
  out[[paste0(race, "_rmargin")]] <- round(rmargin, 2)
  out[[paste0(race, "_source")]]  <- source
  dropped[[race]] <- list(independent_states = indep_states,
                          one_sided_counties = sum(!is.na(r) & !is.na(d) & pmin(r, d) < 5 &
                                                   !agg$st %in% indep_states))
}

out <- select(out, -key)
write.csv(out, here("Data/county/county_results_2026_combined.csv"), row.names = FALSE, na = "")

cat("counties:", nrow(out), "\n")
for (race in names(races)) {
  cat(sprintf("%-9s values: %d | sources: %s\n  whole states blanked (independents): %s | one-sided counties blanked: %d\n",
              race, sum(!is.na(out[[paste0(race, "_r2p")]])),
              paste(names(table(out[[paste0(race, "_source")]])), table(out[[paste0(race, "_source")]]), collapse = ", "),
              paste(dropped[[race]]$independent_states, collapse = " "),
              dropped[[race]]$one_sided_counties))
}
