# ------------------------------------------------------------------
# 2024 presidential vote by state + merge with 2026 polls & tariffs.
#
# STEPS
#   1. Scrape 2024 presidential results by state (Wikipedia) and keep
#      only the Republican (Trump/Vance) result, plus Trump's
#      two-party share (Harris votes used only to compute it).
#   2. Keep only the Republican side of each 2026 Senate/Governor
#      poll (state_vi from script 2), plus its two-party share.
#   3. Merge polls + counter-tariff exposure + 2024 presidential vote.
#      All shares are numeric on a 0-100 scale (no "%" strings).
#   4. rep_diff = 2026 poll R two-party share - 2024 Trump two-party
#      share (two-party removes undecideds, which polls have and
#      results don't).
#   5. lm(rep_2party ~ tariffedPercent + incumbent)
#
# INPUT   Data/nyt_state_vote_intention_2026.csv   (script 2)
#         Data/tariffedImportsByState.csv
# OUTPUT  Data/president_2024_rep_by_state.csv
#         Data/state_polls_tariffs_pres2024.csv
#         Output/model_table.html, state_table.html, incumbent_table.html
#
# here::i_am() pins here() to the project root, so this runs from
# either R project (us_trade_conflict.Rproj or R_Scripts.Rproj).
# ------------------------------------------------------------------

suppressMessages({
  library(readr); library(dplyr); library(stringr)
  library(rvest); library(here)
  library(modelsummary); library(tinytable)
})
here::i_am("R_Scripts/3_president_2024_merge_model.R")   # root = project, not R_Scripts/

# --- 1. 2024 presidential results by state --------------------------
PRES_URL <- "https://en.wikipedia.org/wiki/2024_United_States_presidential_election"

# The "Results by state" table: two header rows, then one row per state
# (plus Maine/Nebraska congressional districts and a Total row).
# Columns 1-4 are State | Trump votes | Trump % | Trump EV.
pres_tbl <- read_html(PRES_URL) |>
  html_elements("table.wikitable") |>
  html_table(header = FALSE, fill = TRUE) |>
  Find(f = function(t) any(grepl("Trump/Vance", unlist(t[1, ]))) &&
                       any(grepl("^Alabama", t$X1)))

state_lookup <- tibble(
  state      = c(state.abb, "DC"),
  state_name = c(state.name, "District of Columbia")
)

pres_2024 <- pres_tbl |>
  slice(-(1:2)) |>
  transmute(
    # strip footnotes "[506]" and the "†" on split-EV states (ME, NE)
    state_name           = str_trim(str_remove_all(X1, "\\[.*?\\]|†")) |>
                           recode("D.C." = "District of Columbia"),
    rep_votes_2024       = parse_number(X2),
    rep_vote_share_2024  = round(parse_number(X3), 2), # "64.57%" -> 64.57
    dem_votes_2024       = parse_number(X5),           # Harris, only for 2-party
    rep_2party_2024      = round(100 * rep_votes_2024 /
                                 (rep_votes_2024 + dem_votes_2024), 2)
  ) |>
  inner_join(state_lookup, by = "state_name") |>     # drops districts & Total
  select(state, state_name, rep_votes_2024, rep_vote_share_2024, rep_2party_2024)

stopifnot(nrow(pres_2024) == 51)
write_csv(pres_2024, here("Data", "president_2024_rep_by_state.csv"))

# --- 2. Republican side of the 2026 statewide polls -----------------
state_vi <- read_csv(here("Data", "nyt_state_vote_intention_2026.csv"),
                     show_col_types = FALSE) |>
  filter(stage == "general") |>
  # Two-party share strips out undecideds so polls compare with results.
  # dem_pct is used only here, then dropped with the other D columns.
  mutate(rep_2party = round(100 * rep_pct / (rep_pct + dem_pct), 2)) |>
  select(-dem_candidate, -dem_pct, -net_dem)

# Incumbent = the Republican in the poll currently holds that seat.
# Hand-coded from the candidates in the feed (Generic Republican = no).
# Includes incumbents polled before announcing retirement (Ernst,
# Tillis, Daines) and appointed senators (Moody, Husted).
rep_incumbents <- tribble(
  ~state, ~office_type,   ~rep_candidate,
  "AK",   "U.S. Senate",  "Dan Sullivan",
  "AR",   "U.S. Senate",  "Tom Cotton",
  "FL",   "U.S. Senate",  "Ashley Moody",
  "IA",   "U.S. Senate",  "Joni Ernst",
  "ID",   "U.S. Senate",  "James Risch",
  "KS",   "U.S. Senate",  "Roger Marshall",
  "ME",   "U.S. Senate",  "Susan Collins",
  "MS",   "U.S. Senate",  "Cindy Hyde-Smith",
  "MT",   "U.S. Senate",  "Steve Daines",
  "NC",   "U.S. Senate",  "Thom Tillis",
  "NE",   "U.S. Senate",  "Pete Ricketts",
  "OH",   "U.S. Senate",  "Jon Husted",
  "SC",   "U.S. Senate",  "Lindsey Graham",
  "SD",   "U.S. Senate",  "Mike Rounds",
  "TN",   "U.S. Senate",  "Bill Hagerty",
  "TX",   "U.S. Senate",  "John Cornyn",
  "AR",   "Governor",     "Sarah Huckabee Sanders",
  "ID",   "Governor",     "Brad Little",
  "NE",   "Governor",     "Jim Pillen",
  "NH",   "Governor",     "Kelly Ayotte",
  "NV",   "Governor",     "Joe Lombardo",
  "TX",   "Governor",     "Greg Abbott",
  "VT",   "Governor",     "Phil Scott"
) |>
  mutate(incumbent = "yes")

state_vi <- state_vi |>
  left_join(rep_incumbents, by = c("state", "office_type", "rep_candidate")) |>
  mutate(incumbent = factor(coalesce(incumbent, "no"), levels = c("no", "yes")))

# --- 3. Merge polls + tariffs + 2024 presidential vote --------------
tariffs <- read_csv(here("Data", "tariffedImportsByState.csv"),
                    show_col_types = FALSE) |>
  rename(state_name = State) |>
  mutate(state_name = str_trim(state_name))           # tariffedPercent is 0-100

merged <- state_vi |>
  left_join(state_lookup, by = "state") |>
  left_join(tariffs,   by = "state_name") |>
  left_join(select(pres_2024, -state_name), by = "state") |>
  # --- 4. Republican over/under-performance vs. 2024 --------------
  mutate(rep_diff = round(rep_2party - rep_2party_2024, 2))

write_csv(merged, here("Data", "state_polls_tariffs_pres2024.csv"))

message(sprintf("merged: %d polls, %d states | missing tariff: %d | missing 2024: %d",
                nrow(merged), n_distinct(merged$state),
                sum(is.na(merged$tariffedPercent)),
                sum(is.na(merged$rep_vote_share_2024))))

# --- 5. Model -------------------------------------------------------
model <- lm(rep_2party ~ tariffedPercent + incumbent, data = merged)

# --- 6. Tables (modelsummary, not console output) -------------------
# Each table shows in the RStudio Viewer and is saved to Output/.
dir.create(here("Output"), showWarnings = FALSE)

# Regression table
reg_table <- modelsummary(
  list("Republican two-party share (%)" = model),
  stars       = TRUE,
  coef_rename = c("tariffedPercent" = "Counter-tariff exposure (%)",
                  "incumbentyes"    = "Republican incumbent"),
  gof_map     = c("nobs", "r.squared", "adj.r.squared")
)

# State-by-state data table: poll averages vs. 2024
state_table <- merged |>
  group_by(State = state_name) |>
  summarise(
    "Polls"                                = as.character(n()),  # whole number, not 5.00
    "Counter-tariff exposure (%)"          = first(tariffedPercent),
    "Trump 2024, two-party (%)"            = first(rep_2party_2024),
    "Republican 2026 polls, two-party (%)" = mean(rep_2party),
    "Change since 2024 (pts)"              = mean(rep_diff)
  ) |>
  datasummary_df(fmt = 2)

# Change since 2024 by incumbency
incumbent_table <- datasummary(
  Heading("Republican incumbent") * incumbent ~
    rep_diff * (Heading("Polls") * N +
                Heading("Mean change since 2024 (pts)") * Mean),
  data = merged, fmt = 2
)

save_tt(reg_table,       here("Output", "model_table.html"),     overwrite = TRUE)
save_tt(state_table,     here("Output", "state_table.html"),     overwrite = TRUE)
save_tt(incumbent_table, here("Output", "incumbent_table.html"), overwrite = TRUE)

print(reg_table)
print(state_table)
print(incumbent_table)
