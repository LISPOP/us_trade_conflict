# ------------------------------------------------------------------
# 2024 presidential vote by state + merge with 2026 polls & tariffs.
#
# STEPS
#   1. Scrape 2024 presidential results by state (Wikipedia) and keep
#      only the Republican (Trump/Vance) result, plus Trump's
#      two-party share (Harris votes used only to compute it).
#   2. Keep only the Republican side of each 2026 Senate/Governor
#      poll (state_vi from script 2), plus its two-party share.
#   3. Merge polls + counter-tariff exposure + Canada trade shares
#      + 2024 presidential vote.
#      All shares are numeric on a 0-100 scale (no "%" strings).
#   4. rep_diff = 2026 poll R two-party share - 2024 Trump two-party
#      share (two-party removes undecideds, which polls have and
#      results don't).
#   5. lm(rep_2party ~ tariffedPercent + incumbent)
#
# INPUT   Data/nyt_state_vote_intention_2026.csv   (script 2)
#         Data/tariffedImportsByState.csv
#         Data/state_trade_with_canada_2025_50states.xlsx
# OUTPUT  Data/president_2024_rep_by_state.csv
#         Data/state_polls_tariffs_pres2024.csv
#         Output/README.md  (tables; renders on GitHub)
#
# here::i_am() pins here() to the project root, so this runs from
# either R project (us_trade_conflict.Rproj or R_Scripts.Rproj).
# ------------------------------------------------------------------

suppressMessages({
  library(readr); library(readxl); library(dplyr); library(stringr)
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
  mutate(state_name = str_trim(state_name) |>        # tariffedPercent is 0-100
                      recode("Washington, State" = "Washington"))

# Canada's share of each state's 2025 goods trade (Census via ITA
# TradeStats Express). Header is on row 4; the "All 50 states" total and
# footnotes have no Code, so filtering on Code keeps the 50 states.
# Shares come as 0-1 fractions; x100 to match the 0-100 scale.
canada_trade <- read_excel(here("Data", "state_trade_with_canada_2025_50states.xlsx"),
                           skip = 3) |>
  filter(!is.na(Code)) |>
  transmute(
    state                    = Code,
    canada_share_exports     = round(100 * `Canada share of exports`, 2),
    canada_share_imports     = round(100 * `Canada share of imports`, 2),
    canada_share_total_trade = round(100 * `Canada share of total trade`, 2)
  )

stopifnot(nrow(canada_trade) == 50)

tariffs
# The first three lines link the state abbreviation in state_vi with 
# the state names that were generated in state_lookup
# because the tariffs data frame contains information only by state name. 
merged <- state_vi |>
  left_join(state_lookup, by = "state") |>
  left_join(tariffs,   by = "state_name") |>
  left_join(canada_trade, by = "state") |>
  left_join(select(pres_2024, -state_name), by = "state") |>
  # --- 4. Republican over/under-performance vs. 2024 --------------
  mutate(rep_diff = round(rep_2party - rep_2party_2024, 2))

write_csv(merged, here("Data", "state_polls_tariffs_pres2024.csv"))

message(sprintf("merged: %d polls, %d states | missing tariff: %d | missing Canada trade: %d | missing 2024: %d",
                nrow(merged), n_distinct(merged$state),
                sum(is.na(merged$tariffedPercent)),
                sum(is.na(merged$canada_share_total_trade)),
                sum(is.na(merged$rep_vote_share_2024))))


# Create a new poll before or after counter tarrifs go into effect

merged |> 
  mutate(after_tariffs=case_when(
    start_date>"2026-09-07"~"Post",
    start_date>"2026-08-01" &start_date<"2026-09-08"~ "Pre",
    TRUE~NA_character_)) |> 
  mutate(after_tariffs=factor(after_tariffs, levels=c("Pre", "Post")))->merged

# --- 5. Model -------------------------------------------------------
model1 <- lm(rep_diff ~ tariffedPercent*after_tariffs + incumbent, data = merged)
summary(model1)
# --- 6. Tables (modelsummary, not console output) -------------------
# Each table is built once as a data frame, then
#   - shown in the RStudio Viewer (tinytable), and
#   - written to Output/README.md as Markdown pipe tables, which GitHub
#     renders on the Output/ folder page.
dir.create(here("Output"), showWarnings = FALSE)

fmt2 <- function(df) mutate(df, across(where(is.double), ~ formatC(.x, format = "f", digits = 2)))
stars_note <- "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"

# Regression table
reg_df <- modelsummary(
  list("Change in R two-party share since 2024 (pts)" = model1),
  stars       = TRUE,
  # coef_rename = c("tariffedPercent" = "Counter-tariff exposure (%)",
  #                 "incumbentyes"    = "Republican incumbent", 
  #                 "after_tariffs"="After Tariffs"),
  gof_map     = c("nobs", "r.squared", "adj.r.squared"),
  output      = "data.frame"
) |>
  mutate(term = if_else(statistic == "std.error", "", term)) |>   # SE under its estimate
  select(-part, -statistic) |>
  rename(" " = term)

reg_df
#### Model 2 - Exclude gubernatorial races, include only senate races
glimpse(merged)
model2<-lm(rep_diff ~ tariffedPercent*after_tariffs + incumbent, data = filter(merged, office_type!="Governor"))
summary(model2)

# State-by-state data table: poll averages vs. 2024
state_df <- merged |>
  group_by(State = state_name) |>
  summarise(
    "Polls"                                = n(),
    "Counter-tariff exposure (%)"          = first(tariffedPercent),
    "Trump 2024, two-party (%)"            = first(rep_2party_2024),
    "Republican 2026 polls, two-party (%)" = mean(rep_2party),
    "Change since 2024 (pts)"              = mean(rep_diff)
  ) |>
  fmt2()

# Change since 2024 by incumbency
incumbent_df <- merged |>
  group_by("Republican incumbent" = incumbent) |>
  summarise("Polls"                        = n(),
            "Mean change since 2024 (pts)" = mean(rep_diff)) |>
  fmt2()

# Markdown for GitHub
md_table <- function(title, df, note = NULL) {
  c(paste("##", title), "",
    knitr::kable(df, format = "pipe", align = c("l", rep("r", ncol(df) - 1))),
    if (!is.null(note)) c("", paste0("*", gsub("*", "\\*", note, fixed = TRUE), "*")), "")  # escape stars
}
writeLines(c(
  "# Results: 2026 Republican polling, counter-tariffs and incumbency", "",
  "Generated by `R_Scripts/3_president_2024_merge_model.R`. Shares are",
  "Republican two-party shares, R / (R + D), on a 0-100 scale.", "",
  md_table("Table 1. Regression of change in Republican two-party share since 2024", reg_df,
           paste("OLS, one observation per poll. Standard errors in parentheses.", stars_note)),
  md_table("Table 2. Change since 2024 by Republican incumbency", incumbent_df),
  md_table("Table 3. State-by-state polling vs. 2024", state_df,
           "Change since 2024 = average 2026 poll two-party share minus Trump's 2024 two-party share.")
), here("Output", "README.md"))

# RStudio Viewer
print(tt(reg_df, notes = stars_note))
print(tt(incumbent_df))
print(tt(state_df))
