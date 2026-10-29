# ------------------------------------------------------------------
# 2024 presidential vote + merge with 2026 Senate & House polls and
# counter-tariff exposure.
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
#      rep_diff = 2026 poll R two-party share - 2024 Trump two-party
#      share (two-party removes undecideds, which polls have and
#      results don't).
#   4. lm(rep_diff ~ tariffedPercent + incumbent), Senate vs. House,
#      SEs clustered by state.
#
# INPUT   Data/nyt_state_vote_intention_2026.csv   (script 2)
#         Data/nyt_house_polls_2026.csv            (script 2)
#         Data/tariffedImportsByState.csv
#         Data/state_trade_with_canada_2025_50states.xlsx
# OUTPUT  Data/president_2024_rep_by_state.csv
#         Data/president_2024_rep_by_district.csv
#         Data/polls_tariffs_pres2024.csv
#
# here::i_am() pins here() to the project root, so this runs from
# either R project (us_trade_conflict.Rproj or R_Scripts.Rproj).
# ------------------------------------------------------------------

suppressMessages({
  library(readr); library(readxl); library(dplyr); library(stringr)
  library(tidyr); library(purrr)
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

# --- 2. 2024 presidential vote by House district (2026 lines) -------
# Source: The Downballot's district-level presidential calculations,
# "exact vote totals" tab of their public Google Sheet, read as CSV.
# These are on the district lines used in 2026, so they already
# reflect the mid-decade redraws (TX, CA, MO, NC, OH, UT) and match
# the districts the 2026 House polls are asked about.
#   https://www.the-downballot.com/p/the-downballots-calculations-of-presidential
# Layout: 2 title rows, 2 header rows, then one row per district.
# Columns 1, 5, 6 are District ("AK-AL", "TX-15") | Harris | Trump.
DISTRICT_URL <- paste0("https://docs.google.com/spreadsheets/d/",
                       "1eZfaFI-c-PFOoKx1-zZA2MP0_dxRq_LVK0re3BOQqy0",
                       "/export?format=csv&gid=1491069057")

pres_2024_cd <- read_csv(DISTRICT_URL, skip = 4, col_names = FALSE,
                         col_types = cols(.default = "c")) |>
  filter(str_detect(X1, "^[A-Z]{2}-(\\d{2}|AL)$")) |>
  transmute(
    state           = str_sub(X1, 1, 2),
    # At-large seats are seat_number 1 in the NYT feed ("AK-AL" -> 1)
    seat_number     = as.numeric(str_replace(str_sub(X1, 4), "AL", "1")),
    rep_votes_2024  = parse_number(X6),
    dem_votes_2024  = parse_number(X5),            # Harris, only for 2-party
    # Same measure as the state-level rep_2party_2024 above
    rep_2party_2024 = round(100 * rep_votes_2024 /
                            (rep_votes_2024 + dem_votes_2024), 2)
  ) |>
  select(-dem_votes_2024)

stopifnot(nrow(pres_2024_cd) == 435,
          !anyNA(pres_2024_cd$rep_2party_2024))
write_csv(pres_2024_cd, here("Data", "president_2024_rep_by_district.csv"))

# 2024 Trump baseline for each poll: the state result for Senate polls,
# the district result for House polls. Senate polls have no
# seat_number, so their key is NA (dplyr joins NA to NA).
pres_baseline <- bind_rows(
  transmute(pres_2024,    chamber = "Senate", state, seat_number = NA_real_, rep_2party_2024),
  transmute(pres_2024_cd, chamber = "House",  state, seat_number,            rep_2party_2024)
)

# --- 3. Republican side of the 2026 Senate and House polls ----------
# Senate polls come from the statewide file (governor races dropped);
# House district polls are stacked underneath. Both files have the
# same columns (script 2). National generic-ballot House rows (state
# "US") drop out: there is no district to compare them with.
state_vi <- bind_rows(
  read_csv(here("Data", "nyt_state_vote_intention_2026.csv"),
           show_col_types = FALSE) |>
    filter(office_type == "U.S. Senate"),
  read_csv(here("Data", "nyt_house_polls_2026.csv"),
           show_col_types = FALSE) |>
    filter(state != "US")
) |>
  filter(stage == "general") |>
  # Two-party share strips out undecideds so polls compare with results.
  # Opponent = leading non-Republican (script 2); opp_pct is used only
  # here. opp_party and three_way are kept for robustness checks.
  mutate(chamber    = if_else(office_type == "U.S. Senate", "Senate", "House"),
         rep_2party = round(100 * rep_pct / (rep_pct + opp_pct), 2)) |>
  select(-opp_candidate, -opp_pct, -net_opp)

# Incumbent = the Republican in the poll currently holds a seat in that
# chamber for that state. Hand-coded from the candidates in the feed
# (Generic Republican = no).
# Senate: includes incumbents polled before announcing retirement
# (Ernst, Tillis, Daines) and appointed senators (Moody, Husted).
# House: checked against the sitting House members (unitedstates/
# congress-legislators, Oct 2026). Members running in a renumbered or
# redrawn district still count (Calvert CA-41 -> CA-40; the Utah
# members), and Calvert and Young Kim are both incumbents in CA-40.
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
  "AK",   "U.S. House",   "Nick Begich",
  "AR",   "U.S. House",   "French Hill",
  "AZ",   "U.S. House",   "Eli Crane",
  "AZ",   "U.S. House",   "Juan Ciscomani",
  "CA",   "U.S. House",   "David G. Valadao",
  "CA",   "U.S. House",   "Ken Calvert",
  "CA",   "U.S. House",   "Young Kim",
  "CA",   "U.S. House",   "Darrell Issa",
  "CO",   "U.S. House",   "Jeff Hurd",
  "CO",   "U.S. House",   "Lauren Boebert",
  "CO",   "U.S. House",   "Jeff Crank",
  "CO",   "U.S. House",   "Gabe Evans",
  "FL",   "U.S. House",   "Cory Mills",
  "FL",   "U.S. House",   "Mike Haridopolos",
  "FL",   "U.S. House",   "Anna Paulina Luna",
  "FL",   "U.S. House",   "Brian Mast",
  "FL",   "U.S. House",   "María Elvira Salazar",
  "GA",   "U.S. House",   "Brian Jack",
  "GA",   "U.S. House",   "Rich McCormick",
  "IA",   "U.S. House",   "Mariannette Miller-Meeks",
  "IA",   "U.S. House",   "Zach Nunn",
  "ID",   "U.S. House",   "Russ Fulcher",
  "ID",   "U.S. House",   "Mike Simpson",
  "IN",   "U.S. House",   "Victoria Spartz",
  "KS",   "U.S. House",   "Derek Schmidt",
  "MD",   "U.S. House",   "Andy Harris",
  "MI",   "U.S. House",   "Bill Huizenga",
  "MI",   "U.S. House",   "Tom Barrett",
  "MN",   "U.S. House",   "Brad Finstad",
  "MO",   "U.S. House",   "Ann Wagner",
  "MT",   "U.S. House",   "Ryan Zinke",
  "MT",   "U.S. House",   "Troy Downing",
  "NC",   "U.S. House",   "Greg Murphy",
  "NC",   "U.S. House",   "David Rouzer",
  "NC",   "U.S. House",   "Richard Hudson",
  "NC",   "U.S. House",   "Pat Harrigan",
  "NC",   "U.S. House",   "Tim Moore",
  "NE",   "U.S. House",   "Mike Flood",
  "NJ",   "U.S. House",   "Jeff Van Drew",
  "NJ",   "U.S. House",   "Thomas H. Kean Jr.",
  "NY",   "U.S. House",   "Nicholas J. LaLota",
  "NY",   "U.S. House",   "Mike Lawler",
  "NY",   "U.S. House",   "Nick Langworthy",
  "OH",   "U.S. House",   "Max Miller",
  "OH",   "U.S. House",   "Mike Turner",
  "OH",   "U.S. House",   "Mike Carey",
  "OK",   "U.S. House",   "Stephanie Bice",
  "PA",   "U.S. House",   "Brian Fitzpatrick",
  "PA",   "U.S. House",   "Ryan E. Mackenzie",
  "PA",   "U.S. House",   "Rob Bresnahan Jr.",
  "PA",   "U.S. House",   "Scott Perry",
  "SC",   "U.S. House",   "Russell Fry",
  "TN",   "U.S. House",   "Andy Ogles",
  "TX",   "U.S. House",   "Monica De La Cruz",
  "UT",   "U.S. House",   "Blake Moore",
  "UT",   "U.S. House",   "Celeste Maloy",
  "UT",   "U.S. House",   "Mike Kennedy",
  "VA",   "U.S. House",   "Robert Wittman",
  "VA",   "U.S. House",   "Jen Kiggans",
  "VA",   "U.S. House",   "John McGuire",
  "VA",   "U.S. House",   "Ben Cline",
  "WA",   "U.S. House",   "Michael Baumgartner",
  "WI",   "U.S. House",   "Bryan Steil",
  "WI",   "U.S. House",   "Derrick Van Orden",
  "WI",   "U.S. House",   "Glenn Grothman"
) |>
  mutate(incumbent = "yes")

state_vi <- state_vi |>
  left_join(rep_incumbents, by = c("state", "office_type", "rep_candidate")) |>
  mutate(incumbent = factor(coalesce(incumbent, "no"), levels = c("no", "yes")))

# --- 4. Merge polls + tariffs + 2024 presidential vote --------------
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
# Tariff exposure is by state for both chambers; the 2024 baseline is
# the state (Senate) or district (House) result.
merged <- state_vi |>
  left_join(state_lookup, by = "state") |>
  left_join(tariffs,   by = "state_name") |>
  left_join(canada_trade, by = "state") |>
  left_join(pres_baseline, by = c("chamber", "state", "seat_number")) |>
  # --- 4. Republican over/under-performance vs. 2024 --------------
  mutate(rep_diff = round(rep_2party - rep_2party_2024, 2))

write_csv(merged, here("Data", "polls_tariffs_pres2024.csv"))

message(sprintf("merged: %d polls, %d states | missing tariff: %d | missing Canada trade: %d | missing 2024: %d",
                nrow(merged), n_distinct(merged$state),
                sum(is.na(merged$tariffedPercent)),
                sum(is.na(merged$canada_share_total_trade)),
                sum(is.na(merged$rep_2party_2024))))

names(merged)

# --- 5. Model -------------------------------------------------------
post <- filter(merged, start_date > "2026-10-01")

# One model per chamber x exposure measure (name = which measure,
# value = its 0-100 score). state stays in data for clustered SEs.
# rep_2party_2024 controls for Trump's 2024 level: places he won big
# have more room to fall, so the shortfall alone would assume every
# point of 2024 support carries over one-for-one.
exposure_models <- post |>
 select(chamber, state, rep_diff, incumbent, rep_2party_2024, tariffedPercent, canada_share_total_trade) |>
 pivot_longer(cols=c(tariffedPercent,canada_share_total_trade)) |>
 nest(data=c(rep_diff, incumbent, rep_2party_2024, value, state)) |>
  mutate(model=map(data, ~lm(rep_diff~value+incumbent+rep_2party_2024, data=.x)))

# Four columns (Senate tariffs, Senate Canada, House tariffs, House
# Canada) under a two-column "Senate" header and a two-column "House"
# header. Column 1 is the coefficient labels, so the models are 2-5.
exposure_models <- exposure_models |>
  mutate(chamber = factor(chamber, levels = c("Senate", "House"))) |>
  arrange(chamber, name != "tariffedPercent")    # tariffs first in each chamber

exposure_tbl <- modelsummary(setNames(exposure_models$model,
                      paste0(recode(exposure_models$name,
                                    tariffedPercent          = "Counter-tariff exposure",
                                    canada_share_total_trade = "Canada share of trade"),
                             c("", " ")[as.integer(exposure_models$chamber)])),  # names must be unique
             stars=T, fmt=2, vcov = ~state,
             coef_rename = c("value"        = "Exposure (%)",
                             "incumbentyes" = "Republican incumbent",
                             "rep_2party_2024" = "Trump 2024 two-party share (%)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared")) |>
  group_tt(j = list("Senate" = 2:3, "House" = 4:5))

exposure_tbl
# Grouped headers are added after modelsummary(), so save the finished
# table rather than passing output= (which would save it without them).
save_tt(exposure_tbl, here("Output", "model_summary.html"), overwrite = TRUE)


