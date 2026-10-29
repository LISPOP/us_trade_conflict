library(tidyverse)
# ------------------------------------------------------------------
# Ingest the New York Times poll database (the 538-style feed NYT now
# hosts) and reshape it into tidy Republican-vs-opponent matchups.
#
# WHY THIS SOURCE
#   NYT's feed is far more complete and current than Wikipedia's hand-
#   curated tables — it includes recent polls (e.g. CA-40 early Sept
#   2026) that Wikipedia never listed. It supersedes the Wikipedia
#   scrapers (scripts 1-3) as the authoritative poll source.
#
# ENDPOINTS (public CSVs; the HTML tracker is bot-blocked, these are not)
#   https://www.nytimes.com/newsgraphics/polls/house.csv
#   https://www.nytimes.com/newsgraphics/polls/senate.csv
#   https://www.nytimes.com/newsgraphics/polls/governor.csv
#
# OUTPUT (one row per poll question = one R-vs-opponent matchup)
#   Data/nyt_house_polls_2026.csv    (district-level)
#   Data/nyt_senate_polls_2026.csv   (state-level)
#   Data/nyt_governor_polls_2026.csv (state-level)
#   Data/nyt_state_vote_intention_2026.csv  (Senate+Governor stacked,
#                                             state-level, for the
#                                             counter-tariff analysis)
#
# The raw CSVs are cached under Data/nyt_raw/ (git-ignored).
# ------------------------------------------------------------------

suppressMessages({
  library(readr); library(dplyr); library(stringr)
  library(lubridate); library(here)
})
here::i_am("R_Scripts/2_ingest_nyt_polls.R")   # root = project, not R_Scripts/

UA      <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
RAW_DIR <- here("Data", "nyt_raw")
BASE    <- "https://www.nytimes.com/newsgraphics/polls/"
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

# --- Download (with UA; the CSV endpoints are not bot-blocked) -------
fetch_csv <- function(office) {
  dest <- file.path(RAW_DIR, paste0(office, ".csv"))
  ok <- tryCatch({
    resp <- httr::GET(paste0(BASE, office, ".csv"),
                      httr::user_agent(UA), httr::timeout(60),
                      httr::write_disk(dest, overwrite = TRUE))
    httr::status_code(resp) == 200
  }, error = function(e) FALSE)
  if (!ok || !file.exists(dest) || file.info(dest)$size < 1000)
    stop("Failed to download ", office, ".csv from NYT")
  message(sprintf("  downloaded %s.csv (%s)", office,
                  format(structure(file.info(dest)$size, class = "object_size"),
                         units = "auto")))
  read_csv(dest, show_col_types = FALSE, col_types = cols(.default = "c"))
}

# --- Reshape long -> wide R-vs-opponent matchups --------------------
# The feed is long: one row per candidate per poll question. For each
# (poll_id, question_id) we take the leading Republican and the leading
# non-Republican ("opponent"), whatever their party. In races where
# Democrats back an independent (AK-AL Hill, MT Bodnar, SD Bengs, ID
# Achilles) the independent is the real opponent, so pairing with the
# leading Democrat would compare against a minor candidate. Front-
# runner of each side also handles CA/WA top-two fields.
#   opp_party  party of the opponent (DEM / IND / ...), for robustness
#   three_way  runner-up non-Republican polls >= 15%, i.e. a viable
#              Democrat and a viable independent both run (MI Gov)
#
# Ranked-choice polls (AK, ME) report several rounds of the same poll
# as separate questions. Only the final round is kept: it is the
# head-to-head closest to a two-party result, and keeping every round
# would count one poll several times.
THREE_WAY_PCT <- 15

tidy_matchups <- function(df) {
  df <- df |>
    filter(cycle == "2026") |>
    mutate(
      start_date  = mdy(start_date),
      end_date    = mdy(end_date),
      pct         = suppressWarnings(as.numeric(pct)),
      sample_size = suppressWarnings(as.numeric(sample_size)),
      rcv_round   = suppressWarnings(as.numeric(ranked_choice_round))
    ) |>
    group_by(poll_id, state, seat_number) |>
    filter(is.na(rcv_round) |
           rcv_round == suppressWarnings(max(rcv_round, na.rm = TRUE))) |>
    ungroup()

  meta <- df |>
    group_by(poll_id, question_id) |>
    summarise(
      state       = first(state),
      seat_number = first(seat_number),
      seat_name   = first(seat_name),
      office_type = first(office_type),
      pollster    = first(pollster),
      sponsors    = first(sponsors),
      partisan    = first(partisan),
      population  = first(population),
      sample_size = first(sample_size),
      start_date  = first(start_date),
      end_date    = first(end_date),
      hypothetical= first(hypothetical),
      stage       = first(stage),
      rcv_round   = first(rcv_round),
      url         = first(url),
      .groups = "drop"
    )

  rep <- df |>
    filter(party == "REP", !is.na(pct)) |>
    group_by(poll_id, question_id) |>
    slice_max(pct, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(poll_id, question_id, rep_candidate = candidate_name, rep_pct = pct)

  # Non-Republicans ranked within each question; "NONE" is undecided /
  # someone else / neither, not a candidate.
  non_rep <- df |>
    filter(!party %in% c("REP", "NONE"), !is.na(pct)) |>
    group_by(poll_id, question_id) |>
    arrange(desc(pct), .by_group = TRUE)

  opp <- non_rep |>
    summarise(
      opp_candidate = first(candidate_name),
      opp_party     = first(party),
      opp_pct       = first(pct),
      three_way     = coalesce(nth(pct, 2), 0) >= THREE_WAY_PCT,
      .groups = "drop"
    )

  meta |>
    inner_join(opp, by = c("poll_id", "question_id")) |>
    inner_join(rep, by = c("poll_id", "question_id")) |>
    mutate(
      net_opp  = opp_pct - rep_pct,
      partisan = na_if(partisan, ""),
      is_partisan = !is.na(partisan)
    ) |>
    arrange(state, seat_number, end_date)
}

# --- Run ------------------------------------------------------------
message("Downloading NYT poll CSVs ...")
house_raw <- fetch_csv("house")
sen_raw   <- fetch_csv("senate")
gov_raw   <- fetch_csv("governor")

house <- tidy_matchups(house_raw)
sen   <- tidy_matchups(sen_raw)
gov   <- tidy_matchups(gov_raw)

write_csv(house, here("Data", "nyt_house_polls_2026.csv"))
write_csv(sen,   here("Data", "nyt_senate_polls_2026.csv"))
write_csv(gov,   here("Data", "nyt_governor_polls_2026.csv"))

# State-level vote intention = statewide contests (Senate + Governor).
state_vi <- bind_rows(sen, gov) |>
  filter(!is.na(state), state != "US") |>
  arrange(state, end_date)
write_csv(state_vi, here("Data", "nyt_state_vote_intention_2026.csv"))
state_vi
# --- Report ---------------------------------------------------------
report <- function(nm, d) {
  message(sprintf("%-9s %4d matchups | %2d states/seats | latest %s | last 30d: %d",
                  nm, nrow(d), n_distinct(d$state),
                  format(max(d$end_date, na.rm = TRUE)),
                  sum(d$end_date >= max(d$end_date, na.rm = TRUE) - 30, na.rm = TRUE)))
}
message("\n--- tidy matchup counts ---")
report("house",  house)
report("senate", sen)
report("gov",    gov)
report("state",  state_vi)
