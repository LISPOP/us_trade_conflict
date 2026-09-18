# ------------------------------------------------------------------
# Ingest the New York Times poll database (the 538-style feed NYT now
# hosts) and reshape it into tidy Democrat-vs-Republican matchups.
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
# OUTPUT (one row per poll question = one D-vs-R matchup)
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

# --- Reshape long -> wide D-vs-R matchups ---------------------------
# The feed is long: one row per candidate per poll question. For each
# (poll_id, question_id) we take the leading Democrat and leading
# Republican (handles CA/WA top-two fields with multiple same-party
# candidates by using the front-runner of each party).
tidy_matchups <- function(df) {
  df <- df |>
    filter(cycle == "2026") |>
    mutate(
      start_date  = mdy(start_date),
      end_date    = mdy(end_date),
      pct         = suppressWarnings(as.numeric(pct)),
      sample_size = suppressWarnings(as.numeric(sample_size))
    )

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
      url         = first(url),
      .groups = "drop"
    )

  lead_party <- function(p) {
    df |>
      filter(party == p, !is.na(pct)) |>
      group_by(poll_id, question_id) |>
      slice_max(pct, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(poll_id, question_id, candidate_name, pct)
  }
  dem <- lead_party("DEM") |> rename(dem_candidate = candidate_name, dem_pct = pct)
  rep <- lead_party("REP") |> rename(rep_candidate = candidate_name, rep_pct = pct)

  meta |>
    inner_join(dem, by = c("poll_id", "question_id")) |>
    inner_join(rep, by = c("poll_id", "question_id")) |>
    mutate(
      net_dem  = dem_pct - rep_pct,
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
