# ------------------------------------------------------------------
# Scrape state-level general-election polls for the 2026 U.S. Senate
# elections from Wikipedia.
#
# WHAT THIS DOES
#   - Visits each 2026 Senate race page (33 Class II states + the two
#     special elections in Florida and Ohio).
#   - Finds every INDIVIDUAL-poll table (column "Poll source") that is a
#     Democrat-vs-Republican GENERAL-election matchup, identified by the
#     "(D)" and "(R)" party labels in the candidate column headers.
#   - Records the specific matchup (which D vs which R), since each
#     hypothetical pairing gets its own table.
#   - Cleans rows into a tidy data frame and writes a CSV.
#
# WHAT IT DEliberately SKIPS
#   - Primary polls (several same-party candidates, no party labels).
#   - Aggregator/average tables ("Source of poll aggregation").
#
# Senate races are statewide, so every row here is a true STATE-level
# reading (unlike the House generic-ballot data, which is district-level).
#
# Source: en.wikipedia.org (CC BY-SA). Re-runnable; caches raw HTML.
# ------------------------------------------------------------------

suppressMessages({
  library(rvest)
  library(xml2)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(here)
})

# --- Config ---------------------------------------------------------
UA        <- "us_trade_conflict research scraper (contact: sjkiss@gmail.com)"
CACHE_DIR <- here("Data", "raw_html")
OUT_CSV   <- here("Data", "senate_general_polls_2026.csv")
POLITE_SLEEP <- 1
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# 33 Class II states with a regular 2026 Senate election.
class2_states <- c(
  "Alabama","Alaska","Arkansas","Colorado","Delaware","Georgia","Idaho",
  "Illinois","Iowa","Kansas","Kentucky","Louisiana","Maine","Massachusetts",
  "Michigan","Minnesota","Mississippi","Montana","Nebraska","New Hampshire",
  "New Jersey","New Mexico","North Carolina","Oklahoma","Oregon",
  "Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
  "Virginia","West Virginia","Wyoming"
)
# Two special elections in 2026.
special_states <- c("Florida","Ohio")

# race registry: state, kind, and the Wikipedia title to try
races <- bind_rows(
  tibble(state = class2_states,   kind = "regular",
         title = paste0("2026_United_States_Senate_election_in_",
                        str_replace_all(class2_states, " ", "_"))),
  tibble(state = special_states,  kind = "special",
         title = paste0("2026_United_States_Senate_special_election_in_",
                        str_replace_all(special_states, " ", "_")))
)

# --- Helpers --------------------------------------------------------
digest_url <- function(url) substr(gsub("[^a-z0-9]", "", tolower(basename(url))), 1, 50)

get_html <- function(title, state) {
  url   <- paste0("https://en.wikipedia.org/wiki/", title)
  cache <- file.path(CACHE_DIR, paste0("senate_", digest_url(url), ".html"))
  if (file.exists(cache) && file.info(cache)$size > 0)
    return(list(pg = read_html(cache), url = url))
  resp <- tryCatch(httr::GET(url, httr::user_agent(UA), httr::timeout(30)),
                   error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  writeLines(txt, cache, useBytes = TRUE)
  Sys.sleep(POLITE_SLEEP)
  list(pg = read_html(txt), url = url)
}

# normalise a header vector (collapse newlines, squish)
norm_hdr <- function(nm) str_squish(str_replace_all(nm, "\\s+", " "))

# strip footnote markers like [g] and trailing party paren
clean_candidate <- function(h) {
  h |>
    str_replace_all("\\[[^]]*\\]", "") |>
    str_replace("\\s*\\([A-Za-z]+\\)\\s*$", "") |>
    str_squish()
}

party_of <- function(h) str_match(h, "\\(([A-Za-z]{1,3})\\)\\s*$")[, 2]

pct_to_num <- function(x) suppressWarnings(as.numeric(str_replace_all(x, "[^0-9.]", "")))

parse_sample <- function(x) {
  n   <- suppressWarnings(as.numeric(str_replace_all(str_extract(x, "[0-9,]+"), ",", "")))
  pop <- str_match(x, "\\(([A-Za-z]+)\\)")[, 2]
  list(n = n, pop = pop)
}

parse_end_date <- function(x) {
  x <- str_squish(str_replace_all(x, "–|—", "-"))
  m <- str_match(x, "([A-Za-z]+)\\s+([0-9]{1,2})\\s*-\\s*([0-9]{1,2}),?\\s*([0-9]{4})")
  if (!is.na(m[1, 1])) {
    d <- suppressWarnings(as.Date(paste(m[1,2], m[1,4], m[1,5]), format = "%B %d %Y"))
    if (!is.na(d)) return(d)
  }
  d <- suppressWarnings(as.Date(str_extract(x, "[A-Za-z]+ [0-9]{1,2}, [0-9]{4}"),
                                format = "%B %d, %Y"))
  d
}

# Is this an INDIVIDUAL-poll table (not an aggregator)?
is_poll_table <- function(nm) {
  any(str_detect(tolower(nm), "poll source")) &&
    any(str_detect(tolower(nm), "date"))
}

# Parse one general-election D-vs-R table into tidy rows (or NULL).
parse_general_table <- function(tb, state, kind, url, heading) {
  df <- tryCatch(html_table(tb, fill = TRUE), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  # html_table() collapses the <th> line breaks (e.g. "Jon\nOssoff" ->
  # "JonOssoff", "Margin\nof error" -> "Marginof error"), which loses word
  # spaces. Recover proper header names from the first header row's <th>
  # cells via html_text2(), falling back to html_table names if the column
  # count doesn't line up (multi-row / colspan headers).
  first_th <- tb |> html_elements("tr")
  first_th <- if (length(first_th)) html_elements(first_th[[1]], "th") else list()
  th_nm    <- str_squish(str_replace_all(html_text2(first_th), "\\s+", " "))
  nm <- if (length(th_nm) == ncol(df)) th_nm else norm_hdr(names(df))
  if (!is_poll_table(nm)) return(NULL)

  parties <- party_of(nm)
  parties[parties == "DFL"] <- "D"   # Minnesota Democrats run as DFL
  d_idx <- which(parties == "D")[1]
  r_idx <- which(parties == "R")[1]
  if (is.na(d_idx) || is.na(r_idx)) return(NULL)   # not a D-vs-R general matchup

  col <- function(pat) which(str_detect(tolower(nm), pat))[1]
  i_poll <- col("poll source")
  i_date <- col("date")
  i_smp  <- col("sample")
  i_moe  <- col("margin")   # only "Margin of error" in an individual-poll table
  i_und  <- col("undecided")
  i_oth  <- which(str_detect(tolower(nm), "^other"))[1]

  pick <- function(i) if (is.na(i)) NA_character_ else df[[i]]

  out <- tibble(
    state        = state,
    race_kind    = kind,
    matchup      = paste(clean_candidate(nm[d_idx]), "(D) vs",
                         clean_candidate(nm[r_idx]), "(R)"),
    dem_candidate = clean_candidate(nm[d_idx]),
    rep_candidate = clean_candidate(nm[r_idx]),
    pollster_raw = pick(i_poll),
    dates_raw    = pick(i_date),
    sample_raw   = pick(i_smp),
    moe_raw      = pick(i_moe),
    dem_raw      = df[[d_idx]],
    rep_raw      = df[[r_idx]],
    other_raw    = pick(i_oth),
    undecided_raw= pick(i_und),
    heading      = heading,
    source_url   = url
  )
  out
}

# Nearest preceding heading (section context, e.g. "General election").
nearest_heading <- function(node) {
  hs <- xml_find_all(node, "preceding::h2|preceding::h3|preceding::h4")
  if (length(hs) == 0) return(NA_character_)
  str_squish(html_text2(hs[[length(hs)]]))
}

# --- Scrape one race ------------------------------------------------
scrape_race <- function(state, kind, title) {
  res <- get_html(title, state)
  if (is.null(res)) { message(sprintf("  [%s/%s] page unreachable", state, kind)); return(NULL) }
  tabs <- res$pg |> html_elements("table.wikitable")
  rows <- map_dfr(tabs, function(tb)
    parse_general_table(tb, state, kind, res$url, nearest_heading(tb)))
  message(sprintf("  [%s/%s] %d D-vs-R poll rows", state, kind, nrow(rows)))
  rows
}

# --- Run ------------------------------------------------------------
message("Scraping ", nrow(races), " Senate race pages ...")
raw <- pmap_dfr(races, function(state, kind, title) scrape_race(state, kind, title))

if (nrow(raw) == 0) stop("No Senate general-election poll rows found.")

# --- Clean ----------------------------------------------------------
clean <- raw |>
  mutate(
    pollster  = str_squish(str_replace_all(pollster_raw, "\\[[^]]*\\]", "")),
    pollster_partisan = str_detect(pollster, "\\((R|D)\\)"),
    end_date  = as.Date(map_dbl(dates_raw, ~ as.numeric(parse_end_date(.x))),
                        origin = "1970-01-01"),
    dem_pct   = pct_to_num(dem_raw),
    rep_pct   = pct_to_num(rep_raw),
    other_pct = pct_to_num(other_raw),
    undecided_pct = pct_to_num(undecided_raw),
    net_dem   = dem_pct - rep_pct,
    moe       = pct_to_num(moe_raw),
    smp        = map(sample_raw, parse_sample),
    sample_size = map_dbl(smp, "n"),
    sample_type = map_chr(smp, ~ .x$pop %||% NA_character_)
  ) |>
  # keep real poll rows only: need both candidate percentages
  filter(!is.na(dem_pct), !is.na(rep_pct)) |>
  # drop embedded aggregate/average rows
  filter(!str_detect(coalesce(pollster, ""),
                     regex("average|aggregate|rcp|^—$", ignore_case = TRUE))) |>
  distinct(state, pollster, end_date, matchup, dem_pct, rep_pct, .keep_all = TRUE) |>
  select(
    state, race_kind, matchup, dem_candidate, rep_candidate,
    pollster, pollster_partisan,
    dates = dates_raw, end_date,
    sample_size, sample_type, moe,
    dem_pct, rep_pct, other_pct, undecided_pct, net_dem,
    heading, source_url
  ) |>
  arrange(state, end_date)

write_csv(clean, OUT_CSV)
message(sprintf("\nWrote %d poll rows across %d states -> %s",
                nrow(clean), n_distinct(clean$state), OUT_CSV))
print(dplyr::count(clean, state, sort = TRUE), n = 50)
