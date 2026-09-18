# ------------------------------------------------------------------
# Scrape state-level (district-level) generic congressional ballot
# polls for the 2026 U.S. House elections from Wikipedia.
#
# WHAT THIS DOES
#   - Visits the per-state "2026 United States House of Representatives
#     elections in <State>" Wikipedia page for all 50 states.
#   - Finds every polling table that asks the GENERIC ballot question
#     (columns "Generic Democrat" / "Generic Republican").
#   - Records the section heading each table sits under, so district-
#     level vs statewide generic-ballot questions can be told apart.
#   - Cleans the rows into a tidy data frame and writes a CSV.
#
# IMPORTANT DATA CAVEAT
#   True *statewide* generic-ballot polls are rare. On Wikipedia these
#   tables almost always appear inside a single congressional DISTRICT
#   section, so the geography is usually one district, not the state.
#   The `geography` and `district` columns record which it is; do not
#   treat every row as a statewide reading.
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
OUT_CSV   <- here("Data", "generic_ballot_state_polls_2026.csv")
POLITE_SLEEP <- 1          # seconds between live page fetches
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

states <- c(
  "Alabama","Alaska","Arizona","Arkansas","California","Colorado",
  "Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois",
  "Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland",
  "Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana",
  "Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York",
  "North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
  "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
  "Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"
)

# --- Helpers --------------------------------------------------------

# Build candidate URLs. Most states use "elections" (plural); single-
# district (at-large) states use "election" (singular). Try both.
page_urls <- function(state) {
  s <- str_replace_all(state, " ", "_")
  base <- "https://en.wikipedia.org/wiki/2026_United_States_House_of_Representatives_"
  c(paste0(base, "elections_in_", s),
    paste0(base, "election_in_",  s))
}

# Fetch a page's HTML, caching to disk so re-runs don't re-hit Wikipedia.
get_html <- function(url, state) {
  cache <- file.path(CACHE_DIR, paste0(str_replace_all(state, " ", "_"),
                                       "__", digest_url(url), ".html"))
  if (file.exists(cache) && file.info(cache)$size > 0) {
    return(read_html(cache))
  }
  resp <- tryCatch(
    httr::GET(url, httr::user_agent(UA), httr::timeout(30)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  writeLines(txt, cache, useBytes = TRUE)
  Sys.sleep(POLITE_SLEEP)
  read_html(txt)
}

# Short stable hash of a URL for cache filenames (avoids extra deps).
digest_url <- function(url) {
  substr(gsub("[^a-z0-9]", "", tolower(basename(url))), 1, 40)
}

# Is this wikitable a generic-ballot poll table?
is_generic_table <- function(node) {
  hdr <- node |> html_elements("th") |> html_text2() |> paste(collapse = " ")
  str_detect(hdr, regex("generic\\s*democrat", ignore_case = TRUE)) &&
    str_detect(hdr, regex("generic\\s*republican", ignore_case = TRUE))
}

# States with a single at-large district: any generic-ballot poll there
# is effectively statewide.
AT_LARGE_STATES <- c("Alaska","Delaware","North Dakota","South Dakota",
                     "Vermont","Wyoming")

# The district/section context of a table: scan ALL preceding headings
# and take the nearest one naming a district (headings nest as
# "District N" > "General election" > "Polling", so the immediate
# heading is usually "Polling" — we want the enclosing district).
nearest_heading <- function(node) {
  hs <- xml_find_all(node, "preceding::h2|preceding::h3|preceding::h4")
  if (length(hs) == 0) return(NA_character_)
  txts <- str_squish(html_text2(hs))                 # document order
  dist <- which(str_detect(txts, regex("district\\s*[0-9]+|at.?large",
                                       ignore_case = TRUE)))
  if (length(dist) > 0) return(txts[max(dist)])       # nearest district heading
  txts[length(txts)]                                  # else nearest heading
}

# Classify geography from the section heading + state.
classify_geo <- function(heading, state) {
  if (state %in% AT_LARGE_STATES)
    return(list(geography = "at-large (statewide)", district = "AL"))
  if (is.na(heading)) return(list(geography = "unknown", district = NA_character_))
  d <- str_match(heading, regex("district\\s*([0-9]+)", ignore_case = TRUE))[, 2]
  if (!is.na(d)) return(list(geography = "district", district = d))
  if (str_detect(heading, regex("at.?large", ignore_case = TRUE)))
    return(list(geography = "at-large (statewide)", district = "AL"))
  if (str_detect(heading, regex("statewide|overall", ignore_case = TRUE)))
    return(list(geography = "statewide", district = NA_character_))
  list(geography = "unknown", district = NA_character_)
}

# Standardise the varied header names into fixed field names.
rename_poll_cols <- function(df) {
  nm <- names(df)
  key <- tolower(str_squish(nm))
  map_to <- function(pattern) which(str_detect(key, pattern))[1]
  idx <- list(
    pollster    = map_to("poll source|pollster"),
    dates       = map_to("date"),
    sample_size = map_to("sample"),
    moe         = map_to("margin"),
    dem         = map_to("generic ?democrat"),
    rep         = map_to("generic ?republican"),
    undecided   = map_to("undecided|other")
  )
  pick <- function(i) if (is.na(i)) NA_character_ else df[[i]]
  tibble(
    pollster_raw = pick(idx$pollster),
    dates_raw    = pick(idx$dates),
    sample_raw   = pick(idx$sample_size),
    moe_raw      = pick(idx$moe),
    dem_raw      = pick(idx$dem),
    rep_raw      = pick(idx$rep),
    undecided_raw= pick(idx$undecided)
  )
}

pct_to_num <- function(x) {
  suppressWarnings(as.numeric(str_replace_all(x, "[^0-9.]", "")))
}

# Split sample size like "879 (LV)" into number + population type.
parse_sample <- function(x) {
  n   <- suppressWarnings(as.numeric(str_replace_all(str_extract(x, "[0-9,]+"), ",", "")))
  pop <- str_match(x, "\\(([A-Za-z]+)\\)")[, 2]
  list(n = n, pop = pop)
}

# Extract the end date from a "April 26-28, 2026" style range.
parse_end_date <- function(x) {
  x <- str_squish(str_replace_all(x, "–|—", "-"))  # en/em dash -> hyphen
  yr <- str_extract(x, "\\b(19|20)[0-9]{2}\\b")
  # try full "Month D, YYYY" at end first
  m  <- str_match(x, "([A-Za-z]+)\\s+([0-9]{1,2})\\s*-\\s*([0-9]{1,2}),?\\s*([0-9]{4})")
  if (!is.na(m[1, 1])) {
    d <- suppressWarnings(as.Date(paste(m[1,2], m[1,4], m[1,5]), format = "%B %d %Y"))
    if (!is.na(d)) return(d)
  }
  d <- suppressWarnings(as.Date(x, format = "%B %d, %Y"))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(str_extract(x, "[A-Za-z]+ [0-9]{1,2}, [0-9]{4}"),
                                format = "%B %d, %Y"))
  d
}

# --- Scrape one state ----------------------------------------------
scrape_state <- function(state) {
  urls <- page_urls(state)
  pg <- NULL; used_url <- NA_character_
  for (u in urls) {
    pg <- get_html(u, state)
    if (!is.null(pg)) { used_url <- u; break }
  }
  if (is.null(pg)) {
    message(sprintf("  [%s] no page reachable", state)); return(NULL)
  }

  tabs <- pg |> html_elements("table.wikitable")
  gen  <- keep(tabs, is_generic_table)
  if (length(gen) == 0) {
    message(sprintf("  [%s] 0 generic-ballot tables", state)); return(NULL)
  }

  rows <- map_dfr(gen, function(tb) {
    df  <- tryCatch(html_table(tb, fill = TRUE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    geo <- classify_geo(nearest_heading(tb), state)
    out <- rename_poll_cols(df)
    out |>
      mutate(
        state       = state,
        geography   = geo$geography,
        district    = geo$district,
        source_url  = used_url,
        heading     = nearest_heading(tb),
        .before = 1
      )
  })

  message(sprintf("  [%s] %d generic table(s), %d rows",
                  state, length(gen), nrow(rows)))
  rows
}

# --- Run ------------------------------------------------------------
message("Scraping ", length(states), " state House pages ...")
raw <- map_dfr(states, scrape_state)

if (nrow(raw) == 0) stop("No generic-ballot poll rows found.")

# --- Clean ----------------------------------------------------------
clean <- raw |>
  filter(!is.na(dem_raw) | !is.na(rep_raw)) |>
  # drop the aggregate/average summary rows some tables include
  filter(!str_detect(coalesce(pollster_raw, ""),
                     regex("average|aggregate|rcp", ignore_case = TRUE))) |>
  mutate(
    pollster  = str_squish(str_replace_all(pollster_raw, "\\[[^]]*\\]", "")),
    pollster_partisan = str_detect(pollster, "\\((R|D)\\)"),
    end_date  = as.Date(map_dbl(dates_raw, ~ as.numeric(parse_end_date(.x))),
                        origin = "1970-01-01"),
    dem_pct   = pct_to_num(dem_raw),
    rep_pct   = pct_to_num(rep_raw),
    undecided_pct = pct_to_num(undecided_raw),
    net_dem   = dem_pct - rep_pct,
    moe       = pct_to_num(moe_raw)
  ) |>
  mutate(
    smp = map(sample_raw, parse_sample),
    sample_size  = map_dbl(smp, "n"),
    sample_type  = map_chr(smp, ~ .x$pop %||% NA_character_)
  ) |>
  select(
    state, geography, district, heading,
    pollster, pollster_partisan,
    dates = dates_raw, end_date,
    sample_size, sample_type, moe,
    dem_pct, rep_pct, undecided_pct, net_dem,
    source_url
  ) |>
  arrange(state, district, end_date)

write_csv(clean, OUT_CSV)
message(sprintf("\nWrote %d rows across %d states -> %s",
                nrow(clean), n_distinct(clean$state), OUT_CSV))
print(dplyr::count(clean, geography))
