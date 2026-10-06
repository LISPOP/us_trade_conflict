# ------------------------------------------------------------------
# Scrape DISTRICT-level House candidate-matchup polls for the 2026
# U.S. House elections from Wikipedia (named Democrat vs named
# Republican, e.g. "Gabe Vasquez (D) vs Greg Cunningham (R)").
#
# This complements:
#   1_scrape_generic_ballot.R  -> district generic-ballot questions
#   2_scrape_senate_polls.R    -> state-level Senate matchups
#
# Together these give district- and state-level vote-intention polls
# that can be aggregated to the state level for the counter-tariff
# analysis.
#
# Method mirrors the Senate scraper: find individual-poll tables whose
# candidate columns carry "(D)" and "(R)" party labels; record the
# enclosing "District N" heading. Primary polls (same-party fields,
# no party labels) and generic-ballot tables ("Generic Democrat") are
# naturally excluded because they lack the (D)/(R) column labels.
#
# Source: en.wikipedia.org (CC BY-SA). Re-runnable; caches raw HTML.
# ------------------------------------------------------------------

suppressMessages({
  library(rvest); library(xml2); library(dplyr); library(stringr)
  library(purrr); library(readr); library(here)
})

UA        <- "us_trade_conflict research scraper (contact: sjkiss@gmail.com)"
CACHE_DIR <- here("Data", "raw_html")
OUT_CSV   <- here("Data", "house_district_polls_2026.csv")
POLITE_SLEEP <- 1
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

# --- Helpers (shared shape with the Senate scraper) -----------------
digest_url <- function(url) substr(gsub("[^a-z0-9]", "", tolower(basename(url))), 1, 50)

page_urls <- function(state) {
  s <- str_replace_all(state, " ", "_")
  base <- "https://en.wikipedia.org/wiki/2026_United_States_House_of_Representatives_"
  c(paste0(base, "elections_in_", s), paste0(base, "election_in_", s))
}

get_html <- function(state) {
  for (url in page_urls(state)) {
    cache <- file.path(CACHE_DIR, paste0(str_replace_all(state, " ", "_"),
                                         "__", digest_url(url), ".html"))
    if (file.exists(cache) && file.info(cache)$size > 0)
      return(list(pg = read_html(cache), url = url))
    resp <- tryCatch(httr::GET(url, httr::user_agent(UA), httr::timeout(30)),
                     error = function(e) NULL)
    if (!is.null(resp) && httr::status_code(resp) == 200) {
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      writeLines(txt, cache, useBytes = TRUE)
      Sys.sleep(POLITE_SLEEP)
      return(list(pg = read_html(txt), url = url))
    }
  }
  NULL
}

clean_candidate <- function(h) {
  h |> str_replace_all("\\[[^]]*\\]", "") |>
    str_replace("\\s*\\([A-Za-z]+\\)\\s*$", "") |> str_squish()
}
party_of  <- function(h) str_match(h, "\\(([A-Za-z]{1,3})\\)\\s*$")[, 2]
pct_to_num<- function(x) suppressWarnings(as.numeric(str_replace_all(x, "[^0-9.]", "")))

parse_sample <- function(x) {
  list(n = suppressWarnings(as.numeric(str_replace_all(str_extract(x, "[0-9,]+"), ",", ""))),
       pop = str_match(x, "\\(([A-Za-z]+)\\)")[, 2])
}
parse_end_date <- function(x) {
  x <- str_squish(str_replace_all(x, "–|—", "-"))
  m <- str_match(x, "([A-Za-z]+)\\s+([0-9]{1,2})\\s*-\\s*([0-9]{1,2}),?\\s*([0-9]{4})")
  if (!is.na(m[1, 1])) {
    d <- suppressWarnings(as.Date(paste(m[1,2], m[1,4], m[1,5]), format = "%B %d %Y"))
    if (!is.na(d)) return(d)
  }
  suppressWarnings(as.Date(str_extract(x, "[A-Za-z]+ [0-9]{1,2}, [0-9]{4}"), format = "%B %d, %Y"))
}

# Nearest enclosing "District N" heading (headings nest District > ... > Polling).
district_heading <- function(node) {
  hs <- xml_find_all(node, "preceding::h2|preceding::h3|preceding::h4")
  if (length(hs) == 0) return(NA_character_)
  txts <- str_squish(html_text2(hs))
  d <- which(str_detect(txts, regex("district\\s*[0-9]+|at.?large", ignore_case = TRUE)))
  if (length(d)) txts[max(d)] else txts[length(txts)]
}
district_num <- function(heading) {
  if (is.na(heading)) return(NA_character_)
  n <- str_match(heading, regex("district\\s*([0-9]+)", ignore_case = TRUE))[, 2]
  if (!is.na(n)) return(n)
  if (str_detect(heading, regex("at.?large", ignore_case = TRUE))) return("AL")
  NA_character_
}

is_poll_table <- function(nm)
  any(str_detect(tolower(nm), "poll source|pollster")) && any(str_detect(tolower(nm), "date"))

parse_matchup_table <- function(tb, state, url) {
  df <- tryCatch(html_table(tb, fill = TRUE), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  first_th <- tb |> html_elements("tr")
  first_th <- if (length(first_th)) html_elements(first_th[[1]], "th") else list()
  th_nm <- str_squish(str_replace_all(html_text2(first_th), "\\s+", " "))
  nm <- if (length(th_nm) == ncol(df)) th_nm
        else str_squish(str_replace_all(names(df), "\\s+", " "))
  if (!is_poll_table(nm)) return(NULL)

  parties <- party_of(nm); parties[parties == "DFL"] <- "D"
  d_idx <- which(parties == "D")[1]; r_idx <- which(parties == "R")[1]
  if (is.na(d_idx) || is.na(r_idx)) return(NULL)

  col <- function(pat) which(str_detect(tolower(nm), pat))[1]
  i_poll <- col("poll source|pollster"); i_date <- col("date")
  i_smp <- col("sample"); i_moe <- col("margin")
  i_und <- col("undecided"); i_oth <- which(str_detect(tolower(nm), "^other"))[1]
  pick <- function(i) if (is.na(i)) NA_character_ else df[[i]]

  heading <- district_heading(tb)
  tibble(
    state = state, district = district_num(heading), heading = heading,
    matchup = paste(clean_candidate(nm[d_idx]), "(D) vs",
                    clean_candidate(nm[r_idx]), "(R)"),
    dem_candidate = clean_candidate(nm[d_idx]),
    rep_candidate = clean_candidate(nm[r_idx]),
    pollster_raw = pick(i_poll), dates_raw = pick(i_date),
    sample_raw = pick(i_smp), moe_raw = pick(i_moe),
    dem_raw = df[[d_idx]], rep_raw = df[[r_idx]],
    other_raw = pick(i_oth), undecided_raw = pick(i_und),
    source_url = url
  )
}

scrape_state <- function(state) {
  res <- get_html(state)
  if (is.null(res)) { message(sprintf("  [%s] unreachable", state)); return(NULL) }
  tabs <- res$pg |> html_elements("table.wikitable")
  rows <- map_dfr(tabs, ~ parse_matchup_table(.x, state, res$url))
  message(sprintf("  [%s] %d district D-vs-R rows", state, nrow(rows)))
  rows
}

# --- Run ------------------------------------------------------------
message("Scraping ", length(states), " state House pages for district matchups ...")
raw <- map_dfr(states, scrape_state)
if (nrow(raw) == 0) stop("No district candidate-matchup poll rows found.")

# --- Clean ----------------------------------------------------------
clean <- raw |>
  mutate(
    pollster = str_squish(str_replace_all(pollster_raw, "\\[[^]]*\\]", "")),
    pollster_partisan = str_detect(pollster, "\\((R|D)\\)"),
    end_date = as.Date(map_dbl(dates_raw, ~ as.numeric(parse_end_date(.x))), origin = "1970-01-01"),
    dem_pct = pct_to_num(dem_raw), rep_pct = pct_to_num(rep_raw),
    other_pct = pct_to_num(other_raw), undecided_pct = pct_to_num(undecided_raw),
    net_dem = dem_pct - rep_pct, moe = pct_to_num(moe_raw),
    smp = map(sample_raw, parse_sample),
    sample_size = map_dbl(smp, "n"), sample_type = map_chr(smp, ~ .x$pop %||% NA_character_)
  ) |>
  filter(!is.na(dem_pct), !is.na(rep_pct)) |>
  filter(!str_detect(coalesce(pollster, ""),
                     regex("average|aggregate|rcp|^—$", ignore_case = TRUE))) |>
  distinct(state, district, pollster, end_date, matchup, dem_pct, rep_pct, .keep_all = TRUE) |>
  select(state, district, heading, matchup, dem_candidate, rep_candidate,
         pollster, pollster_partisan, dates = dates_raw, end_date,
         sample_size, sample_type, moe, dem_pct, rep_pct, other_pct,
         undecided_pct, net_dem, source_url) |>
  arrange(state, district, end_date)

write_csv(clean, OUT_CSV)
message(sprintf("\nWrote %d district poll rows across %d states -> %s",
                nrow(clean), n_distinct(clean$state), OUT_CSV))
