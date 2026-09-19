library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(here)

senate_polls <- read_excel(
  path = here("data/senate_general_polls_2026.xlsx")
)

tariffed_imports <- read_excel(
  path = here("data/tariffedImportsByState.xlsx")
) %>%
  rename(state = State) %>%
  mutate(state = str_trim(state))

senate_polls <- senate_polls %>%
  mutate(state = str_trim(state))

merged <- senate_polls %>%
  left_join(tariffed_imports, by = "state")

View(merged)