library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(here)
here::i_am("R_Scripts/master.R")   # root = project, not R_Scripts/

senate_polls <- read_excel(
  path = here("Data/senate_general_polls_2026.xlsx")
)

tariffed_imports <- read_excel(
  path = here("Data/tariffedImportsByState.xlsx")
) %>%
  rename(state = State) %>%
  mutate(state = str_trim(state))

senate_polls <- senate_polls %>%
  mutate(state = str_trim(state))

merged <- senate_polls %>%
  left_join(tariffed_imports, by = "state")

View(merged)
