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
)
