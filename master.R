library(here)
library(readr)

install.packages(c("here", "readr"))

raw <- read_csv(here("data/senate_general_polls_2026.csv"))

glimpse()