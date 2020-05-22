library(tidyverse)
library(lubridate)
library(janitor)
options(scipen = 999)

# run this to download with the latest data, when desired:
source("00_load_data.R")


# we'll start with the national weekly file 
raw_national_weekly <- read_csv("raw_data/SD_national_scaled_weekly.csv")

#format and clean names
natl_weekly <- raw_national_weekly %>% 
  clean_names() %>% 
  mutate(
    time = dmy_hms(time),
    time = date(time)
  )
