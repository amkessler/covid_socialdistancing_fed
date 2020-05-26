library(tidyverse)
library(lubridate)
library(janitor)
options(scipen = 999)

# run this to download with the latest data, when desired:
source("00_load_data.R")


# we'll start with the national weekly file - note that week of april 11 is the peak the SDI index measured against
raw_national_weekly <- read_csv("raw_data/SD_national_scaled_weekly.csv")

#format and clean names
natl_weekly <- raw_national_weekly %>% 
  clean_names() %>% 
  rename(sdindex = s_dindex) %>% #clean_names struggled with this one, so manual fix
  mutate(
    time = dmy_hms(time),
    time = date(time)
  )

natl_weekly

#plot
natl_weekly %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_line() +
  theme_minimal()


natl_weekly %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_col() +
  theme_minimal()


#highlighting the peak 
mycolors <- c("gray75", "#E83536")


natl_weekly %>% 
  mutate(highlight = sdindex == max(sdindex),
         highlight = as_factor(highlight)) %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_col(aes(fill = highlight),
           alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual(values = mycolors) +
  labs(title = "Social Distancing Index - National Weekly") +
  guides(fill = "none")





### State level weekly data #####

raw_states_weekly <- read_csv("raw_data/SD_states_scaled_weekly.csv")

states_weekly <- raw_states_weekly %>% 
  clean_names() %>% 
  mutate(
    time = dmy_hms(time),
    time = date(time)
  )

states_weekly
