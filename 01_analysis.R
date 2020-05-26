library(tidyverse)
library(lubridate)
library(janitor)
library(tidycensus)
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





### State level weekly data ##### --------------------------------------------------------

raw_states_weekly <- read_csv("raw_data/SD_states_scaled_weekly.csv")

states_weekly <- raw_states_weekly %>% 
  clean_names() %>% 
  mutate(
    time = dmy_hms(time),
    time = date(time)
  )

head(states_weekly)

#convert to long/tidy format
states_weekly_tidy <- states_weekly %>% 
  pivot_longer(-time, names_to = "state", values_to = "sdindex")

#state name values to uppercase
states_weekly_tidy <- states_weekly_tidy %>% 
  mutate(
    state = str_to_upper(state)
  )
  



### MSA weekly data #### ----------------------------------------------------------------
  
raw_msa_weekly <- read_csv("raw_data/SD_msa_scaled_weekly.csv", 
                                 skip = 1)

msa_weekly <- raw_msa_weekly %>% 
  rename(
    time = X1,
    all_metros = All_MSAs,
    all_rural = All_Rural
  )

colnames(raw_msa_weekly)

#format date
msa_weekly <- msa_weekly %>% 
  mutate(
    time = dmy_hms(time),
    time = date(time)
  )

#convert to long/tidy format
msa_weekly_tidy <- msa_weekly %>% 
  pivot_longer(-time, names_to = "msa_fips", values_to = "sdindex")

#remove the MSA_prefix from the fips codes
msa_weekly_tidy <- msa_weekly_tidy %>% 
  mutate(
    msa_fips = str_remove(msa_fips, "MSA_")
  )

msa_weekly_tidy

msa_weekly_tidy %>% 
  filter(msa_fips == "all_metros")

msa_weekly_tidy %>% 
  filter(msa_fips == "10500")


#create fips lookup from top two rows of raw data
msa_fips <- read_csv("raw_data/SD_msa_scaled_weekly.csv") %>% 
  head(1)

msa_fips

