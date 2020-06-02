library(tidyverse)
library(lubridate)
library(janitor)
library(tidycensus)
library(gghighlight)
library(tibbletime)
options(scipen = 999)

# run this to download with the latest data, when desired:
# source("00_load_data.R")


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
  labs(title = "Federal Reserve Social Distancing Index - National Weekly") +
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
  
head(states_weekly_tidy)


#line chart from wide table
states_weekly %>% 
  ggplot(aes(x = time,
             y = az)) +
  geom_line() +
  theme_minimal()


#line chart from long table
states_weekly_tidy %>% 
  filter(state == "AZ") %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_line() +
  theme_minimal()


#faceted for all states
states_weekly_tidy %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~state)


#faceted for all states with custom start date
states_weekly_tidy %>% 
  filter(time >= "2020-03-01") %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~state)


#faceted for selected states only
states_weekly_tidy %>% 
  filter(
    time >= "2020-03-01",
    state %in% c("NY", "NJ", "TN", "KY", "NV", "AZ", "FL", "GA")
    ) %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~state)


#all states on one line chart, with highlighted choice(s)
states_weekly_tidy %>% 
  filter(time >= "2020-02-01") %>% 
  ggplot(aes(x = time,
             y = sdindex,
             group = state)) +
  geom_line() +
  theme_minimal() +
  gghighlight(state == "NY", label_key = state)


#dual highlights
states_weekly_tidy %>% 
  filter(time >= "2020-02-01") %>% 
  ggplot(aes(x = time,
             y = sdindex,
             color = state)) +
  geom_line() +
  theme_minimal() +
  gghighlight(state %in% c("NY", "AZ"), label_key = state)



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




### County-level weekly data #### ----------------------------------------------------------------


raw_counties_weekly <- read_csv("raw_data/SD_counties_scaled_weekly.csv",
                                skip = 1)


counties_weekly <- raw_counties_weekly %>% 
  rename(
    time = X1,
  )

colnames(counties_weekly)

#format date
counties_weekly <- counties_weekly %>% 
  mutate(
    time = dmy_hms(time),
    time = date(time)
  )

#convert to long/tidy format
counties_weekly_tidy <- counties_weekly %>% 
  pivot_longer(-time, names_to = "fips", values_to = "sdindex")

#remove the FIPS prefix from the fips codes
counties_weekly_tidy <- counties_weekly_tidy %>% 
  mutate(
    fips = str_remove(fips, "FIPS_")
  )

counties_weekly_tidy

#fips code table from tidycensus
fips_lookup <- fips_codes %>% 
  mutate(
    fips = paste0(state_code, county_code)
  ) %>% 
  select(-state_code, -county_code)

#join
counties_weekly_tidy <- inner_join(counties_weekly_tidy, fips_lookup) %>% 
  select(time, fips, state, county, sdindex)

counties_weekly_tidy


#faceted line chart for counties in a specific state, with custom start date
counties_weekly_tidy %>% 
  filter(time >= "2020-03-01",
         state == "NJ") %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~county)




############## DAILY DATA #################################


### County-level DAILY data #### ----------------------------------------------------------------


raw_counties_daily <- read_csv("raw_data/SD_counties_scaled.csv",
                                skip = 1)



counties_daily <- raw_counties_daily %>% 
  rename(
    time = X1,
  )

colnames(counties_daily)

head(counties_daily)

#format date
counties_daily <- counties_daily %>% 
  mutate(
    time = dmy(time)
  )

#convert to long/tidy format
counties_daily_tidy <- counties_daily %>% 
  pivot_longer(-time, names_to = "fips", values_to = "sdindex")

#remove the FIPS prefix from the fips codes
counties_daily_tidy <- counties_daily_tidy %>% 
  mutate(
    fips = str_remove(fips, "FIPS_")
  )

counties_daily_tidy

#fips code table from tidycensus
fips_lookup <- fips_codes %>% 
  mutate(
    fips = paste0(state_code, county_code)
  ) %>% 
  select(-state_code, -county_code)

#join
counties_daily_tidy <- inner_join(counties_daily_tidy, fips_lookup) %>% 
  select(time, fips, state, county, sdindex)

counties_daily_tidy


#faceted line chart for counties in a specific state, with custom start date
counties_daily_tidy %>% 
  filter(time >= "2020-02-01",
         state == "NJ") %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_line(color = "gray") +
  geom_smooth() +
  theme_minimal() +
  facet_wrap(~county)


#calculating a seven-day moving average
counties_daily_tidy

#use tibbletime package to create rolling avg
# https://cran.rstudio.com/web/packages/tibbletime/vignettes/TT-03-rollify-for-rolling-analysis.html

counties_daily_tidy %>% 
  as_tbl_time(index = time)


rolling_mean <- tibbletime::rollify(mean, window = 7)

#apply it
counties_daily_tidy <- counties_daily_tidy %>% 
  mutate(
    roll_avg = rolling_mean(sdindex)
  )

counties_daily_tidy


#faceted line chart for counties in a specific state, with custom start date
counties_daily_tidy %>% 
  filter(time >= "2020-02-01",
         state == "NJ") %>% 
  ggplot(aes(x = time,
             y = roll_avg)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~county)


counties_daily_tidy %>% 
  filter(time >= "2020-02-01",
         state == "NJ",
         county == "Atlantic County") %>% 
  ggplot(aes(x = time,
             y = roll_avg)) +
  geom_line() +
  theme_minimal() 


counties_daily_tidy %>% 
  filter(time >= "2020-02-01",
         state == "NJ",
         county == "Atlantic County") %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_line(color = "gray") +
  geom_smooth() +
  theme_minimal() 





counties_daily_tidy %>% 
  filter(time >= "2020-02-01",
         state == "NJ",
         county == "Atlantic County") %>% 
  View()


#MN?
counties_daily_tidy %>% 
  filter(time >= "2020-02-01",
         state == "MN",
         county == "Hennepin County") %>% 
  ggplot(aes(x = time,
             y = sdindex)) +
  geom_line(color = "gray") +
  geom_smooth() +
  theme_minimal() 
