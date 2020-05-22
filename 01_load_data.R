# Source: Social Distancing Index, Federal Reserve Bank of Dallas
# https://www.dallasfed.org/research/sdi
# Methodology found here:
# https://www.dallasfed.org/research/~/media/documents/research/papers/2020/wp2014.pdf
# 
# The Dallas Fed Social Distancing Index summarizes the information in seven different variables based on 
# geolocation data collected from a large sample of mobile devices to gain insight into the economic impact 
# of the pandemic. (Uses data from Safegraph).


#### Download and unzip ####

# There are datasets at the national, state, county and MSA levels. Daily and weekly for each.
# Let's download the entire collection from a zip file available on the Dallas Fed site and unzip.

# url
url <- "https://www.dallasfed.org/~/media/documents/research/sdi/SD_data.zip"

# destination to save to
destfile <- "raw_data/SD_data.zip"

# perform the download
download.file(url, destfile)

# unzip it
unzip("raw_data/SD_data.zip", exdir = "raw_data")


# We now have all the separate csvs downloaded and ready to proceed.




