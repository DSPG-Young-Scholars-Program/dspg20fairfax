# start here:  lehd.ces.census.gov/data/lodes/LODES7/
# navigate to here: https://lehd.ces.census.gov/data/lodes/LODES7/va/od/

library(R.utils)
library(data.table)
library(tidyr)
library(dplyr)

# main table
# the main part includes jobs with both workplace and residence in the state 

download.file(
  url = "https://lehd.ces.census.gov/data/lodes/LODES7/va/od/va_od_main_JT00_2017.csv.gz", 
  destfile = "data/original/va_od_main_JT00_2017.csv.gz")

gunzip("data/original/va_od_main_JT00_2017.csv.gz")

# aux table
# the aux part includes jobs with the workplace in the state and the residence outside of the state

download.file(
  url = "https://lehd.ces.census.gov/data/lodes/LODES7/va/od/va_od_aux_JT00_2017.csv.gz", 
  destfile ="data/original/va_od_aux_JT00_2017.csv.gz")

gunzip("data/original/va_od_aux_JT00_2017.csv.gz")


aux <- fread("data/original/va_od_aux_JT00_2017.csv")
head(aux)

main <- fread("data/original/va_od_main_JT00_2017.csv")
head(main)

# first, append your aux to main by job type

data <- rbind(main, aux)

# second, filter only to rows where a Fairfax County block group is either the origin or in the destination

fairfax <- data %>% 
  filter(w_geocode %in% data$w_geocode[substr(data$w_geocode, 1, 5) == "51059"]|h_geocode %in% data$h_geocode[substr(data$h_geocode, 1, 5) == "51059"])

# third

xwalk <- read.csv("data/original/va_xwalk.csv", 
                 colClasses = c("tabblk2010" = "factor"))


