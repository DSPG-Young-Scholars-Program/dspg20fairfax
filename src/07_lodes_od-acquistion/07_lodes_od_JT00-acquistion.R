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


aux <- fread("data/original/va_od_aux_JT00_2017.csv", 
             colClasses = c("w_geocode" = "character",
                            "h_geocode" = "character"))
head(aux)

main <- fread("data/original/va_od_main_JT00_2017.csv", 
              colClasses = c("w_geocode" = "character",
                             "h_geocode" = "character"))
head(main)

# first, append your aux to main by job type

data <- rbind(main, aux)

# second, filter only to rows where a Fairfax County block group is either the origin or in the destination

fairfax <- data %>% 
  filter(w_geocode %in% data$w_geocode[substr(data$w_geocode, 1, 5) == "51059"]|h_geocode %in% data$h_geocode[substr(data$h_geocode, 1, 5) == "51059"])


# select tracts from geocode strings

fairfax$w_geocode_tract <- substr(fairfax$w_geocode, start = 1, stop = 11)

fairfax$h_geocode_tract <- substr(fairfax$h_geocode, start = 1, stop = 11)

# aggregate by tract level

jobsall <- fairfax %>% select(-w_geocode, - h_geocode) %>%
  group_by(createdate, w_geocode_tract, h_geocode_tract) %>% 
  summarise(S000 = sum(S000), 
            SA01 = sum(SA01), 
            SA02 = sum(SA02), 
            SA03 = sum(SA03), 
            SE01 = sum(SE01), 
            SE02 = sum(SE02), 
            SE03 = sum(SE03), 
            SI01 = sum(SI01), 
            SI02 = sum(SI02), 
            SI03 = sum(SI03))


#write.csv(jobsall, "data/od/jobsall.csv")

jobsall <- read.csv("data/od/jobsall.csv")

