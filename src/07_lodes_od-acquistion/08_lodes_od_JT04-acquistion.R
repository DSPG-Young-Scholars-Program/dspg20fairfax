# start here:  lehd.ces.census.gov/data/lodes/LODES7/
# navigate to here: https://lehd.ces.census.gov/data/lodes/LODES7/va/od/

library(R.utils)
library(data.table)
library(tidyr)
library(dplyr)

# main table
# the main part includes jobs with both workplace and residence in the state 

download.file(
  url = "https://lehd.ces.census.gov/data/lodes/LODES7/va/od/va_od_main_JT04_2015.csv.gz", 
  destfile = "./data/original/va_od_main_JT04_2015.csv.gz")
gunzip("./data/original/va_od_main_JT04_2015.csv.gz")

# aux table
# the aux part includes jobs with the workplace in the state and the residence outside of the state

download.file(
  url = "https://lehd.ces.census.gov/data/lodes/LODES7/va/od/va_od_aux_JT04_2015.csv.gz", 
  destfile ="./data/original/va_od_aux_JT04_2015.csv.gz")

gunzip("./data/original/va_od_aux_JT04_2015.csv.gz")


aux <- fread("./data/original/va_od_aux_JT04_2015.csv", 
             colClasses = c("w_geocode" = "character",
                            "h_geocode" = "character"))
head(aux)

main <- fread("./data/original/va_od_main_JT04_2015.csv", 
              colClasses = c("w_geocode" = "character",
                             "h_geocode" = "character"))
head(main)

# first, append your aux to main by job type

data <- rbind(main, aux)
head(data)
# second, filter only to rows where a Fairfax County block group is either the origin or in the destination
#VA code is 51 and Fairfax county code is 059

fairfax <- data %>% 
  filter(w_geocode %in% data$w_geocode[substr(data$w_geocode, 1, 5) == "51059"]|h_geocode %in% data$h_geocode[substr(data$h_geocode, 1, 5) == "51059"])


# select tracts from geocode strings

fairfax$w_geocode_tract <- substr(fairfax$w_geocode, start = 1, stop = 11)

fairfax$h_geocode_tract <- substr(fairfax$h_geocode, start = 1, stop = 11)

# aggregate by tract level
#Origin-Destination (OD) File Structure 
# w_geocode Char15 Workplace Census Block Code 
# h_geocode Char15 Residence Census Block Code 
# S000 Num Total number of jobs 
# SA01 Num Number of jobs of workers age 29 or younger16 
# SA02 Num Number of jobs for workers age 30 to 5416 
# SA03 Num Number of jobs for workers age 55 or older16 
# SE01 Num Number of jobs with earnings $1250/month or less 
# SE02 Num Number of jobs with earnings $1251/month to $3333/month 
# SE03 Num Number of jobs with earnings greater than $3333/month 
# SI01 Num Number of jobs in Goods Producing industry sectors 
# SI02 Num Number of jobs in Trade, Transportation, and Utilities industry sectors 
# SI03 Num Number of jobs in All Other Services industry sectors 
# createdate Char Date on which data was created, formatted as YYYYMMDD



jobs_federal <- fairfax %>% select(-w_geocode, - h_geocode) %>%
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

#head(jobs_federal)

write.csv(jobs_federal, "./data/od/jobs_federal.csv")

jobs_federal <- read.csv("./data/od/jobs_federal.csv")
