# loading in origin-destination data for all private jobs in Fairfax County, VA

library(R.utils)
library(data.table)
library(tidyr)
library(dplyr)

# downloading main file to data/original
download.file(url = "https://lehd.ces.census.gov/data/lodes/LODES7/va/od/va_od_main_JT02_2017.csv.gz",
              destfile = "data/original/va_od_main_JT02_2017.csv.gz")

gunzip("data/original/va_od_main_JT02_2017.csv.gz")

# downloading aux file to data/original
download.file(url = "https://lehd.ces.census.gov/data/lodes/LODES7/va/od/va_od_aux_JT02_2017.csv.gz",
              destfile = "data/original/va_od_aux_JT02_2017.csv.gz")

gunzip("data/original/va_od_aux_JT02_2017.csv.gz")


# reading in tables

main <- fread("data/original/va_od_main_JT02_2017.csv",
              colClasses = c("w_geocode" = "character",
                             "h_geocode" = "character"))

aux <- fread("data/original/va_od_aux_JT02_2017.csv",
             colClasses = c("w_geocode" = "character",
                            "h_geocode" = "character"))
head(main)
head(aux)

#apending aux to main

combined <- rbind(main, aux)
View(combined)
#filtering for fairfax
View(combined)

fairfax <- combined %>% filter(w_geocode %in% combined$w_geocode[substr(combined$w_geocode, 1, 5) == "51059"]
                               |h_geocode %in% combined$h_geocode[substr(combined$h_geocode, 1, 5) == "51059"])


View(fairfax)

#selecting tracts
fairfax$w_geocode_tract <- substr(fairfax$w_geocode, start = 1, stop = 11)
fairfax$h_geocode_tract <- substr(fairfax$h_geocode, start = 1, stop = 11)

# aggregate by tact
jobs_priv <- fairfax %>% select(-w_geocode, - h_geocode) %>%
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

View(jobs_priv)

#saving jobs_priv as csv to data/od
write.csv(jobs_priv, "data/od/jobs_priv.csv")
