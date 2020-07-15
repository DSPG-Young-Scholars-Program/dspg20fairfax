library(dplyr)
library(tidycensus)
library(leaflet)
library(sf)
library(htmltools)
library(htmlwidgets)
library(readxl)
library(stringr)
library(data.table)
library(tigris)

##Import Occupations Data

occupations_byworkplace <- read_excel(
  "./data/original/jobseq/occupations_byworkplace.xlsx", skip = 2)[1:27,]
occupations_byworkplace <- data.table(occupations_byworkplace)

#Clean Headers & ZCTA Region Column

header <- c(
"Region",
"FIPS",
"Retail Salespersons (41-2030)",
"First-Line Supervisors of Retail Sales Workers (41-1011)",
"Cashiers (41-2010)",
"Waiters and Waitresses (35-3030)",
"Cooks (35-2010)",
"Laborers and Material Movers (53-7060)",
"Construction Laborers (47-2060)",
"Carpenters (47-2030)",
"Grounds Maintenance Workers (37-3000)",
"Personal Care Aides (31-1122)",
"Childcare Workers (39-9010)",
"Assemblers and Fabricators (51-2000)",
"Food Service Managers (11-9050)",
"Food Preparation Workers (35-2020)",
"Security Guards (33-9032)"
)

names(occupations_byworkplace) <- header
occupations_byworkplace$ZCTA <- substring(occupations_byworkplace$Region, 5, 10)
occupations_byworkplace$totalvulnerable <- rowSums(occupations_byworkplace[,3:17])

##Import UIclaims by Zipcode

uiclaims <- read.delim("./data/original/vworks/uiclaims_initials.txt") %>%
  filter(areatyname == "Zip Code")

#Fix Dates

uiclaims$startdate <- substr(uiclaims$startdate,1,nchar(uiclaims$startdate)-9)
uiclaims$enddate <- substr(uiclaims$enddate,1,nchar(uiclaims$enddate)-9)

# Filter to 2020 claims and initial claims only
uiclaims <- uiclaims %>% filter(periodyear == 2020 & claimtype == 1)

#Create Total Claims Column

uiaggregate <- aggregate(cbind(claimants) ~ area, data = uiclaims, sum)
uiheader <- c("ZCTA", "totalclaims")
names(uiaggregate) <- uiheader

#Link Total Claims to Vulnerable Claims

uiaggregate$ZCTA <- as.numeric(uiaggregate$ZCTA)
occupations_byworkplace$ZCTA <- as.numeric(occupations_byworkplace$ZCTA)
final_occupations <- left_join(occupations_byworkplace, uiaggregate, by = "ZCTA")
final_occupations$ZCTA <- as.character(final_occupations$ZCTA)

#Create percentvulnerable

final_occupations$percentvulnerable <- final_occupations$totalvulnerable/final_occupations$totalclaims
