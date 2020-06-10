#install.packages("tidycensus")
#install.packages("tidyverse")
#install.packages("viridis")
#install.packages("sf")
#install.packages("ggthemes")
#install.packages("RColorBrewer")
#install.packages("ggplot2")

library(tidycensus)
library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)

# Obtain the API key and set install=TRUE during the first time use, to use the same key automatically you run the program #
#census_api_key("bf083ecaf2c15d0b10906c4948c5759589df1817", install=TRUE)

# To see the variables in the survey #

var_ACS5 <- load_variables(2018, "acs5", cache = TRUE)

View(var_ACS5)

###############################################################
# Creating a vector with the variable we nedd from the survey #
###############################################################

vars <-c(
  # labor force
  "B23025_002",
  
  # unemplyed
  "B23025_005",
  
  # No Bachelor's degree
  "B23006_001", "B23006_024",
  
  # atleast travle 30 min
  "B08135_001", "B08135_007", "B08135_008", "B08135_009", "B08135_010",
  
  # mean travel time
  "B08135_001",
  
  # no vehicle
  "B08141_001", "B08141_002",
  
  # means of transportation
  "B08301_001", "B08301_002","B08301_003", "B08301_004", "B08301_010", "B08301_016", "B08301_017",
  "B08301_018", "B08301_019", "B08301_020", "B08301_021",
  
  # class of workers
  "C24060_001", "C24060_007", "C24060_013", "C24060_019", "C24060_025", "C24060_031", 

 # industry
 "C24070_001", "C24070_002", "C24070_003", "C24070_004","C24070_005", "C24070_006", "C24070_007", "C24070_008", "C24070_009", "C24070_010",
 "C24070_011", "C24070_012", "C24070_013", "C24070_014")


#View(vars)
####################################################
# Use the get_acs function to pull the survey data #
####################################################

# If geography=tract, we need to define the state and county #
# If you put the name of the state and county in the function, it will indicate the code for that state and/or county #
# Virginia state code is 51 and Fairfax county code 059

Fairfax<-get_acs(geography = "tract", variables=vars,state="51",county="059", 
                year=2018,survey = "acs5",cache_table = TRUE, output = "wide",
                 geometry=TRUE, keep_geo_vars = TRUE)

View(Fairfax)


################################################
# Description of the variable to be calcualted #
################################################

#Percent population in labor force that is unemployed
#Percent population in labor force without a Bahelor's degree
#Workers traveling 30 minutes or more to work
#Mean travle time to work
# Percent worker with no vehicle available

# Percent workers by means of transportation to work #
#Drove alone
#Carpool
#Public transport
#Walked
#Taxicab and other (Motorcycle, bicycle, other means)
#Worked from home

#Percent workers by class of worker #
# Private company
# Self-employed in own incorporated business
# Private not-for-profit wage and salary workers
# Local+state+federal government
# Self-employed in own not incorporated business and unpaid family worker

# Percent workers by industry #
#Agriculture, forestry,fishing, hunting and mining
#Construction
#Manufacturing
#Wholesale trade
#Retail trade
# Transportation, warehousing and utilities
# Information
# Finance, insurance, real estate, rental and lease
# Professional, scietific, management, administrative
# Educational services, health care
# Arts, entertainment and recreation
# Other services
# Public administration


#############################
# Calculating the variables #
#############################

ACS_data <- Fairfax %>% transmute(
  STATEFP= STATEFP,
  COUNTYFP= COUNTYFP,
  TRACTCE= TRACTCE,
  AFFGEOID= AFFGEOID,
  GEOID= GEOID,
  NAME.x= NAME.x,
  LSAD= LSAD,
  ALAND= ALAND,
  AWATER= AWATER,
  NAME.y= NAME.y,
  geometry=geometry,
  pct_unempl= (B23025_005E/B23025_002E)*100,
  pct_without_bachelors=(1-(B23006_024E/B23006_001E))*100,
  travel_30min = (B08135_007E + B08135_008E+ B08135_009E + B08135_010E)/B08135_001E*100,
  mean_travel_time= mean(B08135_001E),
  pct_no_vehicle= (B08141_002E/B08141_001E)*100,
  pct_drove_alone= (B08301_003E/B08301_001E)*100,
  pct_carpooled= (B08301_004E/B08301_001E)*100,
  pct_public_transport= (B08301_010E/B08301_001E)*100,
  pct_walked= (B08301_019E/B08301_001E)*100,
  pct_taxi_other= ((B08301_016E + B08301_017E + B08301_018E + B08301_020E)/B08301_001E)*100,
  pct_worked_home= (B08301_021E/B08301_001E)*100,
  pct_private= (C24060_007E/C24060_001E)*100,
  pct_own_business= (C24060_013E/C24060_001E)*100,
  pct_not_for_profit= (C24060_019E/C24060_001E)*100,
  pct_govt= (C24060_025E/C24060_001E)*100,
  pct_selfEmp_unpaid= (C24060_031E/C24060_001E)*100,
  pct_ag= (C24070_002E/C24070_001E)*100,
  pct_constr= (C24070_003E/C24070_001E)*100,
  pct_manuf= (C24070_004E/C24070_001E)*100,
  pct_wholesale= (C24070_005E/C24070_001E)*100,
  pct_retail= (C24070_006E/C24070_001E)*100,
  pct_transport= (C24070_007E/C24070_001E)*100,
  pct_info= (C24070_008E/C24070_001E)*100,
  pct_fin= (C24070_009E/C24070_001E)*100,
  pct_prof= (C24070_010E/C24070_001E)*100,
  pct_edu= (C24070_011E/C24070_001E)*100,
  pct_arts= (C24070_012E/C24070_001E)*100,
  pct_other= (C24070_013E/C24070_001E)*100,
  pct_pub_admin= (C24070_014E/C24070_001E)*100
)


View(ACS_data)
