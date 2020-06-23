#install.packages("tidycensus")
#install.packages("tidyverse")
#install.packages("viridis")
#install.packages("sf")
#install.packages("ggthemes")
#install.packages("RColorBrewer")
#install.packages("ggplot2")
#install.packages("tigris")

library(tidycensus)
library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
library(tigris)
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
  "B08135_001", "B08135_002", "B08135_003", "B08135_004", "B08135_005", "B08135_006", "B08135_007", "B08135_008", 
  "B08135_009", "B08135_010",
  
  # no vehicle
  "B08141_001", "B08141_002",
  
  # means of transportation
  "B08301_001","B08301_003", "B08301_004", "B08301_010", "B08301_016", "B08301_017",
  "B08301_018", "B08301_019", "B08301_020", "B08301_021",
  
  # class of workers
  "C24060_001", "C24060_007", "C24060_013", "C24060_019", "C24060_025", "C24060_031", 

 # industry
 "C24070_001", "C24070_002", "C24070_003", "C24070_004","C24070_005", "C24070_006", "C24070_007", "C24070_008", "C24070_009", "C24070_010",
 "C24070_011", "C24070_012", "C24070_013", "C24070_014",

 # Means of transportation to work by occupation
 "B08124_002", "B08124_003", "B08124_004", "B08124_005", "B08124_006", "B08124_007", "B08124_009", "B08124_010", "B08124_011", "B08124_012", "B08124_013", "B08124_014",
 "B08124_016", "B08124_017", "B08124_018", "B08124_019", "B08124_020", "B08124_021", "B08124_023", "B08124_024", "B08124_025", "B08124_026", "B08124_027", "B08124_028",
 "B08124_030", "B08124_031", "B08124_032", "B08124_033", "B08124_034", "B08124_035", "B08124_037", "B08124_038", "B08124_039", "B08124_040", "B08124_041", "B08124_042",
 "B08124_044", "B08124_045", "B08124_046", "B08124_047", "B08124_048", "B08124_049"
 )

#View(vars)
####################################################
# Use the get_acs function to pull the survey data #
####################################################

# DATA AT TRACT LEVEL #

# If geography=tract, we need to define the state and county #
# If you put the name of the state and county in the function, it will indicate the code for that state and/or county #
# Virginia state code is 51 and Fairfax county code 059

Fairfax_T<-get_acs(geography = "tract", variables=vars,state="51",county="059", 
                year=2018,survey = "acs5",cache_table = TRUE, output = "wide",
                 geometry=TRUE, keep_geo_vars = TRUE)

View(Fairfax_T)



################################################
# Description of the variable to be calculated #
################################################

#Percent population in labor force that is unemployed
#Percent population in labor force without a Bahelor's degree
#Workers traveling 30 minutes or more to work
#Mean travle time to work (Used the lower limits of the time intervals because the last interval is open ended)
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

#Percent workers by means of transportation to work by occiupation #


#############################
# Calculating the variables #
#############################
###############
# Tract level #
###############

ACS_data_T <- Fairfax_T %>% transmute(
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
  
  res_pct_unempl= (B23025_005E/B23025_002E)*100,
  
  res_pct_without_bachelors=(1-(B23006_024E/B23006_001E))*100,
  
  res_travel_30min = (B08135_007E + B08135_008E + B08135_009E + B08135_010E)/B08135_001E*100,
  
  res_mean_travel_time= (0*B08135_002E + 10*B08135_003E + 15*B08135_004E + 20*B08135_005E + 25*B08135_006E + 30*B08135_007E + 35*B08135_008E + 45*B08135_009E + 60*B08135_010E)/B08135_001E,
  
  res_pct_no_vehicle= (B08141_002E/B08141_001E)*100,
  
  res_pct_drove= (B08301_003E/B08301_001E)*100,
  res_pct_carpooled= (B08301_004E/B08301_001E)*100,
  res_pct_transport= (B08301_010E/B08301_001E)*100,
  res_pct_walked= (B08301_019E/B08301_001E)*100,
  res_pct_taxi_other= ((B08301_016E + B08301_017E + B08301_018E + B08301_020E)/B08301_001E)*100,
  res_pct_worked_home= (B08301_021E/B08301_001E)*100,
  
  res_pct_private= (C24060_007E/C24060_001E)*100,
  res_pct_own_business= (C24060_013E/C24060_001E)*100,
  res_pct_not_for_profit= (C24060_019E/C24060_001E)*100,
  res_pct_govt= (C24060_025E/C24060_001E)*100,
  res_pct_selfEmp_unpaid= (C24060_031E/C24060_001E)*100,
  
  res_pct_ag= (C24070_002E/C24070_001E)*100,
  res_pct_constr= (C24070_003E/C24070_001E)*100,
  res_pct_manuf= (C24070_004E/C24070_001E)*100,
  res_pct_wholesale= (C24070_005E/C24070_001E)*100,
  res_pct_retail= (C24070_006E/C24070_001E)*100,
  res_pct_transport= (C24070_007E/C24070_001E)*100,
  res_pct_info= (C24070_008E/C24070_001E)*100,
  res_pct_fin= (C24070_009E/C24070_001E)*100,
  res_pct_prof= (C24070_010E/C24070_001E)*100,
  res_pct_edu= (C24070_011E/C24070_001E)*100,
  res_pct_arts= (C24070_012E/C24070_001E)*100,
  res_pct_other= (C24070_013E/C24070_001E)*100,
  res_pct_pub_admin= (C24070_014E/C24070_001E)*100,
  
  pct_manage_drove= (B08124_009E/B08124_002E)*100,
  pct_manage_carpooled= (B08124_016E/B08124_002E)*100,
  pct_manage_transport= (B08124_023E/B08124_002E)*100,
  pct_manage_walked= (B08124_030E/B08124_002E)*100,
  pct_manage_taxi= (B08124_037E/B08124_002E)*100,
  pct_manage_worked_home= (B08124_044E/B08124_002E)*100,
  
  res_pct_service_drove= (B08124_010E/B08124_003E)*100,
  res_pct_service_carpooled= (B08124_017E/B08124_003E)*100,
  res_pct_service_transport= (B08124_024E/B08124_003E)*100,
  res_pct_service_walked= (B08124_031E/B08124_003E)*100,
  res_pct_service_taxi= (B08124_038E/B08124_003E)*100,
  res_pct_service_worked_home= (B08124_045E/B08124_003E)*100,
  
  pct_service_drove= (B08124_010E/B08124_003E)*100,
  pct_service_carpooled= (B08124_017E/B08124_003E)*100,
  pct_service_transport= (B08124_024E/B08124_003E)*100,
  pct_service_walked= (B08124_031E/B08124_003E)*100,
  pct_service_taxi= (B08124_038E/B08124_003E)*100,
  pct_service_worked_home= (B08124_045E/B08124_003E)*100,
  
  pct_sales_drove= (B08124_011E/B08124_004E)*100,
  pct_sales_carpooled= (B08124_018E/B08124_004E)*100,
  pct_sales_transport= (B08124_025E/B08124_004E)*100,
  pct_sales_walked= (B08124_032E/B08124_004E)*100,
  pct_sales_taxi= (B08124_039E/B08124_004E)*100,
  pct_sales_worked_home= (B08124_046E/B08124_004E)*100,
  
  res_pct_natural_drove= (B08124_012E/B08124_005E)*100,
  res_pct_natural_carpooled= (B08124_019E/B08124_005E)*100,
  res_pct_natural_transport= (B08124_026E/B08124_005E)*100,
  res_pct_natural_walked= (B08124_033E/B08124_005E)*100,
  res_pct_natural_taxi= (B08124_040E/B08124_005E)*100,
  res_pct_natural_worked_home= (B08124_047E/B08124_005E)*100,
  
  res_pct_prod_drove= (B08124_013E/B08124_006E)*100,
  res_pct_prod_carpooled= (B08124_020E/B08124_006E)*100,
  res_pct_prod_transport= (B08124_027E/B08124_006E)*100,
  res_pct_prod_walked= (B08124_034E/B08124_006E)*100,
  res_pct_prod_taxi= (B08124_041E/B08124_006E)*100,
  res_pct_prod_worked_home= (B08124_048E/B08124_006E)*100,
  
  res_pct_military_drove= (B08124_014E/B08124_007E)*100,
  res_pct_military_carpooled= (B08124_021E/B08124_007E)*100,
  res_pct_military_transport= (B08124_028E/B08124_007E)*100,
  res_pct_military_walked= (B08124_035E/B08124_007E)*100,
  res_pct_military_taxi= (B08124_042E/B08124_007E)*100,
  res_pct_military_worked_home= (B08124_049E/B08124_007E)*100
)


View(ACS_data_T)
