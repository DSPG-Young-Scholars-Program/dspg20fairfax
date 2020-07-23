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

#View(var_ACS5)

###############################################################
# Creating a vector with the variable we nedd from the survey #
###############################################################

vars <-c(
  # labor force
  "B23025_002",

  # unemplyed
  "B23025_005",

  #part time,
  "B23022_003", "B23022_004", "B23022_027", "B23022_028", "B23022_001",

  # Median Income,
  "B24011_001",

  # No Bachelor's degree
  "B23006_001", "B23006_024",

  # class of workers
  "C24060_001", "C24060_003", "C24060_004", "C24060_005", "C24060_006",

  # industry
  "C24070_001", "C24070_006", "C24070_012", "C24070_013"

)

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

#View(Fairfax)


################################################
# Description of the variable to be calcualted #
################################################

#percent_unemployed <- (number of unemployed / number of persons in labor force * 100)
#percent_employed_part_time <- (number of part-time / number of persons in labor force * 100)
#percent_lf_highschool <- (sum persons in labor force with HS education or less than HS education / total in labor force * 100)
#median_household_income <- (NOTE: this variable will have missingness. Use listwise deletion for tracts with no data.)
#percent_employed_occupations_vuln <- identified as vulnerable to covid job loss per Brookings Institution in this appendix table
#percent_employed_impacted_industry <- identified as most impacted by covid per Brookings Institution in this table


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
  pct_empl_part_time= ((B23022_003E-B23022_004E + B23022_027E-B23022_028E)/(B23022_001E))*100,
  pct_lf_highschool= (1-(B23006_024E/B23006_001E))*100,
  median_household_income= B24011_001E,
  percent_employed_occupations_vuln= ((C24060_003E + C24060_004E + C24060_005E + C24060_006E)/C24060_001E)*100,
  percent_employed_impacted_industry= ((C24070_006E + C24070_012E + C24070_013E)/C24070_001E)*100

) %>% rename(NAME = NAME.y) %>% select(-NAME.x)



write.csv(ACS_data, "data/acs/acs_employment.csv", row.names = FALSE)

#View(ACS_data)


