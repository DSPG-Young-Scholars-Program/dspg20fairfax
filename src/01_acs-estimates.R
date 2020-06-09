#install.packages("tidycensus")
#install.packages("tidyverse")

library(tidycensus)
library(tidyverse)

#census_api_key("bf083ecaf2c15d0b10906c4948c5759589df1817", install=TRUE)

var_ACS5 <- load_variables(2018, "acs5", cache = TRUE)

View(var_ACS5)



emp_Fairfax<-get_acs(geography = "tract", variables=c("B23025_001","B23025_002","B23025_003","B23025_004","B23025_005","B23025_006",
                                                      "B16010_041", "B08135_001","B08135_007", "B08135_008", "B08135_009", "B08135_010",
                                                      "B08141_002","B08301_003", "B08301_004", "B08301_010", "B08301_016", "B08301_017",
                                                      "B08301_018", "B08301_019", "B08301_020", "B08301_021", "C24060_007", "C24060_013",
                                                      "C24060_019", "C24060_025", "C24060_031", "C24070_002", "C24070_003", "C24070_004",
                                                      "C24070_005", "C24070_006", "C24070_007", "C24070_008", "C24070_009", "C24070_010",
                                                      "C24070_011", "C24070_012", "C24070_013", "C24070_014"), state="51",county="059", year=2018,survey = "acs5",cache_table = TRUE, output = "wide")

#View(emp_Fairfax)


data <- as.data.frame(emp_Fairfax)
#View(data)

#Percent population in labor force that is unemployed
var1 <- (data$B23025_005E/data$B23025_002E)*100


#Percent population in labor force without a Bahelor's degree
var2 <- (1-(data$B16010_041E/data$B23025_002E))*100
#var2

#Workers traveling 30 minutes or more to work
var3 <- data$B08135_007E + data$B08135_008E+ data$B08135_009E + data$B08135_010E
#var3

#Mean travle time to work
var4 <- data$B08135_001E/data$B23025_002E
#var4

# Percent worker with no vehicle available
var5 <- (data$B08141_002E/data$B23025_002E)*100

# Percent workers by means of transportation to work #
#Drove alone
var6<- (data$B08301_003E/data$B23025_002E)*100

#Carpool
Var7 <-(data$B08301_004E/data$B23025_002E)*100

#Public transport
var8 <- (data$B08301_010E/data$B23025_002E)*100

#Walked
var9 <- (data$B08301_019E/data$B23025_002E)*100

#Taxicab and other (Motorcycle, bicycle, other means)
var10 <- ((data$B08301_016E + data$B08301_017E + data$B08301_018E + data$B08301_020E)/data$B23025_002E)*100

#Worked from home
var11 <- (data$B08301_021E/data$B23025_002E)*100


#Percent workers by class of worker #

# Private company
var12 <- (data$C24060_007E/data$B23025_002E)*100

# Self-employed in own incorporated business
var13 <- (data$C24060_013E/data$B23025_002E)*100

# Private not-for-profit wage and salary workers
var14 <- (data$C24060_019E/data$B23025_002E)*100

# Local+state+federal government
var15<- (data$C24060_025E/data$B23025_002E)*100

# Self-employed in own not incorporated business and unpaid family worker
var16<- (data$C24060_031E/data$B23025_002E)*100

# Percent workers by industry #

#Agriculture, forestry,fishing, hunting and mining
var17 <- (data$C24070_002E/data$B23025_002E)*100

#Construction
var18 <- (data$C24070_003E/data$B23025_002E)*100

#Manufacturing
var19 <- (data$C24070_004E/data$B23025_002E)*100

#Wholesale trade
var20 <- (data$C24070_005E/data$B23025_002E)*100

#Retail trade
var21 <- (data$C24070_006E/data$B23025_002E)*100

# Transportation, warehousing and utilities
var22 <- (data$C24070_007E/data$B23025_002E)*100

# Information
var23 <- (data$C24070_008E/data$B23025_002E)*100

# Finance, insurance, real estate, rental and lease
var24 <- (data$C24070_009E/data$B23025_002E)*100

# Professional, scietific, management, administrative
var25 <- (data$C24070_010E/data$B23025_002E)*100

# Educational services, health care
var26 <- (data$C24070_011E/data$B23025_002E)*100

# Arts, entertainment and recreation
var27 <- (data$C24070_012E/data$B23025_002E)*100

# Other services
var28 <- (data$C24070_013E/data$B23025_002E)*100

# Public administration
var29 <- (data$C24070_014E/data$B23025_002E)*100



data$"% unemployed" <- var1
data$"% without bachelors"<- var2
data$"Travelling >30min"<- var3
data$"Mean travel time to work" <- var4
data$"% with no vehicle" <- var5
data$"% drove alone" <- var6
data$"% carplooed" <- Var7
data$"% public transport" <- var8
data$"% walked" <- var9
data$"% taxicab and other" <- var10
data$"% worked from home" <- var11
data$"% in private company" <- var12
data$"% in self-employed in own incorporated business" <- var13
data$"% in private not-for-profit" <- var14
data$"% in Local&State&Federal govt" <- var15
data$"% Self-employed in own not incorporated business & unpaid family worker" <- var16
data$"% in Agri/Forestry/Fishing/Hunting/Mining" <- var17
data$"% in construction" <- var18
data$"% in manufacturing" <- var19
data$"% in wholesale trade" <- var20
data$"% in retail trade" <- var21
data$"% in transportation&warehousing&utilities" <- var22
data$"% in information" <- var23
data$"% in finance, insurance, real estate, rental" <- var24
data$"% in professional, scietific, management, administrative" <- var25
data$"% in educational services, health care" <-var26
data$"% in arts, entertainment, recreation" <- var27
data$"% in other services" <- var28
data$"% in public administration" <- var29

View(data)



