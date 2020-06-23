#---------------------FORTUNE 500---------------------#

url <- "https://opendata.arcgis.com/datasets/a4d813c396934fc09d0b801a0c491852_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"

download.file(url, destfile = "data/fortune/fortune.zip")

unzip("data/fortune/fortune.zip", exdir = "data/fortune")

#------------------FORTUNE 1000-----------------------#

install.packages('tidygeocoder')
library(tidygeocoder)
library(dplyr)

library(sf)

fortune <- read.csv("data/fortune/fortune_1000/fairfax_fortune_1000.csv")

fortune_geo <- fortune %>% 
  geocode(Address,lat=latitude,long=longitude)

fortune_geo[fortune_geo$Name == "Tegna Inc.", "latitude"] <- 38.923452
fortune_geo[fortune_geo$Name == "Tegna Inc.", "longitude"] <- -77.232659
fortune_geo <- st_as_sf(fortune_geo,  
                     coords = c('longitude', 'latitude'),
                     crs = 4326)

#write_sf(fortune_geo, "data/fortune/fortune_1000/fairfax_fortune_1000_geo.shp")

# Tegna is in Tysons (website), McLean (article); but matches to neither in the geocoder
# I used google maps to fill this in

# General Dynamics is in Falls Church (article), McLean (company website)






