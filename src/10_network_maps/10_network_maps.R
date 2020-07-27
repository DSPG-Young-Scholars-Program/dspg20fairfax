# leaflet maps

library(tidycensus)
library(leaflet)
library(sf)

var = "S000"
year = 2017

source("src/09_network_descriptives/network_functions.R")

network_stats(var = var, year = year)


data_fairfax <-   get_acs(geography = "tract",
                          variables = "B00001_001",
                          state = "VA",
                          county = 059, 
                          geometry = TRUE)


data_fairfax_final <- merge(data_fairfax[, c("GEOID", "geometry")], nodelist_S000_2017, by.x= "GEOID", by.y = "tract", all = TRUE)

data_fairfax_final <- st_transform(data_fairfax_final, 4326)


# WIEGHTED DEGREE CENTRALITY OUT

pal <- colorQuantile("Blues",5,  domain = data_fairfax_final$wtd_deg_cent_out)

leaflet(data = data_fairfax_final,options = leafletOptions(minZoom = 10))%>%
  addTiles("https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png") %>%
  addPolygons(fillColor = ~pal(wtd_deg_cent_out), fillOpacity = .8, stroke = FALSE) %>%
  addLegend("bottomleft", 
            pal = pal, 
            values =  ~wtd_deg_cent_out,
            title = "Weighted Degree Centrality: Out<br> By Quantile Group", 
            opacity = 1, 
            labFormat = function(type = "quantile", cuts = 10, p = wtd_deg_cent_out) {
              n = length(cuts)
              paste0("[", round(cuts[-n], 3), " &ndash; ", round(cuts[-1], 3), ")")
            })

# WEIGHTED DEGREE CENTRALITY IN

pal_in <- colorQuantile("Blues",5, domain = data_fairfax_final$wtd_deg_cent_in)

leaflet(data = data_fairfax_final,options = leafletOptions(minZoom = 10))%>%
  addTiles("https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png") %>%
  addPolygons(fillColor = ~pal_in(wtd_deg_cent_in), fillOpacity = .8, stroke = FALSE) %>%
  addLegend("bottomleft", 
            pal = pal_in, 
            values = ~wtd_deg_cent_in,
            title = "Weighted Degree Centrality: In<br> By Quantile Group", 
            opacity = 1,
            labFormat = function(type = "quantile", cuts = 10, p = wtd_deg_cent_in) {
              n = length(cuts)
              paste0("[", round(cuts[-n], 3), " &ndash; ", round(cuts[-1], 3), ")")
            })

# BETWEENESS CENTRALITY
pal_btw <- colorQuantile("Blues",5,  domain = data_fairfax_final$btw_cent)

leaflet(data = data_fairfax_final,options = leafletOptions(minZoom = 10))%>%
  addTiles("https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png") %>%
  addPolygons(fillColor = ~pal_btw(btw_cent), fillOpacity = .8, stroke = FALSE) %>%
  addLegend("bottomleft", 
            pal = pal_btw, 
            values = ~btw_cent,
            title = "Betweenness Centrality<br> By Quantile Group", 
            opacity = 1, 
            labFormat = function(type = "quantile", cuts = 10, p = btw_cent) {
              n = length(cuts)
              paste0("[", round(cuts[-n], 3), " &ndash; ", round(cuts[-1], 3), ")")
            })
# EIGEN CENTRALITy



