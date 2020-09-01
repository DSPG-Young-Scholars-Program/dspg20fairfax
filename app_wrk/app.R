library(tidyverse)
library(leaflet)
library(shiny)
library(shinythemes)
library(sf)

# Load data
wrkdata <- read_rds("wrk_ACS_data.Rds")
wrkdata <- st_transform(wrkdata, 4269)

vars_age <- c("Percent workers age 16-19", 
              "Percent workers age 20-24",
              "Percent workers age 25-44", 
              "Percent workers age 45-54",
              "Percent workers age 55-59", 
              "Percent workers age 60-64",
              "Percent workers age 65+")

vars_transit <- c("Percent workers driving alone", 
                  "Percent workers carpooling",
                  "Percent workers using public transit", 
                  "Percent workers walking",
                  "Percent workers using other means", 
                  "Percent workers working from home")

# UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                tabsetPanel(
                  tabPanel("Commuting", 
                           fluidRow(style = "margin: 17px",
                                    h4(strong('Means of Commuting of Employees in Fairfax County by Workplace Census Tract')),
                                    p(),
                                    selectInput('whichvar_transit', 'Select Variable:', vars_transit, width = '100%'),
                                    p(),
                                    leafletOutput('plot_transit', height = "450px"),
                                    p('Source: American Community Survey, 2014/18')
                           )
                  ), 
                  tabPanel("Age", 
                           fluidRow(style = "margin: 17px",
                                    h4(strong('Age Structure of Employees in Fairfax County by Workplace Census Tract')),
                                    p(),
                                    selectInput('whichvar_age', 'Select Variable:', vars_age, width = '100%'),
                                    p(),
                                    leafletOutput('plot_age', height = "450px"),
                                    p('Source: American Community Survey, 2014/18')
                           )
                  )
                )
)

# Server
server <- function(input, output, session) {
  
  output$plot_transit <- renderLeaflet({ 
    
    plotvar_transit <- switch(input$whichvar_transit,
                             "Percent workers driving alone" = wrkdata$wrk_pct_drive,
                             "Percent workers carpooling" = wrkdata$wrk_pct_carpool,
                             "Percent workers using public transit" = wrkdata$wrk_pct_pub_trans,
                             "Percent workers walking" = wrkdata$wrk_pct_walk,
                             "Percent workers using other means" = wrkdata$wrk_pct_other,
                             "Percent workers working from home" = wrkdata$wrk_pct_none)
    
    pal <- colorQuantile("Blues", domain = plotvar_transit, probs = seq(0, 1, length = 6), right = FALSE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            wrkdata$NAME.y,
            "<br />",
            "<strong>Percent:</strong>",
            round(plotvar_transit, 2)),
      htmltools::HTML
    )
    
    leaflet(data = wrkdata, 
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(plotvar_transit),
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels) %>%
      addLegend("bottomleft", 
                pal = pal, 
                values =  ~plotvar_transit,
                title = "Percent by<br>Quintile Group", 
                opacity = 1,
                na.label = "Not Available",
                labFormat = function(type = "quantile", cuts = 5, p = plotvar_tansit) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
  output$plot_age <- renderLeaflet({ 
    
    plotvar_age <- switch(input$whichvar_age,
                          "Percent workers age 16-19" = wrkdata$wrk_pct_16_19, 
                          "Percent workers age 20-24" = wrkdata$wrk_pct_20_24, 
                          "Percent workers age 25-44" = wrkdata$wrk_pct_25_44, 
                          "Percent workers age 45-54" = wrkdata$wrk_pct_45_54,
                          "Percent workers age 55-59" = wrkdata$wrk_pct_55_59, 
                          "Percent workers age 60-64" = wrkdata$wrk_pct_60_64, 
                          "Percent workers age 65+" = wrkdata$wrk_pct_65_up)
    
    pal <- colorQuantile("Blues", domain = plotvar_age, probs = seq(0, 1, length = 6), right = FALSE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            wrkdata$NAME.y,
            "<br />",
            "<strong>Percent:</strong>",
            round(plotvar_age, 2)),
      htmltools::HTML
    )
    
    leaflet(data = wrkdata, 
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(plotvar_age),
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels) %>%
      addLegend("bottomleft", 
                pal = pal, 
                values =  ~plotvar_age,
                title = "Percent by<br>Quintile Group", 
                opacity = 1,
                na.label = "Not Available",
                labFormat = function(type = "quantile", cuts = 5, p = plotvar_age) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)