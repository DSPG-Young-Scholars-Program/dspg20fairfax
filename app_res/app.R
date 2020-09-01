library(tidyverse)
library(leaflet)
library(shiny)
library(shinythemes)
library(sf)

# Load data
resdata <- read_rds("res_ACS_data.Rds")
resdata <- st_transform(resdata, 4269)

vars_travel <- c("Percent workers driving alone", 
                 "Percent workers carpooling", 
                 "Percent workers using public transit", 
                 "Percent workers walking to work", 
                 "Percent workers using other transportation", 
                 "Percent workers working from home",
                 "Percent workers traveling 30 minutes or more to work")

vars_class <- c("Percent employed in private companies", 
                "Percent employed in private not-for-profit", 
                "Percent self-employed in own incorporated business", 
                "Percent self-employed in own not incorporated business",
                "Percent employed in government")

vars_ind <- c("Percent employed in construction", 
              "Percent employed in manufacturing", 
              "Percent employed in wholesale trade", 
              "Percent employed in retail trade", 
              "Percent employed in information industry", 
              "Percent employed in finance, insurance, real estate", 
              "Percent employed in professional, scientific, management", 
              "Percent employed in educational services, health care, social assistance", 
              "Percent employed in arts, entertainment, recreation, accomodation, food services", 
              "Percent employed in other services", 
              "Percent employed in public administration")

# UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                tabsetPanel(
                  tabPanel("Commuting", 
                           fluidRow(style = "margin: 17px",
                                    h4(strong('Employed Fairfax County Residents\' Means of Commuting by Residence Census Tract')),
                                    p(),
                                    selectInput('whichvar_travel', 'Select Variable:', vars_travel, width = '100%'),
                                    p(),
                                    leafletOutput('plot_travel', height = "450px"),
                                    p('Source: American Community Survey, 2014/18')
                           )
                  ), 
                  tabPanel("Work Class", 
                           fluidRow(style = "margin: 17px",
                                    h4(strong('Employed Fairfax County Residents\' Work Class by Residence Census Tract')),
                                    p(),
                                    selectInput('whichvar_class', 'Select Variable:', vars_class, width = '100%'),
                                    p(),
                                    leafletOutput('plot_class', height = "450px"),
                                    p('Source: American Community Survey, 2014/18')
                           )
                  ), 
                  tabPanel("Industry", 
                           fluidRow(style = "margin: 17px",
                                    h4(strong('Employed Fairfax County Residents\' Work Industry by Residence Census Tract')),
                                    p(),
                                    selectInput('whichvar_ind', 'Select Variable:', vars_ind, width = '100%'),
                                    p(),
                                    leafletOutput('plot_ind', height = "450px"),
                                    p('Source: American Community Survey, 2014/18')
                           )
                  )
                )
)

# Server
server <- function(input, output, session) {
  
  output$plot_travel <- renderLeaflet({ 
    
    plotvar_travel <- switch(input$whichvar_travel,
                             "Percent workers traveling 30 minutes or more to work" = resdata$res_travel_30min,
                             "Percent workers driving alone" = resdata$res_pct_drove,
                             "Percent workers carpooling" = resdata$res_pct_carpooled,
                             "Percent workers using public transit" = resdata$res_pct_transport,
                             "Percent workers walking to work" = resdata$res_pct_walked,
                             "Percent workers using other transportation" = resdata$res_pct_taxi_other,
                             "Percent workers working from home" = resdata$res_pct_worked_home)
    
    pal <- colorQuantile("Blues", domain = plotvar_travel, probs = seq(0, 1, length = 6), right = FALSE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            resdata$NAME.y,
            "<br />",
            "<strong>Percent:</strong>",
            round(plotvar_travel, 2)),
      htmltools::HTML
    )
    
    leaflet(data = resdata, 
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(plotvar_travel),
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels) %>%
      addLegend("bottomleft", 
                pal = pal, 
                values =  ~plotvar_travel,
                title = "Percent by<br>Quintile Group", 
                opacity = 1,
                na.label = "Not Available",
                labFormat = function(type = "quantile", cuts = 5, p = plotvar_travel) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
  output$plot_class <- renderLeaflet({ 
    
    plotvar_class <- switch(input$whichvar_class,
                             "Percent employed in private companies" = resdata$res_pct_private,
                             "Percent self-employed in own incorporated business" = resdata$res_pct_own_business,
                             "Percent employed in private not-for-profit" = resdata$res_pct_not_for_profit,      
                             "Percent employed in government" = resdata$res_pct_govt,
                             "Percent self-employed in own not incorporated business" = resdata$res_pct_selfEmp_unpaid)
    
    pal <- colorQuantile("Blues", domain = plotvar_class, probs = seq(0, 1, length = 6), right = FALSE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            resdata$NAME.y,
            "<br />",
            "<strong>Percent:</strong>",
            round(plotvar_class, 2)),
      htmltools::HTML
    )
    
    leaflet(data = resdata, 
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(plotvar_class),
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels) %>%
      addLegend("bottomleft", 
                pal = pal, 
                values =  ~plotvar_class,
                title = "Percent by<br>Quintile Group", 
                opacity = 1,
                na.label = "Not Available",
                labFormat = function(type = "quantile", cuts = 5, p = plotvar_class) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
  output$plot_ind <- renderLeaflet({ 
    
    plotvar_ind <- switch(input$whichvar_ind,
                          "Percent employed in construction" = resdata$res_pct_constr,
                          "Percent employed in manufacturing" = resdata$res_pct_manuf,
                          "Percent employed in wholesale trade" = resdata$res_pct_wholesale,
                          "Percent employed in retail trade" = resdata$res_pct_retail, 
                          "Percent employed in information industry" = resdata$res_pct_info,
                          "Percent employed in finance, insurance, real estate" = resdata$res_pct_fin,
                          "Percent employed in professional, scientific, management" = resdata$res_pct_prof,
                          "Percent employed in educational services, health care, social assistance" = resdata$res_pct_edu,
                          "Percent employed in arts, entertainment, recreation, accomodation, food services" = resdata$res_pct_arts,                
                          "Percent employed in other services" = resdata$res_pct_other,
                          "Percent employed in public administration" = resdata$res_pct_pub_admin)
    
    pal <- colorQuantile("Blues", domain = plotvar_ind, probs = seq(0, 1, length = 6), right = FALSE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            resdata$NAME.y,
            "<br />",
            "<strong>Percent:</strong>",
            round(plotvar_ind, 2)),
      htmltools::HTML
    )
    
    leaflet(data = resdata, 
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(plotvar_ind),
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels) %>%
      addLegend("bottomleft", 
                pal = pal, 
                values =  ~plotvar_ind,
                title = "Percent by<br>Quintile Group", 
                opacity = 1,
                na.label = "Not Available",
                labFormat = function(type = "quantile", cuts = 5, p = plotvar_ind) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

