# server

library(tidyverse)
library(forcats)
library(reshape2)
library(gapminder)
library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)
library(DT)

library(crosstalk)


function(input, output) {



############################################################################
# Leaflet: HTML Widget (embedded in Shiny)
############################################################################

output$leaflet <- renderLeaflet({
  
  m_leaflet <- world %>% leaflet() %>% setView(0, 20, 2) %>%
    addProviderTiles("MapBox", 
                     options = providerTileOptions(
                       id = "mapbox.light",
                       accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addPolygons(fillColor = ~pal(world_info$HDI),
                weight = 1,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 3,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels_world,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "16px",
                  direction = "auto")) %>%
    addLegend(pal = pal, 
              values = ~world_info$HDI,
              title = "HDI among Countries")
  
  return(m_leaflet)
})

############################################################################
# Dygraph: HTML Widget (embedded in Shiny)
############################################################################

output$plotshiny <- renderPlotly({
  gdppc <- reactive({

    df <- data.frame(Decades = c("1990-2000", "2000-2010", "2010-2015" ))
      sc <- world_hdi %>% select(Average.annual.HDI.growth.1990.2000,
                                  Average.annual.HDI.growth.2000.2010,
                                  Average.annual.HDI.growth.2010.2015,
                                  Country) %>% dplyr::filter(Country %in%  input$country_subset)
      for (c in as.vector(sc$Country)){
        df[,c] = as.numeric(sc[sc$Country == c,])[-4]
      }
      df <- melt(df, id.vars = 'Decades') 
    return(df)
    
  })
  
  p <- ggplot(gdppc()) + my_theme +
    labs (x = "Decades", y = "Average Annual HDI Growth", title = "Average Annual HDI Growth 1990-2015") + 
    geom_line(mapping = aes(x = Decades, y = value, colour = variable, group = variable)) + 
    scale_colour_discrete(name = "Country")
 
  p_plotly <- ggplotly(p,height = 300)
  
  return(p_plotly)

})

}
