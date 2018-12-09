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
              labFormat = labelFormat(),
              title = "Human Development Index")
 
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
    labs (x = "Decades", y = "Average Annual HDI Growth(%)", title = "Average Annual HDI Growth 1990-2015(%)") + 
    geom_line(mapping = aes(x = Decades, y = value, colour = variable, group = variable)) + 
    scale_colour_discrete(name = "Country")
 
  p_plotly <- ggplotly(p,height = 400)
  
  return(p_plotly)

})

#scatterplot

output$plot1 <- renderPlotly({
# color = Income_Inequality
  p <- ggplot(data = world_hdi, aes(text = Country)) + my_theme +
    labs(title = "Estimated Gross Incomes of Countries by Gender", y = "HDI", x = "GNI")
 if("Overall" %in% input$Gender){
   p <- p + geom_point(aes(x = Gross.national.income..GNI..per.capita, y = HDI), size = input$pointSize2)
 }
    
  if("Male" %in% input$Gender){
    p <- p + geom_point(aes(x = Estimated.gross.national.income.per.capita.Male, y = HDI), size = input$pointSize2, color = "blue")
  }
  if("Female" %in% input$Gender){
    p <- p + geom_point(aes(x = Estimated.gross.national.income.per.capita.Female, y = HDI), size = input$pointSize2, color = "red")
  }
  p_plotly <- ggplotly(p, height = 400)
  return(p_plotly)
})

output$plot2 <- renderPlotly({
  p <- ggplot(data = world_hdi) + geom_histogram(aes(x = Life.expectancy, fill = HDLevel, color = "black"))
  p_plotly <- ggplotly(p, height = 400)
  return(p_plotly)
})

}
