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
    geom_line(mapping = aes(x = Decades, y = value, colour = variable, group = variable))
 
  p_plotly <- ggplotly(p,height = 400)
  
  return(p_plotly)

})

############################################################################
# income scatter plot 
############################################################################


output$income_plotly <- renderPlotly({
# color = Income_Inequality
  
  p <- ggplot(data = world_hdi, aes(text = Country)) + my_theme +
    labs(title = "Countries's Estimated Gross National Income per Capita", 
         y = "Human Development Index", x = "Gross National Income per Capita")
  
 if("Overall" %in% input$Gender && input$regression_line != TRUE){
   p <- p + geom_point(aes(x = Gross.national.income..GNI..per.capita, y = HDI), color = "brown") 
 }
  else if("Overall" %in% input$Gender && input$regression_line){
    p <- p + geom_point(aes(x = Gross.national.income..GNI..per.capita, y = HDI), color = "brown") + 
      geom_smooth(aes(x = Gross.national.income..GNI..per.capita, y = HDI))
  }
  
  if("Male" %in% input$Gender){
    p <- p + geom_point(aes(x = Estimated.gross.national.income.per.capita.Male, y = HDI), color = "pink")
    
  } else if("Male" %in% input$Gender && input$regression_line){
    p <- p + geom_point(aes(x = Estimated.gross.national.income.per.capita.Male, y = HDI), color = "pink") + 
      geom_smooth(aes(x = Estimated.gross.national.income.per.capita.Male, y = HDI))
  }
  
  if("Female" %in% input$Gender){
    p <- p + geom_point(aes(x = Estimated.gross.national.income.per.capita.Female, y = HDI), color = "orange")
    
  } else if("Female" %in% input$Gender && input$regression_line){
    p <- p + geom_point(aes(x = Estimated.gross.national.income.per.capita.Female, y = HDI), color = "orange") + 
      geom_smooth(aes(x = Estimated.gross.national.income.per.capita.Female, y = HDI))
  }
  
  p <- p 
  p_plotly <- ggplotly(p, height = 400)
  return(p_plotly)
})
############################################################################
# health histogram 
############################################################################
output$health_histogram <- renderPlotly({
  
  temp <- world_hdi %>% select(Physicians...per.10.000.people..2001.2014,
                       Deaths.due.to.Tuberculosis..per.100.000.people..)
  temp <- mutate(temp, Physicians = cut(Physicians...per.10.000.people..2001.2014,
                                        c(-1, 5, 10, 20, Inf),
                                labels = c("Extrmely Low (<5)","Low(<10)",
                                           "Moderate (<20)", "High(<infinity)")),
         Tuberculosis = cut(Deaths.due.to.Tuberculosis..per.100.000.people..,
                            c(-1, 10, 20, 40, 60, Inf),
                            labels = c("Extremely Low","Low",
                                       "Mid", "Slightly Large","Significantly Large")))
  
  if (input$health_choice == "General" && input$Regions == "all"){
      p <- ggplot(data = world_hdi) + geom_histogram(aes(x = Life.expectancy, fill = HDLevel), color = "black",
                                                     bins = as.numeric(input$n_breaks)) + 
        labs(title = "Life Expectancy over HDI")
     
  }else if (input$health_choice == "Physicians" && input$Regions == "all"){
      p <- ggplot(data = world_hdi) + geom_histogram(aes(x = Life.expectancy, fill = HDLevel), color = "black",
                                                     bins = as.numeric(input$n_breaks)) + 
            facet_wrap(~temp$Physicians) + 
        labs(title = "Life Expectancy over HDI by Number of Physicians per 10,000 people")
      
  } else if (input$health_choice == "Tuberculosis" && input$Regions == "all"){
    p <- ggplot(data = world_hdi) + geom_histogram(aes(x = Life.expectancy, fill = HDLevel), color = "black",
                                                   bins = as.numeric(input$n_breaks)) + 
      facet_wrap(~temp$Tuberculosis) + 
    labs(title = "Life Expectancy over HDI by Deaths due to Tuberculosis per 10,000 people") 
  
  } else if (input$health_choice == "Tuberculosis" && input$Regions != "all"){
    subtemp = world_hdi[which(world_hdi$HDLevel == input$Regions),]
    subtemp = mutate(subtemp, Tuberculosis = cut(Deaths.due.to.Tuberculosis..per.100.000.people..,
                                      c(-1, 10, 20, 40, 60, Inf),
                                      labels = c("Extremely Low","Low",
                                                  "Mid", "Slightly Large","Significantly Large")))
    
    p <- ggplot(data = subtemp) +
          geom_histogram(aes(x = Life.expectancy, fill = HDLevel), color = "black",
                            bins = as.numeric(input$n_breaks)) + 
      facet_wrap(~subtemp$Tuberculosis) + 
      labs(title = "Life Expectancy over HDI by Deaths due to Tuberculosis per 10,000 people")
    
    } else if (input$health_choice == "Physicians" && input$Regions != "all"){
      subtemp = world_hdi[which(world_hdi$HDLevel == input$Regions),]
      subtemp = mutate(subtemp, Physicians = cut(Physicians...per.10.000.people..2001.2014,
                                                 c(-1, 5, 10, 20, Inf),
                                                 labels = c("Extrmely Low (<5)","Low(<10)",
                                                            "Moderate (<20)", "High(<infinity)")))
      
      p <- ggplot(data = subtemp) +
        geom_histogram(aes(x = Life.expectancy, fill = HDLevel), color = "black",
                       bins = as.numeric(input$n_breaks)) + 
        facet_wrap(~subtemp$Physicians) + 
        labs(title = "Life Expectancy over HDI by Deaths due to Tuberculosis per 10,000 people") 
    } else if (input$health_choice == "General" && input$Regions != "all"){
        p <- ggplot(data = world_hdi[which(world_hdi$HDLevel == input$Regions),]) + 
          geom_histogram(aes(x = Life.expectancy, fill = HDLevel), color = "black",
                                                       bins = as.numeric(input$n_breaks)) + 
          labs(title = "Life Expectancy over HDI")
    }
  
  p_plotly <- ggplotly(p, height = 400)
  return(p_plotly)
  })

############################################################################
# education scatter plot (plotly playable)
############################################################################

output$education_plotly_playable <- renderPlotly({
  if (input$education_choice == "General"){
    p_gen <- ggplot(world_hdi, 
                    aes(x = `HDI`,
                        y = `Mean.years.of.schooling`,
                        color = continent,
                        text = `Country`)) +
      geom_point(
        #frame = year
      )  + 
      labs(x = "HDI",
           y = "Mean Years of Schooling",
           color = "Continent",
           title = "HDI vs. Mean Years of Schooling") +
      my_theme + scale_color_manual(values = cb_pal) 
    
  } else if (input$education_choice == "Female"){
    p_gen <- ggplot(world_hdi, 
                    aes(x = `HDI`, 
                        color = continent,
                        text = `Country`)) +
      geom_point(aes(y = `Mean.years.of.schooling.Female`
                     #frame = year
      )) + 
      labs(x = "HDI",
           y = "Mean Years of Schooling of Female",
           color = "Continent",
           title = "HDI vs. Mean Years of Schooling of Female") +
      my_theme + scale_color_manual(values = cb_pal)
    
    
  } else {
    p_gen <- ggplot(world_hdi, 
                    aes(x = `HDI`, 
                        color = continent,
                        text = `Country`)) +
      geom_point(aes(y = `Mean.years.of.schooling.Male`
                     #frame = year
      )) + 
      labs(x = "HDI",
           y = "Mean Years of Schooling of Female",
           color = "Continent",
           title = "HDI vs. Mean Years of Schooling of Male") +
      my_theme + scale_color_manual(values = cb_pal)
    
  }

  
  p_gen_plotly <- ggplotly(p_gen,tooltip = "text")
  
  return(p_gen_plotly)
})


############################################################################
# MDS contour plot
############################################################################

output$m_plot <- renderPlot({
  all_cont_with_cc <- world_hdi %>% select("HDI",
                                           "Life.expectancy",
                                           "Mean.years.of.schooling", 
                                           "Gross.national.income..GNI..per.capita",
                                           "Total.Population..millions..2015","Total.Population..millions..2030",
                                           "Population.Median.age..years..2015",
                                           "Mortality.rates.Infant..per.1.000.live.births..2015",                          
                                           "HIV.prevalence..adult....ages.15.49.",                                         
                                           "Public.health.expenditure....of.GDP..2014",                                    
                                           "Employment.to.population.ratio....ages.15.and.older." ,                        
                                           "Labour.force.participation.rate....ages.15.and.older.",                        
                                           "Total.Unemployment....of.labour.force..2015" ,                               
                                           "Working.poor.at.PPP.3.10.a.day.....2004.2013",                                 
                                           "Mandatory.paid.maternity.leave..days.",                                      
                                           "Internet.users",                                                               
                                           "Internet.users....2010..2015.",                                              
                                           "Difference.from.HDI.rank",                                                     
                                           "Coefficient.of.human.inequality",                          
                                           "Inequality.adjusted.education.index",                    
                                           "Inequality.adjusted.income.index",
                                           "Country",
                                           "continent"
  )
  
  all_cont_with_cc[complete.cases(all_cont_with_cc), ]
  
  all_cont <-  all_cont_with_cc %>% select("HDI",
                                           "Life.expectancy",
                                           "Mean.years.of.schooling", 
                                           "Gross.national.income..GNI..per.capita",
                                           "Total.Population..millions..2015","Total.Population..millions..2030",
                                           "Population.Median.age..years..2015",
                                           "Mortality.rates.Infant..per.1.000.live.births..2015",                          
                                           "HIV.prevalence..adult....ages.15.49.",                                         
                                           "Public.health.expenditure....of.GDP..2014",                                    
                                           "Employment.to.population.ratio....ages.15.and.older." ,                        
                                           "Labour.force.participation.rate....ages.15.and.older.",                        
                                           "Total.Unemployment....of.labour.force..2015" ,                               
                                           "Working.poor.at.PPP.3.10.a.day.....2004.2013",                                 
                                           "Mandatory.paid.maternity.leave..days.",                                      
                                           "Internet.users",                                                               
                                           "Internet.users....2010..2015.",                                              
                                           "Difference.from.HDI.rank",                                                     
                                           "Coefficient.of.human.inequality",                          
                                           "Inequality.adjusted.education.index",                    
                                           "Inequality.adjusted.income.index" )
  cont_scale <- scale(all_cont)
  
  dist_cont <- dist(cont_scale)
  
  cont_mds <- cmdscale(dist_cont, k = 2)
  
  cont_mds <- data.frame(cont_mds)
  cont_mds$Country <- all_cont_with_cc$Country
  cont_mds$continent <- all_cont_with_cc$continent
  
  colnames(cont_mds) <- c("mds_coordinate_1", "mds_coordinate_2","Country",
                          "continent")
  
  
  base <- ggplot(cont_mds, 
                 aes(x = mds_coordinate_1, y = mds_coordinate_2)) 
  
  only_point <- base +
    geom_point(size = 2) + 
    my_theme
  
  gg_with_dens <- 
    base + 
    geom_density2d() + 
    geom_point() + 
    my_theme
  
  
  if (input$contourline){
    gg_with_dens + 
      my_theme
  } 
  else {
    base + 
      geom_point()+ 
      my_theme
  }
  
  
  
})


}
