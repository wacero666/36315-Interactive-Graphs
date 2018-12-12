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

## Histogram

############################################################################
# Leaflet: HTML Widget (embedded in Shiny)
############################################################################
output$plothis <- renderPlotly({
  if (input$individual_obs & input$density) {
    gg <- ggplot(world_hdi, aes(x = HDI)) +
      geom_histogram(aes(y = ..density..),
                     bins = as.numeric(input$n_breaks),
                     color = "black",
                     fill = "orangered3") +
      geom_rug() + my_theme + 
      geom_density(adjust = input$bw_adjust,
                   colour = "black") + 
      labs(x = "Human Development Index", y = "Frequency", title = "Distribution of HDI")
  } 
  
  if (!(input$individual_obs) & (input$density)) {
    gg <- ggplot(world_hdi, aes(x = HDI, y = ..density..)) +
      geom_histogram(aes(y = ..density..),
                     bins = as.numeric(input$n_breaks),
                     color = "black",
                     fill = "orangered3") +
      geom_density(adjust = input$bw_adjust,
                   colour = "black") + 
      labs(x = "Human Development Index", y = "Density", title = "Distribution of HDI")
    
  }
  
  if (input$individual_obs & !(input$density)) {
    gg <- ggplot(world_hdi, aes(x = HDI)) +
      geom_histogram(aes(y = ..density..),
                     bins = as.numeric(input$n_breaks),
                     color = "black",
                     fill = "orangered3") +
      geom_rug() + 
      labs(x = "Human Development Index", y = "Density", title = "Distribution of HDI")
    
  }
  
  if (!(input$individual_obs) & !(input$density)) {
    gg <- ggplot(world_hdi, aes(x = HDI, y = ..density..)) +
      geom_histogram(aes(y = ..density..),
                     bins = as.numeric(input$n_breaks),
                     color = "black",
                     fill = "orangered3") + 
      labs(x = "Human Development Index", y = "Density", title = "Distribution of HDI")
  }
  
  
  p_plotly <- ggplotly(gg,height = 400)
  return (p_plotly)
})
  
output$leaflet <- renderLeaflet({
  
  sub_world <- reactive({
    all <- data.frame(Country = as.character(world$name))
    sub <- world_info %>% subset(HDI.Rank <= as.numeric(input$TopNo.) & HDLevel %in% input$Level)
    all <- all %>% left_join(sub, by = c("Country" = "name"))
    
    return (all)
  })
  
  
  sub_world_info <- sub_world()
  m_leaflet <- world %>% leaflet() %>% setView(0, 20, 2) %>%
    addProviderTiles("MapBox", 
                     options = providerTileOptions(
                       id = "mapbox.light",
                       accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addPolygons(fillColor = ~pal(sub_world_info$HDI),
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
      sc <- world_hdi %>% dplyr::select(Average.annual.HDI.growth.1990.2000,
                                  Average.annual.HDI.growth.2000.2010,
                                  Average.annual.HDI.growth.2010.2015,
                                  Country) %>% dplyr::filter(Country %in%  input$country_subset)
      for (c in as.vector(sc$Country)){
        df[,c] = as.numeric(sc[sc$Country == c,])[-4]
      }
      df <- melt(df, id.vars = 'Decades') 
      df1 <- world_hdi %>% dplyr::select(HDLevel, Country)
      df<- df %>% left_join(df1, by = c( "variable" = "Country"))
    return(df)
    
  })
  
  p <- ggplot(gdppc()) + my_theme +
    labs (x = "Decades", y = "Average Annual HDI Growth(%)", 
          title = "Average Annual HDI Growth(%) 1990-2015",
            color = "Countries") + 
    geom_line(mapping = aes(x = Decades, y = value, colour = HDLevel, group = variable))
 
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
         subtitle = "abc",
         y = "Human Development Index", x = "Gross National Income per Capita")
  
  if("Overall" %in% input$Gender){
    p <- p + geom_point(aes(x = Gross.national.income..GNI..per.capita, y = HDI), color = "black") 
  }
  #if("Overall" %in% input$Gender && input$regression_line){
  #p <- p + geom_point(aes(x = Gross.national.income..GNI..per.capita, y = HDI), color = "brown") + 
  #geom_vline(xintercept = 0) + geom_smooth(data = world_hdi,aes(x = Gross.national.income..GNI..per.capita, y = HDI)) 
  #}
  
  if("Male" %in% input$Gender){
    p <- p + geom_point(aes(x = Estimated.gross.national.income.per.capita.Male, y = HDI), color = "dodgerblue2")
    
  }
  #if("Male" %in% input$Gender && input$regression_line){
  # p <- p + geom_point(aes(x = Estimated.gross.national.income.per.capita.Male, y = HDI), color = "pink") 
  #geom_smooth(aes(x = Estimated.gross.national.income.per.capita.Male, y = HDI))
  #}
  
  if("Female" %in% input$Gender){
    p <- p + geom_point(aes(x = Estimated.gross.national.income.per.capita.Female, y = HDI), color = "firebrick2")
    
  } 
  #if("Female" %in% input$Gender && input$regression_line){
  #p <- p + geom_point(aes(x = Estimated.gross.national.income.per.capita.Female, y = HDI), color = "orange") 
  #geom_smooth(aes(x = Estimated.gross.national.income.per.capita.Female, y = HDI),method = 'loess')
  #}
  
  p <- p + xlim(-2, 100000)
  p_plotly <- ggplotly(p, height = 400)
  return(p_plotly)

})
############################################################################
# health histogram 
############################################################################
output$health_histogram <- renderPlotly({
  sub <- reactive({
    sub <- world_hdi %>% filter(HDLevel %in% input$HDLevel)
    return(sub)
  })
  sub_data <- sub()
  temp <- sub_data %>% dplyr::select(Physicians...per.10.000.people..2001.2014,
                       Deaths.due.to.Tuberculosis..per.100.000.people..)
  temp <- mutate(temp, Physicians = cut(Physicians...per.10.000.people..2001.2014,
                                        c(-1, 5, 10, 20, Inf),
                                labels = c("Extrmely Low (<5)","Low(<10)",
                                           "Moderate (<20)", "High(<infinity)")),
         Tuberculosis = cut(Deaths.due.to.Tuberculosis..per.100.000.people..,
                            c(-1, 10, 20, 40, 60, Inf),
                            labels = c("Extremely Low","Low",
                                       "Mid", "Slightly Large","Significantly Large")))
  
  if (input$health_choice == "General"){
      p <- ggplot(data = sub_data) + geom_histogram(aes(x = Life.expectancy, fill = factor(HDLevel)), color = "black",
                                                     bins = as.numeric(input$n_breaks)) + 
        labs(title = "Life Expectancy over HDI")
     
  } else if (input$health_choice == "Tuberculosis"){
    subtemp = mutate(sub_data, Tuberculosis = cut(Deaths.due.to.Tuberculosis..per.100.000.people..,
                                      c(0, 1, 2, 13, Inf),
                                      labels = c("Extremely Low","Low",
                                                  "Mid", "Significantly Large")))
    
    p <- ggplot(data = subtemp) +
          geom_histogram(aes(x = Life.expectancy, fill = HDLevel), color = "black",
                            bins = as.numeric(input$n_breaks)) + 
      facet_wrap(~subtemp$Tuberculosis) + 
      labs(title = "Life Expectancy over HDI by Deaths due to Tuberculosis per 10,000 people")
    
    } else if (input$health_choice == "Physicians"){
      subtemp = mutate(sub_data, Physicians = cut(Physicians...per.10.000.people..2001.2014,
                                                 c(-1, 5, 10, 20, Inf),
                                                 labels = c("Extrmely Low (<5)","Low(<10)",
                                                            "Moderate (<20)", "High(<infinity)")))
      
      p <- ggplot(data = subtemp) +
        geom_histogram(aes(x = Life.expectancy, fill = HDLevel), color = "black",
                       bins = as.numeric(input$n_breaks)) + 
        facet_wrap(~subtemp$Physicians) + 
        labs(title = "Life Expectancy over HDI by Deaths due to Tuberculosis per 10,000 people") 
    } 
  
  colors <- c("firebrick2", "dodgerblue2", "chocolate", "aquamarine4")
  names(colors) <-  c("Very High Human Development", "High Human Development",
                      "Medium Human Development", "Low Human Development")
  p <- p + scale_fill_manual(values= colors)
  p_plotly <- ggplotly(p, height = 400)
  return(p_plotly)
  })

############################################################################
# education scatter plot (plotly playable)
############################################################################

output$education_plotly_playable <- renderPlotly({
  if (input$education_choice == "General"){
    p_gen <- ggplot(world_hdi, 
                    aes(x = `Mean.years.of.schooling`,
                        y = `HDI`,
                        color = continent,
                        text = `Country`)) +
      geom_point(
        #frame = year
      )  + 
      labs(x = "Mean Years of Schooling",
           y = "HDI",
           color = "Continent",
           title = "HDI vs. Mean Years of Schooling") +
      my_theme + scale_color_manual(values = cb_pal) 
    
  } else if (input$education_choice == "Female"){
    p_gen <- ggplot(world_hdi, 
                    aes(x = `Mean.years.of.schooling.Female`, 
                        color = continent,
                        text = `Country`)) +
      geom_point(aes(y = `HDI`
                     #frame = year
      )) + 
      labs(x = "Mean Years of Schooling of Female",
           y = "HDI",
           color = "Continent",
           title = "HDI vs. Mean Years of Schooling of Female") +
      my_theme + scale_color_manual(values = cb_pal)
    
    
  } else {
    p_gen <- ggplot(world_hdi, 
                    aes(x = `Mean.years.of.schooling.Male`, 
                        color = continent,
                        text = `Country`)) +
      geom_point(aes(y = `HDI`
                     #frame = year
      )) + 
      labs(x = "Mean Years of Schooling of Male",
           y = "HDI",
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
  all_cont_with_cc <- world_hdi %>% dplyr::select("HDI",
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
  
  all_cont <-  all_cont_with_cc %>% dplyr::select("HDI",
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
                 aes(x = mds_coordinate_1, y = mds_coordinate_2)) +
    labs(title = "MDS Compression of Country Attributes",
         x = "MDS Coordinate 1",
         y = "MDS Coordinate 2",
         coloe = "Continent")
  
  only_point <- base +
    geom_point(aes(color = continent), size = 3) + 
    my_theme + scale_color_brewer(palette="Dark2")
  
  gg_with_dens <-
    base +
    geom_density2d(h = c(input$mds_bandwidth_x, input$mds_bandwidth_y)) +
    geom_point(aes(color = continent), size = 3) + 
    my_theme + scale_color_brewer(palette="Dark2")
  
  
  if (input$contourline){
    gg_with_dens + ylim(-5, 5)
  } 
  else {
    only_point + ylim(-5, 5)
  }
  
})


output$plotlydend <- renderPlot({
  
  
  library(dendextend)
  library(RColorBrewer)
  # cbbPalette <- c("#A7A7A7",
  #                 "dodgerblue",
  #                 "firebrick",
  #                 "forestgreen",
  #                 "gold")
  darkcols <- brewer.pal(8, "Dark2")
  
  get_colors <- function(x, palette = darkcols) palette[match(x, unique(x))]
  

   a = input$Var
  world_hdi_cont <- world_hdi[,a]
  # Physicians...per.10.000.people..2001.2014,
  # Employment.to.population.ratio....ages.15.and.older.)
  world_hdi_cont <- drop_na(world_hdi_cont)
  world_hdi_cont_scale <- world_hdi_cont %>% scale()
  dist_world_hdi <- world_hdi_cont_scale %>% dist()
  
  
  color_by = input$dend_color
  hc_world_hdi_complete <- dist_world_hdi %>% hclust %>% as.dendrogram %>% set("labels", world_hdi[, color_by], order_value = TRUE) %>%
    set("labels_col", get_colors(world_hdi[, color_by]), order_value = TRUE) %>% set('branches_lwd', 0.6) %>%
    set("labels_cex", c(.6,1.2))
  p <- hc_world_hdi_complete %>% ggplot() +
    labs(
      title = "Cluster Dendogram of Countries",
      y = "",
      x = "") +
    
    theme(axis.text = element_text(color = "darkslategrey"),
          text = element_text(color = "black", size = 20), legend.key.size = unit(5, "lines")
    )  + ylim(-6, 10)
  # library(ggraph)
  # hierarchy <- dist_world_hdi %>% hclust %>% as.dendrogram %>% set("labels", world_hdi$continent, order_value = TRUE)
  # p <-  ggraph(hierarchy, layout = "dendrogram") +
  #   geom_edge_elbow() +
  #   geom_node_text(aes(label = label), size = 3,
  #                  angle = 45, hjust = 1)  + my_theme + 
  #   labs(x = "actors", y = "Pairwise Euclidean Distance", 
  #        title = "Cluster Dendogram of characters")
  
 return(p)
})
}
