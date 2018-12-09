############################################################################
# Leaflet: HTML Widget (embedded in Shiny)
############################################################################
# ui.R

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


navbarPage(
title = "World Analysis",
tabPanel("HTML Widgets -- leaflet",  
           leafletOutput(outputId = "leaflet", height = "600px")
  ),
  
 
tabPanel("HTML Widgets -- Dygraphs",
         selectizeInput(inputId = "country_subset", 
                        label = "Choose Your Content Ratings", 
                        choices = world_hdi$Country, 
                        selected = c("China", "United States", "United Kingdom",
                                     "Congo", "Albania","Bosnia and Herzegovia",
                                     "Poland"), 
                        multiple = TRUE,
                        options = NULL),
         
           
          plotlyOutput(outputId = "plotshiny", height = 400, width = 900)
  )
############################################################################
# Leaflet: HTML Widget (embedded in Shiny)
############################################################################ 






)
