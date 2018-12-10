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
                        label = "Select Countries", 
                        choices = world_hdi$Country, 
                        selected = c("China", "Mexico", "United States", "United Kingdom",
                                     "Congo","Sudan"), 
                        multiple = TRUE,
                        options = NULL),
         
           
          plotlyOutput(outputId = "plotshiny", height = 400, width = 900)
),


tabPanel("Scatter Plot GNI",

         sliderInput(inputId = "pointSize1",
                     label = "points' size adjustment:",
                     min = 0.5, max = 3, value = 1, step = 0.3),
         sliderInput(inputId = "pointSize2",
                     label = "points' size adjustment:",
                     min = 0.5, max = 3, value = 1, step = 0.3),
         selectizeInput(inputId = "Gender",
                        label = "Choose Your Content Ratings",
                        choices = c("Overall", "Female", "Male") ,
                        selected = c("Overall", "Female", "Male"),
                        multiple = TRUE,
                        options = NULL),
         plotlyOutput(outputId = "plot1", height = 400, width = 900)


),

tabPanel("Education VS. HDI",
         selectInput(inputId = "education_choice",
                     label = "Show general/female/male education ",
                     choices = c("General","Female","Male"),
                     selected = 5),
         
         plotlyOutput(outputId = "education_plotly_playable", 
                      height = 600,
                      width = 900)
),

tabPanel("Histogram",
         plotlyOutput("plot2", height = 250),
         selectInput(inputId = "n_breaks",
                     label = "Number of bins in histogram (approximate):",
                     choices = c(10, 20, 35, 50),
                     selected = 20)),
tabPanel("Countour Line",
  checkboxInput(inputId = "contourline",
                label = strong("Show ContourLine"),
                value = FALSE),
  
  plotOutput(outputId = "m_plot", height = "300px")
)



)
