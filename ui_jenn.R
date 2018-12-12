############################################################################
# Leaflet: HTML Widget (embedded in Shiny)
############################################################################
# ui.R

library(tidyverse)
library(forcats)
library(reshape2)
library(gapminder)
library(shinythemes)
library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)
library(DT)
library(crosstalk)
library(shinydashboard)

# Define UI for application that draws a histogram

dashboardPage(
  skin = "red",
  
  dashboardHeader(title = "World HDI Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("globe")),
      menuItem("Pairwise Relation", tabName = "pr",icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      '))),
    tabItems(
      # First tab content
      tabItem(tabName = "overview",
              tabsetPanel(type = "tabs",
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
                                   
                                   
                                   plotlyOutput(outputId = "plotshiny", height = 500, width = 1000)
                          ),
                          tabPanel("MDS Plot",
                                   checkboxInput(inputId = "contourline",
                                                 label = strong("Show ContourLine"),
                                                 value = FALSE),
                                   
                                   plotOutput(outputId = "m_plot", height = "500px")
                          )
              )
      ),
      # Second tab content
      tabItem(tabName = "pr",
              tabsetPanel(type = "tabs",
                          tabPanel("Income VS. HDI",
                                   
                                   ***
                                   selectizeInput(inputId = "Gender",
                                                  label = "Choose Your Content Ratings",
                                                  choices = c("Overall", "Female", "Male") ,
                                                  selected = c("Overall", "Female", "Male"),
                                                  multiple = TRUE,
                                                  options = NULL),
                                   
                                   helpText("Overall = Brown, Male = Pink, Female = Orange"),
                                   
                                   plotlyOutput(outputId = "income_plotly", height = 500, width = 1000)
                                   ***
                                   #checkboxInput(inputId = "regression_line",
                                                 #label = strong("Show Regression Line"),
                                                 #value = FALSE),
                                  
                          ),
                          
                          
                          tabPanel("Education VS. HDI",
                                   selectInput(inputId = "education_choice",
                                               label = "Show general/female and male education",
                                               choices = c("General","Female","Male"),
                                               selected = 5),
                                   
                                   plotlyOutput(outputId = "education_plotly_playable")
                          ),
                          
                          tabPanel(
                            "Health vs. HDI",
                            plotlyOutput("health_histogram"),
                            
                            selectInput(inputId = "health_choice",
                                        label = "Show Relationship between Physicians/Tuberculosis",
                                        choices = c("General","Physicians","Tuberculosis"),
                                        selected = "General"),
                            
                            selectInput(inputId = "Regions",
                                        label = "Different Regions based on HDI Level",
                                        choices = c("Low Human Development","Medium Human Development",
                                                    "High Human Development","Very High Human Development", "all"),
                                        selected = "all"),
                            
                            selectInput(inputId = "n_breaks",
                                        label = "Number of bins in histogram",
                                        choices = c(10, 20, 35, 50),
                                        selected = 20)
                          )
              )
      ) 
    )
  )
)


