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
 
  dashboardHeader(title = "Human Development Index Analysis",titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("globe")),
      menuItem("Pairwise Relation", tabName = "pr",icon = icon("th")),
      menuItem("Summary", tabName = "su", icon = icon("globe"))
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
                          tabPanel("Distribution of Human Development",
                                   selectInput(inputId = "n_breaks",
                                               label = "Number of bins in histogram (approximate):",
                                               choices = c(10, 20, 35, 50),
                                               selected = 20),
                                   
                                   checkboxInput(inputId = "individual_obs",
                                                 label = strong("Show individual observations"),
                                                 value = FALSE),
                                   
                                   checkboxInput(inputId = "density",
                                                 label = strong("Show density estimate"),
                                                 value = FALSE),
                                   
                                   # Display this only if the density is shown
                                   conditionalPanel(condition = "input.density == true",
                                                    sliderInput(inputId = "bw_adjust",
                                                                label = "Bandwidth adjustment:",
                                                                min = 0.2, max = 2, value = 1, step = 0.2)
                                   ),
                                   plotlyOutput(outputId = "plothis", height = 600, width = 1200)
                                   ),
                          
                          tabPanel("World Map of HDI" , 
                                   fluidRow(width=12,
                                            column(width = 6,
                                                  selectizeInput(inputId = "TopNo.", 
                                                                 label = "Select Top n Countries", 
                                                                 choices = world_hdi$HDI.Rank, 
                                                                 selected = 188, 
                                                                 multiple = FALSE,
                                                                 options = NULL)),
                                            column(width = 6,
                                                  selectizeInput(inputId = "Level", 
                                                                 label = "Select HDI Level", 
                                                                 choices = unique(world_hdi$HDLevel), 
                                                                 selected = c("Very High Human Development", "High Human Development",
                                                                              "Medium Human Development", "Low Human Development"), 
                                                                 multiple = TRUE,
                                                                 options = NULL
                                                                 
                                                  ))),
                                                  
                                     
                                   leafletOutput(outputId = "leaflet", height = "600px")
                          ),
                          
                          tabPanel("Average Annual HDI Growth By Countries",
                                   selectizeInput(inputId = "country_subset", 
                                                  label = "Select Countries", 
                                                  choices = world_hdi$Country, 
                                                  selected = c("China","Sudan", 
                                                               "Palestine","Jordan",
                                                               "Turkey", "Vietname", "Mali", "Iran",
                                                               "United States", "United Kingdom", "Norway"
                                                              
                                                               ), 
                                                  multiple = TRUE,
                                                  options = NULL),
                                   
                                   
                                   plotlyOutput(outputId = "plotshiny", height = 600, width = 1200)
                          )
                    
                          
              )
      ),
      # Second tab content
      tabItem(tabName = "pr",
              tabsetPanel(type = "tabs",
                          tabPanel("Income vs. HDI",
                                   
                                   selectizeInput(inputId = "Gender",
                                                  label = "Choose Your Content Ratings",
                                                  choices = c("Overall", "Female", "Male") ,
                                                  selected = c("Overall", "Female", "Male"),
                                                  multiple = TRUE,
                                                  options = NULL),
                                   
                                   helpText("Overall = Brown, Male = Pink, Female = Orange"),
                                   
                                   plotlyOutput(outputId = "income_plotly")
                          ),
                          
                          tabPanel("Education vs. HDI",
                                   selectInput(inputId = "education_choice",
                                               label = "Show general/female and male education",
                                               choices = c("General","Female","Male"),
                                               selected = 5),
                                   
                                   plotlyOutput(outputId = "education_plotly_playable")
                          ),
                          
                          tabPanel(
                            "Health vs. HDI",
                            plotlyOutput("health_histogram"),
                            checkboxGroupInput(inputId = "HDLevel",
                                          label = "",
                                          choices =  c("Very High Human Development", "High Human Development",
                                                       "Medium Human Development", "Low Human Development"),
                                          selected =  c("Very High Human Development", "High Human Development",
                                                        "Medium Human Development", "Low Human Development")),
                            
                            selectInput(inputId = "health_choice",
                                        label = "Show Relationship between Physicians/Tuberculosis",
                                        choices = c("General","Physicians","Tuberculosis"),
                                        selected = "General"),
                            
                            # selectInput(inputId = "Regions",
                            #             label = "Different Regions based on HDI Level",
                            #             choices = c("Low Human Development","Medium Human Development",
                            #                         "High Human Development","Very High Human Development", "all"),
                            #             selected = "all"),
                            
                            selectInput(inputId = "n_breaks",
                                        label = "Number of bins in histogram",
                                        choices = c(10, 20, 35, 50),
                                        selected = 20)
                          )
              )
      ),
  #Summery Tab
  tabItem(tabName = "su", 
          tabsetPanel(type = "tabs",
                      tabPanel("Contour Plot",
                               checkboxInput(inputId = "contourline",
                                             label = strong("Show ContourLine"),
                                             value = FALSE),
                               conditionalPanel(condition = "input.contourline == true",
                                                sliderInput(inputId = "mds_bandwidth_x",
                                                            label = "Bandwidth adjustment for x",
                                                            min = 1, max = 8, value = 3, step = 1),
                                                sliderInput(inputId = "mds_bandwidth_y",
                                                            label = "Bandwidth adjustment for y",
                                                            min = 1, max = 8, value = 3, step = 1)
                               ),
                               
                               
                               plotOutput(outputId = "m_plot", height = "500px")
                      ),
                      
                      
                      tabPanel("Dendrogram",
                               fluidRow(width=12,
                                        column(width = 6,
                                               selectizeInput(inputId = "dend_color", 
                                                              label = "Color By", 
                                                              choices = c("HDLevel", "continent"), 
                                                              selected = c("HDLevel"), 
                                                              multiple = FALSE,
                                                              options = NULL)),
                                        column(width = 6,
                                               selectizeInput(inputId = "Var", 
                                                              label = "Select Variables included in Dendrogram", 
                                                              choices = c("Life Expectancy" = "Life.expectancy", 
                                                                          "Gross National Income GNI per.capita" = "Gross.national.income..GNI..per.capita", 
                                                                          "Mean years of schooling" = "Mean.years.of.schooling",
                                                                          "Total Population" = "Total.Population..millions..2015",
                                                                          "Mortality Rates" = "Mortality.rates.Infant..per.1.000.live.births..2015"
                                                              ),
                                                              selected = c("Life.expectancy", 
                                                                           "Gross.national.income..GNI..per.capita", 
                                                                           "Mean.years.of.schooling"), 
                                                              multiple = TRUE,
                                                              options = NULL))),
                               plotOutput(outputId = "plotlydend", height = 800, width = 1600)
                               
                      )          
          )
  )
    )
  )
)


