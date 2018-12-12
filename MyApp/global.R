# global.R

# Notes:
# ------
# Where you can prepare all data that isn't reactive, clean up things and more.
# 
# Note: you could also do this preparation elsewhere, especially if it takes 
# some time to run and just save it as a .Rdata file and load it in this file


# Libraries

library(tidyverse)
library(forcats)
library(reshape2)

library(gapminder)

library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)

library(crosstalk)


### colors and theme

cb_pal = c("#000000", "#E69F00", 
           "#56B4E9", "#009E73",
           "#F0E442", "#0072B2",
           "#D55E00", "#CC79A7")
cb_pal_cont = c("Africa" = "#000000",
                "Americas" = "#E69F00",
                "Asia" = "#56B4E9", 
                "Europe" = "#009E73",
                "Oceania" = "#F0E442")

my_theme = theme_minimal() +
  theme(axis.title  = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16), 
        plot.title = element_text(size = 20)
  )

############################################################################
# Choropleth Cleaning for Leaflet: HTML Widget (embedded in Shiny)
############################################################################
# The following code cleans up and filters the gapminder code to just focus 
#   2007 and have matching country names as that from the shape file pulled 
#   from `johan`'s github 
#
#   You will also notice I also set up the color range, and labels in this 
#   document so to not clutter up my server file. 

world_hdi <- read.csv("https://raw.githubusercontent.com/wacero666/36315-Interactive-Graphs/master/HDI.csv?token=ATCO4fxg736-wds9LCcI4ZMRg5yi7nqxks5cFYi8wA%3D%3D")
world_hdi <- world_hdi %>% 
  mutate(Country = as.character(fct_recode(as.character(Country),
                                           "Russia" = "Russian Federation",
                                           "Bolivia" = "Bolivia (Plurinational State of)",
                                           "Venezuela" = "Venezuela (Bolivarian Republic of)",
                                           "Dem. Rep. Congo" = "Congo (Democratic Republic of the)",
                                           "Iran" = "Iran (Islamic Republic of)",
                                           "Central African Rep."="Central African Republic",
                                           "S. Sudan" = "South Sudan",
                                           "Syria" = "Syrian Arab Republic",
                                           "Czech Rep." = "Czech Republic",
                                           "Korea" = "Korea (Republic of)",
                                           "Dem. Rep. Korea" = "Korea (Democratic People's Rep. of)",
                                           "Lao PDR" = "Lao People's Democratic Republic",
                                           "Vietnam" = "Viet Nam",
                                           "Tanzania" = "Tanzania (United Republic of)",
                                           "Dominican Rep." = "Dominican Republic",
                                           "Macedonia" = "The former Yugoslav Republic of Macedonia",
                                           "Bosnia and Herz." = "Bosnia and Herzegovina",
                                           "Moldova" = "Moldova (Republic of)",
                                           "Palestine" = "Palestine, State of"
                                           
  )))
world_hdi$Country <- as.character(world_hdi$Country)
world_hdi <- mutate(world_hdi, HDLevel = cut(HDI, c(0, 0.556, 0.7, 0.8, 1),
                                             labels = c("Low Human Development",  "Medium Human Development",
                                                        "High Human Development", "Very High Human Development")))
library(geojsonio)
world = geojson_read("countries.geojson", method = "local", what = "sp")


#Add Continent column
sub_world <- data.frame(name = world$name, continent = world$continent)

world_hdi <- world_hdi %>% left_join(sub_world, by = c("Country" = "name")) 


world_info <- data.frame(name = as.character(world$name) )
world_info$name <- as.character(world_info$name)

world_info <- world_info %>% left_join(world_hdi,
                                       by = c("name" = "Country"))
pal <- colorBin("YlOrRd", domain = world_info$HDI, na.color = "grey",
                bins = c(0,0.556, 0.7, 0.8, 1))

labels_world <- paste0(
  "<strong>", world_info$name, 
  "</strong><br/>HDI: ",
  world_info$HDI) %>% lapply(htmltools::HTML)


