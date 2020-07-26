library(shiny)
library(leaflet)
library(dplyr)
library(ggvis)
library(rgdal)
library(quantmod)

# From case study 3 
library(plsdepot)
library(ggplot2)
library(visreg)
library(caret)
library(leaflet)
library(sp)
library(sf)

#housing_data <- read.csv("real_estate_valuation_data_set.csv")
# sandiego     <- read.csv("real_estate_valuation_data_set.csv")
# colnames(sandiego)[6] <- "latitude"
# colnames(sandiego)[7] <- "longitude"
# sandiego["longitude"] <- -sandiego["longitude"]


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
# Run the app
shinyApp(ui=ui, server=server)

