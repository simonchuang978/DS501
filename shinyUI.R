library (shiny)
library (dplyr)
#library (ggvis)
library(plsdepot)
library(ggplot2)
library(visreg)
library(caret)

ui <- fluidPage(
    titlePanel("SD Real Estate Valuation Analysis Viewer"),
    fluidRow(
      column(3, 
          h4("User's Guide"),
          helpText(
            "This tool allows an analyst of the real esate transactions ", 
            "to build a Linear Model (LM) in three steps:", 
            span("Step 1:", style="font-size:12pt" ), 
                         "Select one of Xn parameters for Correlation Analysis. ",
            span("Step 2:",style="font-size:12pt" ),  
                         " Check all Xn parameters with high correlations to the price ",
            span("Step 3:", style="font-size:12pt"), 
                         "Display the RMSEP and R2 values of the built LM and Leaflet",
            "Map for browsing and comparison purposes.")
      )
    ),
    fluidRow(  # for Correlation Analysis
      sidebarLayout(
        sidebarPanel(
        helpText(h3("Step 1: Select one of Xn parameters for Correlation Analysis.")),

        # Display correlation value in front of selected var
        span(textOutput("correlation"), style="font-size:14pt"),
        
        # Display drop down list
        selectInput("selected_var",
          label = "Xn parameters correlating to Y house price of unit area",
          choices = c(
            "X1.transaction.date",
            "X2.house.age",
            "X3.distance.to.the.nearest.MRT.station",
            "X4.number.of.convenience.stores",
            "X5.latitude",
            "X6.longitude"),
            selected = "X1.trasaction.date"),
        
        # Display check boxes for Linear Regression Model parameters
        column(3,
             checkboxGroupInput("checkGroup",
                h3("Step2: Check one or more Xn var to build LM:"),
                choices = list(
                            "X1.transaction.date" = 1, 
                            "X2.house.age" = 2, 
                            "X3.distance.to.the.nearest.MRT.station" = 3,
                            "X4.number.of.convenience.stores" = 4,
                            "X5.latitude" = 5,
                            "X6.longitude" = 6),
                selected = 1)) # End of column
        ), # End of side bar Panel

        mainPanel(
      
          # The following objects are for server reactive
          # function to update and for debugging purposes
          span(textOutput("selected_var"), style="font-size:14pt"),
          span(textOutput("checkGroup"), style="font-size:14pt"),
      
          # Display scatter plot for selected_var for correlation analysis
          plotOutput("plot1")
        ) # end of mainPanel
      
    ) # end of sidebar layout
  ), # end of fluid row
  
  # Display leaflet map, markers, and labels
  leafletOutput("mymap")
  
) # end of UI fluid page
