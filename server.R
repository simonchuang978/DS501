library (shiny)
library (ggvis)
library(plsdepot)
library(ggplot2)
library(visreg)
library(caret)
library(leaflet)
library(sp)
library(sf)

housing_data <- read.csv("real_estate_valuation_data_set.csv")
sandiego     <- read.csv("real_estate_valuation_data_set.csv")
colnames(sandiego)[6] <- "latitude"
colnames(sandiego)[7] <- "longitude"
sandiego["longitude"] <- -sandiego["longitude"]

server <- function(input, output) {
  
  output$selected_var <- renderText({ 
  #   paste("Selected: ", input$selected_var, "Correlation Analysis")
     
    scatter_x <- function(Y, X) {
      # build the housing model with single predictor
      hModel <- lm(Y ~ X, data=housing_data)
      
      #model_coef_x <- as.list(coef(hModel))
      scatterPlot <- qplot(X, Y, data=housing_data)+geom_point(colour = "#3366FF", size=0.5)
      sp_regline = scatterPlot+geom_abline(intercept=hModel[1]$coefficients[1],
                                           slope= hModel$coefficients[2], color="red")
      sp_smooth  = sp_regline+geom_smooth(method=lm, color='#2C3E50')
      output$plot1 <- renderPlot({
        sp_smooth
      })
      
    }
    
    # # Display correlation
    # The following is not good code. Line 42 is better.
    # output$selected_var <- renderText({
    #   paste(" Correlation =", round(cor(housing_data[,"Y.house.price.of.unit.area"],
    #                                    housing_data[, input$selected_var])), 2)
    # })

    # Draw the scatter plot based on the input$selected_var
    scatter_x(housing_data$Y.house.price.of.unit.area, housing_data[,input$selected_var])
  
  })

  # Display correlation
    output$selected_var <- renderText({
      paste(" Correlation =", round(cor(housing_data[,"Y.house.price.of.unit.area"],
                                      housing_data[, input$selected_var]), 2))
    })
  
  output$checkGroup <- renderText({ 
    paste("Checked : ", input$checkGroup)
    # input$checkGroup
  })
  
  # new leaflet code from sad_diego_viewer.

  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)

  # housing data in app.R without X5 and X6 column name changed
  scatter_analyze <- function(Y, X) {
    # build the housing model with single predictor
    hModel <- lm(Y ~ X, data=housing_data)

    #model_coef_x <- as.list(coef(hModel))
    scatterPlot <- qplot(X, Y, data=housing_data)+geom_point(colour = "#3366FF", size=0.5)
    sp_regline = scatterPlot+geom_abline(intercept=hModel[1]$coefficients[1],
                                         slope= hModel$coefficients[2], color="red")
    sp_smooth  = sp_regline+geom_smooth(method=lm, color='#2C3E50')
    sp_smooth
    
    }
  
  choice = data.frame(  "X variable" = c(
                                        "X1.transaction.date",
                                        "X2.house.age", 
                                        "X3.distance.to.the.nearest.MRT.station",
                                        "X4.number.of.convenience.stores",
                                        "X5.latitude",
                                        "X6.longitude"),
                        "column" = c(2, 3, 4, 5, 6, 7)
                      )
  # get the boolean vector, select that row and the column index the row has
  # ??? can only be done from a reactive or observer function
  #
  col = choice[choice["X.variable"] == "X1.transaction.date", "column"]
  
  # scatter_analyze(housing_data$Y.house.price.of.unit.area, housing_data[,col[1]])
  output$plot1 <- renderPlot({scatter_analyze(housing_data$Y.house.price.of.unit.area, housing_data[,input$selected_var])
  })
  
  # predicted price processing
  housing_data <- read.csv("real_estate_valuation_data_set.csv")
  
  
  #x <- housing_data[3:7]
  #y <- housing_data[8]
  #cor(x, y)
  #                                       Y.house.price.of.unit.area
  #X1.house.transaction.date                               0.0874906 (low)
  #X2.house.age                                           -0.2105670 (low)
  #X3.distance.to.the.nearest.MRT.station                 -0.6736099
  #X4.number.of.convenience.stores                         0.5710049
  #X5.latitude                                             0.5459911
  #X6.longitude                                            0.5235417
  
   output$mymap <- renderLeaflet({
    # default the X variable
    formula_string <- ""
    if (length(input$checkGroup) == 0){
        formula_string = "X1.transaction.date"
    } else {
        # compose the X part of the formula from check box
        for (i in 1:length(input$checkGroup)) {
            formula_string = paste(
            formula_string,
            toString(choice[input$checkGroup,"X.variable"][as.integer(i)]),
              "+")
        }
        # rid of the extraneous + to complete the formula
        formula_string = substr(formula_string,1,nchar(formula_string)-1)
    }

    # compose the Y part of the formula string
    formula_string = paste("Y.house.price.of.unit.area ~", formula_string)

    # create the formula object
    formula = as.formula(formula_string)

    # creat the linear model based on the formula
    housing_model <- lm(formula,
                        data=housing_data)

    # # colnames(housing_data)
    # housing_model <- lm(Y.house.price.of.unit.area ~
    #                     X3.distance.to.the.nearest.MRT.station +
    #                     X4.number.of.convenience.stores,
    #                     data=housing_data)

    model_coef <- as.list(coef(housing_model))

    # print(model_coef)

    housing_predictor <- function ( x3, x4) {
      model_coef[["X3.distance.to.the.nearest.MRT.station"]] * x3 +
        model_coef[["X4.number.of.convenience.stores"]] * x4 +
        model_coef[["(Intercept)"]]
    }

    # fill in the predicted value for each house
    housing_data[,"predict"] <- fitted(housing_model)
    #print(head(housing_data[,"predict"]))

    # compute RMSE for output 7/20
    # split housing_data as Training and Test sets
    set.seed(2012)
    splitHousing <- caret::createDataPartition (housing_data[,1], p = 0.8, list=F, times=1)

    # Training data set
    trainHousing <- housing_data[splitHousing, ]
    head(trainHousing)

    # Test data set
    testHousing <- housing_data[!row.names(housing_data) %in% row.names(trainHousing),]
    testHousing <- testHousing[-splitHousing,]

    # library(Metrics)
    # RMSEP Root - Mean Squared Error of Prediction
    # Predicted Residual Sum of Squares
    PRESS = sum((housing_data[,"Y.house.price.of.unit.area"] -
                   housing_data[,"predict"])^2)
    RMSEP = sqrt(PRESS/nrow(housing_data))
    #print("RMSEP=")
    #print(RMSEP)

    # SST Sum of Squares of difference between individual mean
    # Note: Word could make normal quote, apostrophe, and minus to ", ', - (dash)
    #
    m = mean(housing_data[,"Y.house.price.of.unit.area"])
    housing_data[, "Mean"] = 0.0
    housing_data[, "Mean"] = m
    housing_data[, "St"] = 0.0
    housing_data[, "St"] = housing_data[,"Y.house.price.of.unit.area"] - m
    SST = sum((housing_data[,"St"])^2)
    #        print("SST=", SST)

    # R2
    R2 = 1- (PRESS/SST)
    #print ("R2= ")
    #print (R2)

    output$checkGroup <- renderText({
       paste("R2= ", print(round(R2,2)))
     })

    # End of RMSEP computation

    # fill in Leaflet info
      # convert lat lon pairs to label
      # The following code has side effect
      # Run time error on the Web app server.

      latLonVec = c()
      latLonVec <- sprintf(
        "<B>TR Price=%f Pred Price = %f<br>Dist to MRT=%f,# Stores=%d</B><br>Lat=%f Lon=%f",
        sandiego$Y.house.price.of.unit.area,
        housing_data$predict,
        sandiego$X3.distance.to.the.nearest.MRT.station,
        sandiego$X4.number.of.convenience.stores,
        sandiego$latitude,
        sandiego$longitude)
        m <- leaflet()
        m <- addTiles(m)
        m <- addMarkers(m, data = sandiego, lng =~longitude, lat=~latitude,
               # the following displays label in two lines
               # and considers the <br> in the latLonVec
               label= lapply(latLonVec, htmltools::HTML),
               labelOptions = labelOptions(style = list("font-weight" = "normal",
                         padding = "3px 8px"),
                         textsize = "15px",
                         direction = "auto")
             )
        #m
   })
   # 
}
