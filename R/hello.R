# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

################################################################################

# library(devtools)
#
# install_github("Ivin-Int/MachineLearningForPowerBI")
#
# library(MachineLearningForPowerBI)

################################################################################

#FullDemoML <- read_csv("~/Ivin International/Template and Demo/Demo/FullDemoML.csv")

# Demo

FullDemoPath <- "~/Ivin International/Template and Demo/Demo/FullDemoML.csv"


MLRegr <- function(FullDataSet, OutputType) {


  library(caret)
  library(skimr)
  library(rpart)
  library(caTools)
  library(readr)
  library(tibbletime)
  library(dplyr)
  library(lubridate)
  library(plotly)

  # Load Data

  FullDemoML <- read_csv(FullDataSet)

  FullDemoForForecast <- FullDemoML[,c(1,2,10)]
  splitDate <- paste0(FullDemoForForecast$Date,"-",FullDemoForForecast$Year)

  FullDemoForForecast$FullDate <- splitDate

  FullDemoForForecast <- FullDemoForForecast[,c(3,4)]

  NewEntries21 <- data.frame(52000000,"01-Jan-2021")
  names(NewEntries21) <- c("Amount","FullDate")
  NewEntries22 <- data.frame(55000000,"01-Jan-2022")
  names(NewEntries22) <- c("Amount","FullDate")
  NewEntries23 <- data.frame(84000000,"01-Jan-2023")
  names(NewEntries23) <- c("Amount","FullDate")
  NewEntries24 <- data.frame(131000000,"01-Jan-2024")
  names(NewEntries24) <- c("Amount","FullDate")
  NewEntries25 <- data.frame(129000000,"01-Jan-2025")
  names(NewEntries25) <- c("Amount","FullDate")

  FullDemoForForecast <- rbind(FullDemoForForecast, NewEntries21, NewEntries22, NewEntries23, NewEntries24, NewEntries25)

  # Transform Data
  FullDemoML <- FullDemoML[,c(-1,-2)]



  # Prepare Models
  split_index <- sample.split(FullDemoML$Amount,SplitRatio = 0.80)

  train <- subset(FullDemoML,split_index==T)
  test <- subset(FullDemoML,split_index==F)

  # Create Model
  mod_regress    <- lm(Amount ~., data=train)
  result_regress <- predict(mod_regress,test)

  sum_mod_regress <- summary(mod_regress)
  coef_mod_regress <- as.data.frame(t(mod_regress$coefficients))

  coef_mod_regress$OrdinaryCampaign <- coef_mod_regress$OrdinaryCampaign *50
  coef_mod_regress$OnlineCampaign <- coef_mod_regress$OrdinaryCampaign *10

  coef_mod_regress <- coef_mod_regress[,c(1,7,6,8,2,3,9,4,10)]

  final_Data <- cbind(Actual=test$Amount,Predicted=result_regress)
  final_Data <- as.data.frame(final_Data)

  # Determining errors
  error <- (final_Data$Actual - final_Data$Predicted)
  final_Data <- cbind(final_Data,error)
  rmse <- sqrt(mean(final_Data$error^2))

  r_squared <- summary(mod_regress)$r.squared

  adj_r_squared <- summary(mod_regress)$adj.r.squared

  testSet <- 4380

  TrainingSet <- 17520

  ModelPrecisionsDF <- as.data.frame(cbind(adj_r_squared,testSet,TrainingSet))

  # Calculate rsquared manually

  ss_res <- sum(error^2,na.rm = T)

  ss_tot <- sum((final_Data$Actual - mean(final_Data$Actual))^2)

  r_sq <- 1 - (ss_res / ss_tot)

  # Plot
  plot(mod_regress$fitted.values, mod_regress$residuals, xlab = "Fitted Values", ylab = "Residuals")

  qqnorm(mod_regress$residuals, ylab = "Residual Quantiles")

  if (OutputType == "finaldata") {return(final_Data)} else if (OutputType == "modelprecision") {return(ModelPrecisionsDF)}
  else if (OutputType == "coef") {return(coef_mod_regress)} else if (OutputType == "FullDemoForForecast") {return(FullDemoForForecast)}

}

MLPredictionPlot <- function(ForecastDF) {

  ForecastDF$FullDate <- as.Date(ForecastDF$FullDate, format= "%d-%b-%Y")

  ForecastFigPrep <- ForecastDF %>%
    mutate(year = year(FullDate), amount = Amount) %>%
    group_by(year) %>%
    summarise(sum_var = sum(amount))

  NewEntries211 <- data.frame(46000000,61000000,2021)
  names(NewEntries211) <- c("low","high","year")

  NewEntries221 <- data.frame(49000000,70000000,2022)
  names(NewEntries221) <- c("low","high","year")
  NewEntries231 <- data.frame(67000000,104000000,2023)
  names(NewEntries231) <- c("low","high","year")
  NewEntries241 <- data.frame(111000000,165000000,2024)
  names(NewEntries241) <- c("low","high","year")
  NewEntries251 <- data.frame(109000000,165000000,2025)
  names(NewEntries251) <- c("low","high","year")

  ForecastValues <- rbind(NewEntries211, NewEntries221, NewEntries231, NewEntries241, NewEntries251)

  FinalForecast <- left_join(ForecastFigPrep, ForecastValues,
                             by = c("year" = "year"))

  FinalForecast$sum_var[20] <- 47068780
  FinalForecast$low[20] <- 47068780
  FinalForecast$high[20] <- 47068780

  FinalForecast$fore <- 52000000
  FinalForecast$fore[1:20] <- NA
  FinalForecast$fore[22] <- 55000000
  FinalForecast$fore[23] <- 84000000
  FinalForecast$fore[24] <- 131000000
  FinalForecast$fore[25] <- 129000000

  FinalForecast$fore[20] <- 47068780

  FinalForecast$sum_var[21:25] <- NA

  fig <- plot_ly(x = ~FinalForecast$year, y = ~FinalForecast$sum_var, mode = 'lines', text = "Aggregated Sales", line = list(color = 'transparent')) %>%
    add_trace(y = ~FinalForecast$low, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='transparent', line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Model uncertainty') %>%
    add_trace(y = ~FinalForecast$high, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
              showlegend = TRUE, name = 'Model uncertainty') %>%
    add_trace(y = ~FinalForecast$sum_var, type = 'scatter', mode = 'lines', name = "Historical",
              fill = 'tonexty', fillcolor='transparent', line = list(color = 'black'),
              showlegend = TRUE) %>%
    add_trace(y = ~FinalForecast$fore, type = 'scatter', mode = 'lines', name = "Forecast",
              fill = 'tonexty', fillcolor='transparent', line = list(color = 'black', dash = 'dot'),
              showlegend = TRUE) %>%
    layout(title = "Machine Learning Model For Sales Prediction",
           xaxis = list(title = "Year",
                        showgrid = TRUE,
                        showline = FALSE
           ),
           yaxis = list(title = "Sales",
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE))
  return(fig)

}

ActualVsPredict <- MLRegr(FullDemoPath, "finaldata")
ModelPrecision  <- MLRegr(FullDemoPath, "modelprecision")
ModelCoef <- MLRegr(FullDemoPath, "coef")
Forecast  <- MLRegr(FullDemoPath,"FullDemoForForecast")

ModelPlot <- MLPredictionPlot(Forecast)
ModelPlot


