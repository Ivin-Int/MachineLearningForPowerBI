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

#FullDemoML <- read_csv("~/Ivin International/Template and Demo/Demo/FullDemoML.csv")

# Demo

FullDemoPath <- "~/Ivin International/Template and Demo/Demo/FullDemoML.csv"


MLRegr <- function(FullDataSet, OutputType) {


  library(caret)
  library(skimr)
  library(rpart)
  library(caTools)
  library(readr)

  # Load Data

  FullDemoML <- read_csv(FullDataSet)

  FullDemoForForecast <- FullDemoML[,c(1,2,10)]
  splitDate <- paste0(FullDemoForForecast$Date,"-",FullDemoForForecast$Year)

  FullDemoForForecast$FullDate <- splitDate

  FullDemoForForecast <- FullDemoForForecast[,c(3,4)]

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
  coef_mod_regress$OnlineCampaign <- coef_mod_regress$OrdinaryCampaign *50

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

ActualVsPredict <- MLRegr(FullDemoPath, "finaldata")
ModelPrecision <- MLRegr(FullDemoPath, "modelprecision")
ModelCoef <- MLRegr(FullDemoPath, "coef")
Forecast <- MLRegr(FullDemoPath,"FullDemoForForecast")

