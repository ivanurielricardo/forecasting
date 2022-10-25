rm(list=ls()) # Remove everything from your working environment

library(readxl)
library(ggplot2)
library(tidyverse)
library(bootUR)
dataset_exam <- read_excel("dataset_exam.xlsx")


###### Forecasting Production
# First we perform some sample statistics
dataset_exam <- data.frame(dataset_exam)
colnames(dataset_exam)
# dataset_exam$Time <- 1:nrow(dataset_exam)
# # Add cumsum column
# # dataset_exam$cumulative_boxes <- cumsum(dataset_exam$TotalBoxes)
# Take first differences of the data
# diff_cum_boxes <- diff(dataset_exam$cumulative_boxes)

# Datasets based on Employee ID
Emp1 <- dataset_exam %>%
  filter(EmpID==1)
Emp2 <- dataset_exam %>%
  filter(EmpID==2)
Emp3 <- dataset_exam %>%
  filter(EmpID==3)
Emp4 <- dataset_exam %>%
  filter(EmpID==4)
Emp5 <- dataset_exam %>%
  filter(EmpID==5)
Emp6 <- dataset_exam %>%
  filter(EmpID==6)
Emp7 <- dataset_exam %>%
  filter(EmpID==7)
Emp8 <- dataset_exam %>%
  filter(EmpID==8)
Emp9 <- dataset_exam %>%
  filter(EmpID==9)
Emp10 <- dataset_exam %>%
  filter(EmpID==10)
Emp11 <- dataset_exam %>%
  filter(EmpID==11)
Emp12 <- dataset_exam %>%
  filter(EmpID==12)


aggboxes <- dataset_exam$AGGBOXES
aggboxes <- aggboxes[!is.na(aggboxes)]



# Create various datasets that contain only the products that do not meet the criteria and those that do

failed_box <- dataset_exam %>%
  filter(Fail>0)
success_box <- dataset_exam %>%
  filter(Fail==0)

# Do the same for part-time employees and temporary employees

part_time_emp <- dataset_exam %>%
  filter(dPART==1)
ful_time_emp <- dataset_exam %>%
  filter(dPART==0)

temp_emp <- dataset_exam %>%
  filter(dTEMP==1)
non_temp_emp <- dataset_exam %>%
  filter(dTEMP==0)

# Plot the data
# Total Boxes
dataset_exam %>%
  ggplot(aes(x = AGGDATE, y = AGGBOXES)) + 
  geom_line()

# Boxes per hour
dataset_exam %>%
  ggplot(aes(x = Time, y = Boxhrs)) + 
  geom_line()

###### Mean Model
# Mean of total boxes and box per hour
mean_total <- mean(dataset_exam$AGGBOXES)
mean_perhour <- mean(dataset_exam$Boxhrs)

# Split into Training and Testing sets
train_total <- aggboxes[1:(length(aggboxes)/2)]
mean(train_total)
mean_model <- rep(mean(train_total), length(train_total))

test_total <- aggboxes[414:length(aggboxes)]
test_total <- test_total[-length(test_total)]
mean(test_total)

# Calculating the MSE of the mean model
library(forecast)
library(MLmetrics)
RMSE(mean_model,test_total)


meanRMSE <- (test_total-mean(train_total))^2
# sum of values in a vector
meanRMSE <- sum(mean_model, na.rm=FALSE)
meanRMSE <- mean_model/4605
meanRMSE <- sqrt(mean_model)
####### Seasonal Naive

seasonal_naive <- snaive(train_total,h=length(test_total))
RMSE(seasonal_naive$mean,test_total)

plot(aggboxes, col="blue", xlab="Time", ylab="Total Boxes", main="Mean Model Forecast", type='l')
lines(seasonal_naive$mean, col="red", lwd=2)

###### Exponential Smoothing
time_train_boxes <- as.ts(train_total)
time_test_boxes <- as.ts(test_total)
ets_model = ets(time_train_boxes)
summary(ets_model)

ets_forecast = forecast(ets_model, h=length(test_total))
RMSE(ets_forecast$fitted,test_total)

plot(test_total, col="blue", xlab="Time", ylab="Total Boxes", main="Exp Smoothing Forecast", type='l')
lines(ets_forecast$fitted, col="red", lwd=2)

####### SARIMA model
arima_optimal = auto.arima(train_total, ic=c('bic'))
library(astsa)
sarima_forecast = sarima.for(train_total, n.ahead=length(test_total),
                             p=2,d=1,q=2,P=0,D=0,Q=0,S=0)
RMSE(sarima_forecast$pred, test_total)

plot(aggboxes, col="blue", xlab="Time", ylab="Total Boxes", main="ARIMA model", type='l')
lines(sarima_forecast$pred, col="red", lwd=2)

###### Proper forecast
time_series_boxes <- data.frame(dataset_exam$AGGDATE, dataset_exam$AGGBOXES)
time_series_boxes <- time_series_boxes[c(1:827),]

install.packages("xts")                      # Install & load xts package
library("xts")
data_ts <- xts(time_series_boxes$dataset_exam.AGGBOXES, time_series_boxes$dataset_exam.AGGDATE)

ets_model2 = ets(data_ts)
summary(ets_model2)

ets_forecast2 = forecast(ets_model2)

plot(ets_forecast2$fitted, col="blue", xlab="Time", ylab="Total Boxes", main="Exp Smoothing Forecast", type='l')
lines(ets_forecast$fitted, col="red", lwd=2)

time_boxes %>%
  stlf(lambda = 0, h = 36) %>%
  autoplot()

###### Analysis of production data

# Total Boxes given the box did not meet criteria

# dataset_exam %>%
#   ggplot(aes(x = 1:length(failed_box$TotalBoxes), y = failed_box$TotalBoxes)) + 
#   geom_line()

mean(failed_box$TotalBoxes)
mean(success_box$TotalBoxes)
max(failed_box$TotalBoxes)
max(success_box$TotalBoxes)

