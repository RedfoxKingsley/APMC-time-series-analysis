#' ---
#' title: "APMC/Mandi DS Task - Analysis"
#' #author: "Raam"
#' #date: "23 April 2018"
#' output: 
#'   html_document: 
#'     fig_caption: yes
#'     toc_depth: 4
#'     keep_md: true
#' ---
#' 
#' The overall objective of the task is two-fold:
#' 
#' 1. To understand the "trend" in the modal (average) prices of different commodities in different APMCs (Agricultural produce market committee),
#' 2. To predict the modal prices of different APMC-Commodity sets.
#' 
#' This file contains the **prediction** part.
#' 
## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

#' 
#' Note:
#' Please make sure to run the analysis part of this task (apmc_data_analysis.R) before running this file as this is a sequence to the operations performed in the analysis part.
#' 
#' ### Prediction of a single time series
#' 
#' It was found in the analysis part that our data has 278 time series (APMC-Commodity). So, first, a single time series is extracted and plotted to see visually how good the prediction works.
#' 
## ---- include=FALSE------------------------------------------------------
load("grouped_data1.Rdata")
load("data1_analysis.Rdata")


#' 
## ------------------------------------------------------------------------
library("forecast")
library("zoo")
# Extracting a time series
ind_data<-subset(grouped_data1, APMC == "Amarawati" & Commodity=="pigeon pea (tur)")

# Now, some of the series do not have values for certain in-between months. Hence such values have to replaced with NA.
ind_data <- ind_data[,c("date", "modal_price")]
ind_data$date <- as.Date(ind_data$date)

# creating a duplicate dataframe
ind_data_new <- data.frame(date=as.Date(character()),modal_price=as.integer())

# The below loop replaces any missing month-value pair with NAs.
library("lubridate")
for(i in 1: (nrow(ind_data)-1)){
    ind_data_new[nrow(ind_data_new) + 1,] = list(ind_data$date[i], ind_data$modal_price[i])
    
    if((month(ind_data$date[i+1]) - month(ind_data$date[i])) > 1){
        miss <- month(ind_data$date[i+1]) - month(ind_data$date[i]) - 1
        for(j in 1:miss){
            ind_data_new[nrow(ind_data_new) + 1,] = list(ymd(ind_data$date[i])%m+% months(j), 0)
        }
    }
}
ind_data_new[nrow(ind_data_new) + 1,] = list(ind_data$date[nrow(ind_data)], ind_data$modal_price[nrow(ind_data)])

# Converting it into a time series
ind_data_ts<-ts(ind_data_new$modal_price, frequency = 12, start = c(2014,9) )

# Fitting stl model
fit <- stl(ind_data_ts, s.window="periodic", na.action = na.approx) 

#' 
#' Regarding the selection of models: first, it is difficult to choose optimal parameters for ARIMA models for 278 time series without plotting ACF and PACF (Partial ACF) for each of them. Considering this, the STL model is chosen for our task. However, STL model handles only additive decomposition (if the series is not log-transformed). 
#' 
#' The results obtained so far indicates that there does not exist any significant difference between additive and multiplicative decomposition of (almost all of) our time series. **Hence, all the time series are assumed to have only additive components so that they can be used with STL models.** This is my assumption for prediction.
#' 
## ------------------------------------------------------------------------
#library(forecast)

# forecasting the prices for next three months.
fcast <- forecast(fit, h=3)

# summary(fcast)

#' 
#' The following plot shows the prediction (with 95% confidence interval) of modal prices for a single APMC-Commodity group for the next three months. The plot also compares the fitted values with the original values.
#' 
## ------------------------------------------------------------------------
library(scales)
library(ggplot2)
#library(data.table)
#library(tseries)

# a df for ploting forecast
ind_data <- ind_data[,c("date", "modal_price")]
fitted_data <- data.frame(date=as.Date(time(fcast$fitted)), Y = as.matrix(fcast$fitted))
predicted_data <- data.frame(date=as.Date(time(fcast$mean)), Y = as.matrix(fcast$mean))
predicted_data$upper <- as.matrix(fcast$upper[,2])
predicted_data$lower <- as.matrix(fcast$lower[,2])

ind_data$date <- as.POSIXct(ind_data$date)
fitted_data$date <- as.POSIXct(fitted_data$date)
predicted_data$date <- as.POSIXct(predicted_data$date)

x1 = fitted_data$date[nrow(fitted_data)]
y1 = fitted_data$Y[nrow(fitted_data)]
x2 = predicted_data$date[1]
y2 = predicted_data$Y[1]

ggplot()+
  geom_line(data=ind_data, aes(x=date, y=modal_price, color="original")) +
  geom_line(data=fitted_data, aes(x=date, y=Y, color="fitted")) +
  geom_line(data=predicted_data, aes(x=date, y=Y, color="predicted")) + 
  geom_ribbon(data=predicted_data,aes(x=date,ymin=lower,ymax=upper),alpha=0.3) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "predicted")) +
  scale_x_datetime(date_breaks = "3 month", labels = date_format("%m-%Y")) +
  xlab("Time period") +
  ylab("Modal price") +
  ggtitle("Time series plot for APMC = Amarawati & Commodity = pigeon pea (tur)") +
  theme(plot.title = element_text(hjust = 0.5))


#' 
#' ### Complete predictions
#' 
#' Similar to the above computation, the modal prices of all the APMC-Commodity groups are forecasted for the next three months (Dec 16 - Feb 17).
#' 
## ------------------------------------------------------------------------
final_predictions <- data1_analysis[,c("APMC", "Commodity")]
final_predictions$Dec16 <- NA
final_predictions$Jan17 <- NA
final_predictions$Feb17 <- NA

for(i in 1:nrow(final_predictions)){
  ind_data <- as.data.frame(subset(grouped_data1, APMC == as.character(final_predictions$APMC[i]) & Commodity==as.character(final_predictions$Commodity[i])))
  ind_data_ts<-ts(ind_data$modal_price, frequency = 12, start = c(2014,9))
  fit <- stl(ind_data_ts, s.window="periodic")
  fcast <- forecast(fit, h=3)
  final_predictions$Dec16[i] <- fcast$mean[1]
  final_predictions$Jan17[i] <- fcast$mean[2]
  final_predictions$Feb17[i] <- fcast$mean[3]
}

#' 
## ------------------------------------------------------------------------
# output for some APMC-Commodities
final_predictions[c(1:2,49:50),]

#' 
## ---- include=FALSE------------------------------------------------------
# save required dataframes
save(grouped_data1,file="final_predictions.Rdata")

#' 
#' 
