## loading the necessary packages
library(tidyverse)
library(forecast)
library(tseries)
library(ggplot2)
library(tidyquant)
library(readxl)
library(gt)
library(zoo)
library(Metrics)
## loading and preparing the data (data preprocessing)
# set the working directory
setwd("C:/Users/Baha/Downloads")
airbnb <-read_excel("Airbnb.xlsx")
## display the first 10 observations of the data
airbnb |> 
  head(10) |> 
  gt()
# Create a mapping of quarters to months
airbnb$Month <- with(airbnb, ifelse(Quarter == "Q1", "01",
                           ifelse(Quarter == "Q2", "04",
                           ifelse(Quarter == "Q3", "07", "10"))))

# Combine Year and Month into a date string
airbnb$Date <- as.Date(paste(airbnb$Year, airbnb$Month, "01", sep="-"), "%Y-%m-%d")
## structure of the data
str(airbnb)
# Order the data by Date
airbnb <- airbnb[order(airbnb$Date),]
airbnb |> 
  head(10) |> 
  gt()
# Create a time series object
airbnb_ts <- ts(airbnb$`Revenue(MM$)`, start=min(airbnb$Date), frequency=4)
class(airbnb_ts)
# View the time series object
print(airbnb_ts)
## exploratory data analysis
attach(airbnb)
ggplot() +
  geom_line(data = airbnb, aes(x = Date, y = `Revenue(MM$)`)) +
  labs(
    title = "Air bnb Revenue in million dollars over time",
    x = "Date",
    y = "Revenue in million dollars"
  ) +
  scale_color_manual(values = palette_light()[[1]]) + theme_tq() +
  theme(plot.title = element_text(size = 20))
# Decompose the time series to observe trends and seasonality
airbnb_decomp <- decompose(airbnb_ts)
autoplot(airbnb_decomp) + ggtitle("Decomposition of Revenue Time Series")

## model identification and estimation
# identify the appropriate ARIMA model parameters (p, d, q)
# Automatically select the best ARIMA model
arima_model <- auto.arima(airbnb_ts)
# Display the summary of the model
summary(arima_model)
## model diagnostics
# Check the residuals of the fitted model to ensure that they behave like white noise
# Plot the residuals
checkresiduals(arima_model)
# Perform Ljung-Box test to check for autocorrelation
Box.test(arima_model$residuals, lag=20, type="Ljung-Box")
##model forecasting
##Once your model is validated, you can generate forecasts for the next five years (20 quarters)
# Forecast the next 20 quarters
forecasted_values <- forecast(arima_model, h=20)
# Plot the forecasts
autoplot(forecasted_values) + 
  ggtitle("Revenue Forecast for the Next 5 Years") + 
  xlab("Year") + 
  ylab("Revenue (MM$)")
# Print the forecasted values
print(forecasted_values)
summary(forecasted_values$x)
## model evaluation
## Calculate the performance metrics for your model
# Mean Absolute Error (MAE)
mae <- mean(abs(arima_model$residuals))
print(paste("Mean Absolute Error (MAE):", round(mae, 2)))

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(arima_model$residuals^2))
print(paste("Root Mean Squared Error (RMSE):", round(rmse, 2)))

# R-squared (using a custom function)
R2 <- function(arima_model) {
  1 - sum(arima_model$residuals^2) / sum((airbnb_ts - mean(airbnb_ts))^2)
}
r_squared <- R2(arima_model)
print(paste("R-squared:", round(r_squared, 2)))

