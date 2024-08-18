## load the required libraries
library(zoo)
library(ggplot2)
library(forecast)
library(Metrics)
######### data preprocessing for time series forecasting ########
#dataset loading and data type conversion
#set the working directory
setwd("C:/Users/Baha/Downloads")
passengers <-read.csv("airline-passengers.csv")
head(passengers)
#data type conversion
passengers$Month <-as.yearmon(passengers$Month)
max(passengers$Month)
# the first 6 observations of the airline passengers dataset
head(passengers)
#train and test datasets
#with time series data, we do not split the data randomly, time is a crucial component here and we want to 
#predict the future in a sequential manner
attach(passengers)
test_set_start_date <-as.yearmon("Jan 1959")
train_set <-subset(passengers, Month <test_set_start_date)
test_set <-subset(passengers, Month>=test_set_start_date)
dim(train_set) # 10 years for model training
dim(test_set) #2 years for model evaluation
ggplot() +
  geom_line(data = train_set, aes(x = Month, y = Passengers, color = "Training"), size = 1) +
  geom_line(data = test_set, aes(x = Month, y = Passengers, color = "Testing"), size = 1) +
  labs(
    title = "Airline Passengers - Training and Testing Sets",
    x = "Date",
    y = "Number of Passengers"
  ) +
  scale_color_manual(values = c("Training" = "#12355B", "Testing" = "#D72638"), name = "Airline passengers") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))
######## simple and naive forecasting
train_set_avg <-mean(passengers$Passengers)
simple_avg_predictions <-data.frame(
  Month = test_set$Month,
  Passengers = rep(train_set_avg, nrow(test_set))
)
ggplot() +
  geom_line(data = train_set, aes(x = Month, y = Passengers, color = "Training"), size = 1) +
  geom_line(data = test_set, aes(x = Month, y = Passengers, color = "Testing"), size = 1) +
  geom_line(data = simple_avg_predictions, aes(x = Month, y = Passengers, color = "Predicting"), size = 1) +
  labs(
    title = "Airline Passengers - predictions with simpe averaging method",
    x = "Date",
    y = "Number of Passengers"
  ) +
  scale_color_manual(values = c("Training" = "#12355B",
 "Testing" = "#D72638",
"Predicting" = "#32CD32"), name = "Airline passengers") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

## moving averages
passengers_ts <- ts(train_set$Passengers,frequency = 12)
passengers_ts
###create the model and forecasts for moving averages of window sizes 3, 6, and 12
ma3 <-ma(passengers_ts, order = 3, centre = F)
ma6 <-ma(passengers_ts,order = 6, centre = F)
ma12 <-ma(passengers_ts, order = 12, centre = F)
#forecasting
ma3_forecast <-forecast(ma3, h=nrow(test_set))
ma6_forecast <-forecast(ma6, h=nrow(test_set))
ma12_forecast <-forecast(ma12, h=nrow(test_set))
ma_forecast_df<-data.frame(
  Month=test_set$Month,
  MA3=ma3_forecast$mean,
  MA6=ma6_forecast$mean,
  MA12=ma12_forecast$mean
)
ma_forecast_df
ggplot() +
  geom_line(data = train_set, aes(x = Month, y = Passengers, color = "Training"), size = 1) +
  geom_line(data = test_set, aes(x = Month, y = Passengers, color = "Testing"), size = 1) +
  geom_line(data = ma_forecast_df, aes(x = Month, y = MA3, color = "MA(3) Predicted"), size = 1) +
  geom_line(data = ma_forecast_df, aes(x = Month, y = MA6, color = "MA(6) Predicted"), size = 1) +
  geom_line(data = ma_forecast_df, aes(x = Month, y = MA12, color = "MA(12) Predicted"), size = 1) +
  labs(
    title = "Airline Passengers - Predictions with moving averages",
    x = "Date",
    y = "Number of Passengers"
  ) +
  scale_color_manual(values = c(
    "Training" = "#12355B", 
    "Testing" = "#D72638", 
    "MA(3) Predicted" = "#FF6347", 
    "MA(6) Predicted" = "#32CD32",
    "MA(12) Predicted" = "#FFD700"
  ), name = "Airline passengers") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

############ exponential smoothing ########################
##simple exponential smoothing-no trend and seasonality in the data
##double exponential smoothing (holt's method)-trend, no seasonality
##triple exponential smoothing (holt's winter method)-trend and seasonality in the data
ses <-HoltWinters(passengers_ts, beta = FALSE, gamma = FALSE)
des <-HoltWinters(passengers_ts, gamma = FALSE)
tes <-HoltWinters(passengers_ts)
#forecasting
ses_forecast <-forecast(ses, h=nrow(test_set))
des_forecast <-forecast(des, h=nrow(test_set))
tes_forecast <-forecast(tes,h=nrow(test_set))
exsm_forecast_df <-data.frame(
  Month=test_set$Month,
  SES=ses_forecast$mean,
  DES=des_forecast$mean,
  TES=tes_forecast$mean
)
exsm_forecast_df
ggplot() +
  geom_line(data = train_set, aes(x = Month, y = Passengers, color = "Training"), size = 1) +
  geom_line(data = test_set, aes(x = Month, y = Passengers, color = "Testing"), size = 1) +
  geom_line(data = exsm_forecast_df, aes(x = Month, y = SES, color = "SES Predicted"), size = 1) +
  geom_line(data = exsm_forecast_df, aes(x = Month, y = DES, color = "DES Predicted"), size = 1) +
  geom_line(data = exsm_forecast_df, aes(x = Month, y = TES, color = "TES Predicted"), size = 1) +
  labs(
    title = "Airline Passengers - Predictions with Exponential Smoothing",
    x = "Date",
    y = "Number of Passengers"
  ) +
  scale_color_manual(values = c(
    "Training" = "#12355B", 
    "Testing" = "#D72638", 
    "SES Predicted" = "#FF6347", 
    "DES Predicted" = "#32CD32",
    "TES Predicted" = "#FFD700"
  ), name = "Airline passengers") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

tes_seasonal_add <- HoltWinters(passengers_ts, seasonal = "additive")
tes_seasonal_mul <- HoltWinters(passengers_ts, seasonal = "multiplicative")

tes_seasonal_add_forecast <- forecast(tes_seasonal_add, h = nrow(test_set))
tes_seasonal_mul_forecast <- forecast(tes_seasonal_mul, h = nrow(test_set))

exsm_tes_forecast_df <- data.frame(
  Month = test_set$Month,
  TES = tes_forecast$mean,
  TESAdd = tes_seasonal_add_forecast$mean,
  TESMul = tes_seasonal_mul_forecast$mean
)
exsm_tes_forecast_df

# Plot the forecasts
ggplot() +
  geom_line(data = train_set, aes(x = Month, y = Passengers, color = "Training"), size = 1) +
  geom_line(data = test_set, aes(x = Month, y = Passengers, color = "Testing"), size = 1) +
  geom_line(data = exsm_tes_forecast_df, aes(x = Month, y = TESAdd, color = "TES Additive Predicted"), size = 1) +
  geom_line(data = exsm_tes_forecast_df, aes(x = Month, y = TESMul, color = "TES Multiplicative Predicted"), size = 1) +
  geom_line(data = exsm_tes_forecast_df, aes(x = Month, y = TES, color = "TES Predicted"), size = 1) +
  labs(
    title = "Airline Passengers - Predictions with Triple Exponential Smoothing",
    x = "Date",
    y = "Number of Passengers"
  ) +
  scale_color_manual(values = c(
    "Training" = "#334252", 
    "Testing" = "#D72638", 
    "TES Additive Predicted" = "#FF6347", 
    "TES Multiplicative Predicted" = "#32CD32",
    "TES Predicted" = "#FFD700"
  ), name = "Airline passengers") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

################# ARIMA MODELS #################################################
ar_model <-Arima(passengers_ts, order = c(1,0,0))
ma_model <-Arima(passengers_ts, order = c(0,0,1))
arma_model <-Arima(passengers_ts, order = c(1,0,1))
arima_model <-Arima(passengers_ts, order = c(1,1,1))
auto_arima_no_season_model <-auto.arima(passengers_ts, seasonal = FALSE)
auto_arima_season_model <-auto.arima(passengers_ts, seasonal = TRUE)
#forecasts
ar_forecast<-forecast(ar_model, h=nrow(test_set))
ma_forecast <-forecast(ma_model, h=nrow(test_set))
arma_forecast <-forecast(arma_model, h=nrow(test_set))
arima_forecast <-forecast(arima_model,h=nrow(test_set))
auto_arima_no_season_forecast <-forecast(auto_arima_no_season_model, h=nrow(test_set))
auto_arima_season_forecast <-forecast(auto_arima_season_model, h=nrow(test_set))
#data frame
arima_forecast_df <-data.frame(
  Month=test_set$Month,
  AR=ar_forecast$mean,
  MA=ma_forecast$mean,
  ARMA=arma_forecast$mean,
  ARIMA=arima_forecast$mean,
  AutoARIMAnoSeason=auto_arima_no_season_forecast$mean,
  AutoARIMAseason=auto_arima_season_forecast$mean
)
arima_forecast_df
# Plot the data
ggplot() +
  geom_line(data = train_set, aes(x = Month, y = Passengers, color = "Training"), size = 1) +
  geom_line(data = test_set, aes(x = Month, y = Passengers, color = "Testing"), size = 1) +
  geom_line(data = arima_forecast_df, aes(x = Month, y = AR, color = "AR Predicted"), size = 1) +
  geom_line(data = arima_forecast_df, aes(x = Month, y = MA, color = "MA Predicted"), size = 1) +
  geom_line(data = arima_forecast_df, aes(x = Month, y = ARMA, color = "ARMA Predicted"), size = 1) +
  geom_line(data = arima_forecast_df, aes(x = Month, y = ARIMA, color = "ARIMA Predicted"), size = 1) +
  geom_line(data = arima_forecast_df, aes(x = Month, y = AutoARIMAnoSeason, color = "AutoARIMA no Seasonality Predicted"), size = 1) +
  geom_line(data = arima_forecast_df, aes(x = Month, y = AutoARIMAseason, color = "AutoARIMA with Seasonality Predicted"), size = 1) +
  labs(
    title = "Airline Passengers - Predictions with (S)ARIMA Models",
    x = "Date",
    y = "Number of Passengers"
  ) +
  scale_color_manual(values = c(
    "Training" = "#12355B", 
    "Testing" = "#D72638", 
    "AR Predicted" = "#FF6347", 
    "MA Predicted" = "#32CD32",
    "ARMA Predicted" = "#FFD700",
    "ARIMA Predicted" = "#6A5ACD",
    "AutoARIMA no Seasonality Predicted" = "#20B2AA",
    "AutoARIMA with Seasonality Predicted" = "#8A2BE2"
  ), name = "Airline passengers") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

########## how to evaluate tieme series forecasting models #######################
all_models_data <- data.frame(
  Month = test_set$Month,
  Passengers = test_set$Passengers,
  AVG = simple_avg_predictions$Passengers,
  MA = ma_forecast$mean,
  MA3 = ma3_forecast$mean,
  MA6 = ma6_forecast$mean,
  MA12 = ma12_forecast$mean,
  SES = ses_forecast$mean,
  DES = des_forecast$mean,
  TES = tes_forecast$mean,
  TESAdd = tes_seasonal_add_forecast$mean,
  TESMul = tes_seasonal_mul_forecast$mean,
  AR = ar_forecast$mean,
  ARMA = arma_forecast$mean,
  ARIMA = arima_forecast$mean,
  AutoARIMANoSeason = auto_arima_no_season_forecast$mean,
  AutoARIMASeason = auto_arima_season_forecast$mean
)

all_models_data
write.csv(all_models_data,"Forecasted_models_data.csv")
#onto evaluation now
mae_values <- c()
mape_values <- c()
rmse_values <- c()

for (col in names(all_models_data)[3:ncol(all_models_data)]) {
  mae_values <- c(mae_values, mae(all_models_data$Passengers, all_models_data[[col]]))
  mape_values <- c(mape_values, mape(all_models_data$Passengers, all_models_data[[col]]))
  rmse_values <- c(rmse_values, rmse(all_models_data$Passengers, all_models_data[[col]]))
}

model_test_set_metrics <- data.frame(
  Model = names(all_models_data)[3:ncol(all_models_data)],
  MAE = mae_values,
  MAPE = mape_values,
  RMSE = rmse_values
)
model_test_set_metrics
