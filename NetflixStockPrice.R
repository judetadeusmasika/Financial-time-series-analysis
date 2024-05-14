###load the required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
#### import the dataset
###set the working directory
setwd("C:/Users/Baha/Downloads")
NFLX <-read.csv("NFLX.csv")
##display the first few observations of the dataset
head(NFLX)
##data preparation and cleaning
#glimpse the data to check its structure
glimpse(NFLX)
#check for the missing values in the dataset
colSums(is.na(NFLX))
#check the dimensionality of the dataset
dim(NFLX)
#check the class of the variables
sapply(NFLX, class)
#Visualizing the data can reveal trends, patterns, and outliers
# Convert Date column to Date type
NFLX$Date <- as.Date(NFLX$Date, format = "%m/%d/%Y")

ggplot(NFLX, aes(x = as.Date(Date), y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price", title = "Netflix Stock Prices Over Time")

#correlation to check the variables relationship
cr <- round(cor(NFLX[, c("Open", "High", "Low", "Close", "Volume")]),1) # rounded up the correlation to the first decimal
cr
library(ggcorrplot)
#Correlation using the ggcorrplot function
ggcorrplot(cr, hc.order = TRUE, type = "lower",
           title = "Correlation of netflix stock price variables", legend.title = "pearson 
\n corr",
           lab = TRUE, outline.color = "black", lab_size = 2,
           colors = c("#6D9EC1", "white", "#E46726"))
#summary statistics
summary(NFLX)
#examine the distribution of the dataset
# Plot histograms for numeric variables
ggplot(NFLX, aes(x = Close)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(x = "Close Price", y = "Frequency", title = "Distribution of Close Price")

options(scipen = 999)
ggplot(NFLX, aes(x = Volume)) +
  geom_histogram(binwidth = 1000000, fill = "blue", color = "black") +
  labs(x = "Volume", y = "Frequency", title = "Distribution of Volume")
# Remove duplicates
NFLX <- unique(NFLX)

####exploratory data analysis
library(gridExtra)
#close price
plot1 <-ggplot(NFLX, aes(x = as.Date(Date), y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price", title = "Netflix Stock Prices Over Time")

#adjusted close price
plot2 <-ggplot(NFLX, aes(x = as.Date(Date), y = Adj.Close)) +
  geom_line() +
  labs(x = "Date", y = "Adjusted Close price", title = "Netflix Stock Prices Over Time")

#low price 
plot3 <-ggplot(NFLX, aes(x = as.Date(Date), y = Low)) +
  geom_line() +
  labs(x = "Date", y = "Low Price", title = "Netflix Stock Prices Over Time")

#high price
plot4 <-ggplot(NFLX, aes(x = as.Date(Date), y = High)) +
  geom_line() +
  labs(x = "Date", y = "High Price", title = "Netflix Stock Prices Over Time")
#open price
plot5 <-ggplot(NFLX, aes(x = as.Date(Date), y = Open)) +
  geom_line() +
  labs(x = "Date", y = "Open Price", title = "Netflix Stock Prices Over Time")
# Combine plots into a grid
grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 3)
#boxplot of the close price
b1 <- boxplot(NFLX$Close, main = "Boxplot of Close Price", ylab = "Close Price", col = "skyblue")
#boxplot of the adjusted close price
b2 <- boxplot(NFLX$Adj.Close, main = "Boxplot of the adjusted Close Price", ylab = "Adjusted Close Price", col = "skyblue")
#continuation of the exploratory data analysis
# Bar plot for mean Close Price by month
NFLX$Month <- format(as.Date(NFLX$Date, format = "%m/%d/%Y"), "%m")
monthly_mean <- aggregate(Close ~ Month, data = NFLX, FUN = mean)

ggplot(monthly_mean, aes(x = Month, y = Close)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Month", y = "Mean Close Price", title = "Mean Close Price by Month")

############ Forecasting and predictive modelling approach
library(FSelector)
library(rpart)
library(caret)
library(rpart.plot)
library(data.tree)
library(caTools)
library(randomForest)
library(e1071)

########### RANDOM FOREST REGRESSION MODEL #############

#splitting data into training and testing data
set.seed(123)
sample = sample.split(NFLX$Close, SplitRatio = .80) #split the data using 80:20 ratio
train_data <- subset(NFLX, sample == TRUE) #create the train data
test_data <- subset(NFLX, sample == FALSE) #create the test data
dim(train_data) #show how many observations are in the train data
dim(test_data) #show no of observations in the test data
#You use the function prop.table() combined with table() 
#to verify if the randomization process is correct.
prop.table(table(train_data$Close))
prop.table(table(test_data$Close))
# run the model with the default parameters
#Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
set.seed(1234)
attach(NFLX)
#using the default setting
default_rf <- train(Close~Adj.Close + Open +Low +High +Volume,data = train_data, 
                    method = "rf", trControl = trControl) #call randomForest()function
print(default_rf)
#tune the mtry to optimize the best level
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10))
netflix_rf_mtry <- train(Close~Adj.Close + Open +Low +High +Volume,
                       data = train_data,
                       method = "rf",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE)
print(netflix_rf_mtry)
### run the random forest code
netflix_rf <- randomForest(Close~Adj.Close + Open +Low +High +Volume, data = train_data, mtry = 5, ntree = 200,
                         importance = TRUE)
print(netflix_rf)
plot(netflix_rf)#plot the random forest to show error gap n trend
#prediction for train data
prediction <- predict(netflix_rf, train_data)
#prediction for test data
prediction <- predict(netflix_rf, test_data)
print(prediction)
#check the variables with key predictor influence 
importance(netflix_rf) # importance of the class descriptors
varImpPlot(netflix_rf)
netflixPred<-predict(netflix_rf,newdata=test_data) #prediction
tab4 <-table(netflixPred, test_data$Close);tab4# confusion matrix
accuracy <- sum(diag(tab4))/sum(tab4);accuracy #prediction accuracy
error <- 1-accuracy;error #prediction error

################# TIME SERIES ANALYSIS #################

library(ggfortify)
library(ggthemes)
library(forecast)
library(tseries)
library(TSstudio)
library(tsibble)
ClosePrice<-NFLX %>% 
  select(Date,Close)
#convert to tsibble
NetflixXTS<- ClosePrice %>% as_tsibble(index = Date)
ts_plot(NetflixXTS, title="Netflix stock prices Time Series")
#convert to time series object
netflixts<- ts(ClosePrice$Close, start=c(2023,2),end=c(2024,1), frequency=12);netflixts
#plot the time series
autoplot(netflixts) + labs(x ="Year", 
                         y = "Average Netflix close stock price", 
                         title="Average Netflix stock price from 2023 to 2024")+
  theme_classic()
#SEASONAL PLOTS
ts_heatmap(netflixts)
ts_surface(netflixts)
ts_seasonal(netflixts, type="cycle")
ts_seasonal(netflixts, type="box")
#PREDICTING RATE with Simple models: Average, Naive, Seasonal Naive
#split data into train and test data
trainTS<-window(netflixts, start=c(2023,2), end=c(2023,10))
testTS<-window(netflixts, start=c(2023,11))
class(trainTS)
#PREDICTING RATE with Simple models: Average, Naive, Seasonal Naive
#Average: forecast will be equal to the average of past data**
m_mean<-meanf(trainTS, h= 12)
autoplot(m_mean)
accuracy(m_mean, testTS)
#Naive: forecast will be equal to the last observation**
# Mean forecast
m_naive<-naive(trainTS, h=12)
accuracy(m_naive, testTS)
#Drift: Extrapolating line between the first and last observation of same season**
drift = rwf(trainTS, drift = TRUE, h= 12)
accuracy(drift, testTS)

autoplot(trainTS)+
  autolayer(m_mean, series="Mean", PI=FALSE)+
  autolayer(m_naive, series="Naive", PI=FALSE)+
  autolayer(drift, series="Drift", PI=FALSE)+
  xlab('Year')+ylab('Average netflix stock prices')+
  ggtitle('Forecasts for Netflix stock prices')+
  guides(colour=guide_legend(title='Forecast'))
#############################################################
#Fit models
m_mean <- meanf(trainTS, h=12)
plot1<-autoplot(m_mean)+theme_classic()+
  ggtitle('mean with Confidence interval')
m_naive <- naive(trainTS, h=12)
plot2<-autoplot(m_naive)+theme_classic()+
  ggtitle('Naive with Confidence interval')
m_drift <- rwf(trainTS, drift=TRUE, h=12)
plot3<-autoplot(m_drift)+theme_classic()+
  ggtitle('Drift with Confidence interval')
m_simple <- rwf(trainTS, drift=FALSE, h=12)
plot4<-autoplot(m_simple)+theme_classic()+
  ggtitle('Simple naive with Confidence interval')
#combine the plots
grid.arrange(plot1,plot2,plot3,plot4, ncol = 2)

# Make forecasts
forecast_mean <- forecast(m_mean)
forecast_naive <- forecast(m_naive)
forecast_drift <- forecast(m_drift)
forecast_simple <- forecast(m_simple)
# Combine forecasts
combined_forecasts <- cbind(forecast_mean$mean,
          forecast_naive$mean, forecast_drift$mean,
          forecast_simple$mean)
colnames(combined_forecasts) <- c("Mean", "Naive", "Drift", "Simple")
# Plot forecasts for simple model
autoplot(combined_forecasts) +
  xlab('Year') + ylab('Netflix stock prices') +
  ggtitle('Forecasts for Netflix stock prices') +
  guides(colour=guide_legend(title='Forecast'))+
  theme_classic()

# Fit models
m_ses <- ses(trainTS, h=12)
m_ets_add <- ets(trainTS, model="AAN")
m_ets_mul <- ets(trainTS, model="MAM")
# Make forecasts
forecast_ses <- forecast(m_ses, h=12)
forecast_ets_add <- forecast(m_ets_add, h=12)
forecast_ets_mul <- forecast(m_ets_mul, h=12)
# Combine forecasts
combined_forecasts <- cbind(forecast_ses$mean, forecast_ets_add$mean)
colnames(combined_forecasts) <- c("SES", "ETS (Additive)")
# Plot forecasts
autoplot(combined_forecasts) +
  xlab('Year') + ylab('Netflix stock prices') +
  ggtitle('Forecasts for Netflix stock prices') +
  guides(colour=guide_legend(title='Forecast'))+
  theme_classic()
# Calculate accuracy of forecasts
accuracy_ses <- accuracy(forecast_ses, testTS)
accuracy_ets_add <- accuracy(forecast_ets_add, testTS)
accuracy_ets_mul <- accuracy(forecast_ets_mul, testTS)
# Print accuracy measures
print(accuracy_ses)
print(accuracy_ets_add)
print(accuracy_ets_mul)
# Accuracy for Mean method
accuracy(forecast_mean, testTS)
# Accuracy for Naive method
accuracy(forecast_naive, testTS)
# Accuracy for Drift method
accuracy(forecast_drift, testTS)