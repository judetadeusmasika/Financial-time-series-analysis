library(tidyverse)
library(tidyquant)
library(timetk)
library(caret)
library(modelr)
library(broom)
options(na.action = na.warn)
retail_p_day <-read.csv("Online_retail per day.csv")
head(retail_p_day)

### training and testing sets
retail_p_day <- retail_p_day %>%
  mutate(model = ifelse(day <= "2011-11-01", "train", "test"))
colnames(retail_p_day)[grep("^[0-9]+", colnames(retail_p_day))] <- paste0("P_", colnames(retail_p_day)[grep("^[0-9]+", colnames(retail_p_day))])
##Here, I am testing out timekit’s functions with the net income per day as response 
##variable. Because the time series in our data set is relatively short and doesn’t
##cover multiple years, this forecast will only be able to capture recurring variation 
##in days and weeks. Variations like increased sales before holidays, etc. would need
##additional data from several years to be accurately forecast.

##As we can see in the plot below, the net income shows variation between days.
