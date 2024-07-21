###loading the required packages
library(mvgam)           # Fit, interrogate and forecast DGAMs
library(dplyr)           # Tidy and flexible data manipulation
library(forecast)        # Construct fourier terms for time series
library(ggplot2)         # Flexible plotting
theme_set(theme_bw())      # Black and White ggplot2 theme
library(gratia)          # Graceful plotting of smooth terms
library(marginaleffects) # Interrogating regression models
library(patchwork)       # Combining ggplot objects
library(janitor)         # Creating clean, tidy variable names

##load the data to be used
data("AirPassengers")
str(AirPassengers)
##plotting a seasonal time  series
ts.plot(AirPassengers, lwd=1.5,
col="darkred")
###One common way to inpsect possible seasonal patterns is to calculate and plot 
##an STL decomposition (Seasonal and Trend decomposition using Loess), which uses loess smoothing
##to iteratively smooth and remove certain components of the data (i.e. the long-term trend, 
##which may be nonlinear, and the potentially time-varying seasoanality)
stl(AirPassengers, s.window = 9) |> 
  autoplot()
#lets convert the time series data into a data frame, using the functions mvgam, 
# which will also automatically split the data into training and testing sets
air_data <-series_to_mvgam(AirPassengers, freq = frequency(AirPassengers))
dplyr::glimpse(air_data$data_train)
dplyr::glimpse(air_data$data_test)
##plot some features of the time series using some of {mvgam}’s S3 functions
plot_mvgam_series(data = air_data$data_train,
newdata = air_data$data_test)
##The Autocorrelation Function (ACF) plot clearly shows evidence of strong autocorrelation and seasonality,
##which we hope to capture in our models
########## Fit an mvgam model using a tensor product interaction #################
# Load cmdstanr library
library(cmdstanr)
# Install cmdstan
cmdstanr::check_cmdstan_toolchain(fix = TRUE)
cmdstanr::install_cmdstan()
model_1 <- mvgam(y ~ 
  # A tensor product of season and time
  te(season, time,
     
     # Cyclic and thin-plate marginal bases
     bs = c('cc', 'tp'),
     
     # Reasonable complexities
     k = c(8, 15)),

# Define where the seasonal ends should 'join'
knots = list(season = c(0.5, 12.5)),

# Allow for overdispersion in the count series
family = nb(),

# Define the data and newdata for automatic forecasting
data = air_data$data_train,
newdata = air_data$data_test,

# Silence printed messages
silent = 2)
model_1
#use gratia::draw()
#to view the ’time’ * ‘season’ smooth interaction function from the model’s underlying gam
#object (stored in the mgcv_model slot of the returned model object)
gratia::draw(model_1$mgcv_model)
##The primary seasonal pattern can be viewed using plot_predictions()
##from the {marginaleffects} package, which is an extremely 
##useful package for using targeted predictions to interrogate nonlinear effects from GAMs.
library(collapse)
plot_predictions(model_1, condition = "season", type = "link")
##how splines extrapolate beyond the training data
plot(model_1, type = "forecast")
##We are capturing the temporal dimension with a spline, and we all know that splines generally give poor extrapolation behaviours
##This model assumes that the seasonal pattern changes at the same rate that the temporal spline changes, and this might not always be the most suitable model
####################Using Fourier terms to capture time-varying seasonality##############
data.frame(forecast::fourier(AirPassengers, K = 4)) %>%
  # Use clean_names as fourier() gives terrible name types
  janitor::clean_names() -> fourier_terms
dplyr::glimpse(fourier_terms)
matplot(as.matrix(fourier_terms)[1:24,], type = 'l',
        ylab = 'Function value', xlab = 'Time',
        col = viridis::viridis(8), lty = 1, lwd = 1.5)
#add these new predictors to the training and testing datasets
air_data$data_train <-air_data$data_train |> 
  dplyr::bind_cols(fourier_terms[1:NROW(air_data$data_train),])
  air_data$data_test = air_data$data_test %>%
    dplyr::bind_cols(fourier_terms[(NROW(air_data$data_train) + 1):
                                     NROW(fourier_terms), ])
library(splines2)
model_2 <- mvgam(y ~ 
                                      # Monotonic smooth of time to ensure the trend
                                      # either increases or flattens, but does not 
                                      # decrease
                                      s(time, bs = 'moi', k = 10) +
                                      
                                      # Time-varying fourier coefficients to capture
                                      # possibly changing seasonality
                                      s(time, by = s1_12, k = 5) +
                                      s(time, by = c1_12, k = 5) +
                                      s(time, by = s2_12, k = 5) +
                                      s(time, by = c2_12, k = 5) +
                                      s(time, by = s3_12, k = 5) +
                                      s(time, by = c3_12, k = 5),
                                    
                                    # Response family and data as before
                                    family = nb(),
                                    data = air_data$data_train,
                                    newdata = air_data$data_test,
                                    silent = 2)
model_2
##model is preferred based on in-sample fits, which we can readily compare using the
##Approximate Leave-One-Out Cross-Validation with functionality from the wonderful {loo} package. A higher Expected Log Predictive Density (ELPD) is preferred here
library(loo)
loo_compare(model_1,model_2)
##out of sample forecasts
plot(model_1, type = "forecast")
plot(model_2, type = "forecast")
gratia::draw(model_2$mgcv_model, select = 2:7)
############# Plotting trend and seasonal components from a harmonic regression##############
p1 <-plot_predictions(model_2,
condition = "time",
type = "expected") +
  labs(y="Expected passengers",
x="time",
title = "Long-term trend")
p1
##And now the time-varying seasonal pattern, which requires subtracting the conditional trend predictions from the total predictions
with_season <- predict(model_2, summary = FALSE,
  type = 'expected')
agg_over_season <- predict(model_2, 
      newdata = datagrid(model = model_2,
                         time = unique),
      summary = FALSE,
      type = 'expected')
season_preds <- with_season - agg_over_season
p2 <- ggplot(air_data$data_train %>%
dplyr::mutate(pred = apply(season_preds, 2, mean),
        upper = apply(season_preds, 2, function(x) 
          quantile(x, probs = 0.975)),
        lower = apply(season_preds, 2, function(x) 
          quantile(x, probs = 0.025))),
aes(x = time, y = pred)) +
geom_ribbon(aes(ymax = upper,
ymin = lower),
alpha = 0.2) +
geom_line()+
labs(y = 'Expected passengers',
title = 'Time-varying seasonality')
p2
##We can now plot these effects together using functionality from the {patchwork} package
p1+p2+plot_layout(ncol = 1)
################### time varying periodicity #####################################
##First, I define function to construct monotonically increasing or decreasing coefficients. This will be helpful to simulate time-varying periodicity
monotonic_fun = function(times, k = 4){
  x <- sort(as.vector(scale(times)))
  exp(k * x) / (1 + exp(k * x))
}

set.seed(123)
times <- 1:120

# Construct Fourier series for two different periodicities
data.frame(forecast::fourier(ts(times, frequency = 12), K = 3)) %>%
  dplyr::bind_cols(forecast::fourier(ts(times, frequency = 6), K = 3)) %>%
  # Use clean_names as fourier() gives terrible name types
  janitor::clean_names() -> fourier_terms

# Create the time-varying Fourier coefficients
betas <- matrix(NA, nrow = length(times), ncol = NCOL(fourier_terms))

# Period 12 coefficients drop montonically toward zero over time
p12_names <- grep('_12', names(fourier_terms), fixed = TRUE)
for(i in p12_names){
  betas[, i] <- monotonic_fun(times, k = runif(1, -3, -0.25))
}

# Period 6 coefficients increase monotonically over time
p6_names <- grep('_6', names(fourier_terms), fixed = TRUE)
for(i in p6_names){
  betas[, i] <- monotonic_fun(times, k = runif(1, 0.25, 3))
}
##plot the fourier coefficients through time
matplot(betas, type = 'l', xlab = 'Time',
        col = viridis::viridis(8), lty = 1, lwd = 1.5)
#now calculate the linear predictor and plot it
mu <- vector(length = length(times))
for(i in 1:length(times)){
  mu[i] <- as.matrix(fourier_terms)[i, ] %*% betas[i, ] 
}
plot(mu, type = 'l', xlab = 'Time')
###Finally, add some Gaussian Random Walk noise and plot the resulting simulated time series
y <- mu + cumsum(rnorm(length(times), sd = 0.25))
plot(y, type = 'l', xlab = 'Time')
####Now we can construct the data for modeling; this assumes we’ve thought carefully about the problem and have included the Fourier series for both periodicities as predictors
dat <- data.frame(y, time = times) %>%
  dplyr::bind_cols(fourier_terms)
dat
###And we can now fit the dynamic GAM, which uses a State-Space representation to capture time-varying seasonality, periodicity and the Random Walk autocorrelation process
model_3 <- mvgam(
  
  # Observation formula; empty
  formula = y ~ -1,
  
  # Process formula contains the seasonal terms
  trend_formula = ~
    
    # Time-varying fourier coefficients to capture
    # possibly changing seasonality AND periodicity
    s(time, by = s1_12, k = 5) +
    s(time, by = c1_12, k = 5) +
    s(time, by = s2_12, k = 5) +
    s(time, by = c2_12, k = 5) +
    s(time, by = s3_12, k = 5) +
    s(time, by = c3_12, k = 5) +
    s(time, by = s1_6, k = 5) +
    s(time, by = c1_6, k = 5) +
    s(time, by = s2_6, k = 5) +
    s(time, by = c2_6, k = 5) +
    s(time, by = c3_6, k = 5),
  
  # Random walk for the trend
  trend_model = RW(),
  
  # Gaussian observations
  family = gaussian(),
  
  # Realistic priors on variance components
  priors = c(prior(exponential(2),
                   class = sigma),
             prior(exponential(2),
                   class = sigma_obs)),
  
  # Sampler settings
  burnin = 1000,
  samples = 1000,
  silent = 2,
  
  # Data
  data = dat)
#plot the time varying fourier coefficients
gratia::draw(model_3$trend_mgcv_model)

season_preds <- predict(model_3, summary = FALSE,
  type = 'expected')
p1 <- ggplot(dat %>%
dplyr::mutate(pred = apply(season_preds, 2, mean),
 upper = apply(season_preds, 2, function(x) 
   quantile(x, probs = 0.975)),
 lower = apply(season_preds, 2, function(x) 
   quantile(x, probs = 0.025))),
aes(x = time, y = pred)) +
geom_ribbon(aes(ymax = upper,
ymin = lower),
alpha = 0.2) +
geom_line() +
labs(y = 'Posterior expectations',
x = '',
title = 'Time-varying seasonality')
p1

trend_preds <- hindcast(model_3, type = 'expected')$hindcasts[[1]] - 
  season_preds

p2 <- ggplot(dat %>%
         dplyr::mutate(pred = apply(trend_preds, 2, mean),
                       upper = apply(trend_preds, 2, function(x) 
                         quantile(x, probs = 0.975)),
                       lower = apply(trend_preds, 2, function(x) 
                         quantile(x, probs = 0.025))),
       aes(x = time, y = pred)) +
  geom_ribbon(aes(ymax = upper,
                  ymin = lower),
              alpha = 0.2) +
  geom_line() +
  labs(y = 'Posterior expectations',
       x = 'Time',
       title = 'RW trend')
p2
p1 + p2 + plot_layout(ncol = 1)
