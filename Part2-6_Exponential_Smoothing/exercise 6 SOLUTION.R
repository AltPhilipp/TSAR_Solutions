##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Exercises 6: Exponential Smoothing
# gwendolin.wilke@fhnw.ch
##############################################


#### Task 1 - Simple exponential smoothing

# a) For the optimal smoothing parameter provided by the ETS() function, 
#    calculate the the weights for the last 6 months used in the weighted average form.


# extract the optimal alpha value from the model
pigs_Victoria <- aus_livestock  %>% 
  filter(Animal == "Pigs", State == "Victoria")
fit <-  pigs_Victoria %>% 
  model(ses = ETS(Count ~ error("A") + trend("N") + season("N")))
alpha = tidy(fit)$estimate[1] 

# calculate the first 6 weights
weights = c(alpha, alpha*(1-alpha), alpha*(1-alpha)^2, alpha*(1-alpha)^3, alpha*(1-alpha)^4, alpha*(1-alpha)^5)


# b) Using the last 6 values of the series shown with tail()on the last slide, 
#    estimate the value of the 1-step forecast by calculating the first 6 terms of the infinite sum 
#    Compare it to the corresponding forecast value provided by the forecast()function above. Why is it so much lower?

# extract the last 6 values of ghe time series
last_6_values = filter(pigs_Victoria, Month >= yearmonth("2018 Jul"))$Count

# estimate the 1-step forecast
sum(weights*last_6_values) # 88299.44

# compare with the forecast:
forecast(fit, h = 1) # 95187

# Why is it so much lower?
#     The values weights are becoming small very quickly when we go back in time. 
#     However, the weighted average form is an infinite sum, and a lot of small values can add up to a large value.
#     The forecast function uses the infinite sum to calculate the forecast, while the manual calculation only uses the first 6 terms of the sum.

# c) Now calculate the 1-step forecast as the 𝛼-weighted average of the last actual value and 
#    the last fitted value:
  
tail(pigs_Victoria)
augment(fit) %>% tail()

# calculate the 1-step forecast
forecast = alpha * tail(pigs_Victoria)$Count + (1-alpha) * tail(augment(fit))$.fitted
forecast[6] # 95186.56


#### Task 2 - Holt Winter's Method

# plot the Gas time series
aus_production |> autoplot(Gas)

# We fit a Holt-Winters model with additive trend and multiplicative seasonality,as well as
# a Holt-Winters model with damped additive trend and multiplicative seasonality
fit <- aus_production %>% 
  model(
    hw = ETS(Gas ~ error("M") + trend("A") + season("M")),
    hwdamped = ETS(Gas ~ error("M") + trend("Ad") + season("M")),
  )

# Why is multiplicative seasonality necessary here? 
# Because the seasonal variations increase drastically with the level

# Look at the evaluation metrics
fit %>%  glance()

# Which model has the better model fit? Why? 
# We select hw, its slightly a better fit: 
# - AICc and BIC are slightly smaller  
# - MSE is the same
# - MAE is slightly smaller

# Can think of a sensible reason why this model wins?
# Probably because the trend is very strong over most of the historical data.

# Check the residual plots of hw
fit %>% 
  select(hw) %>% 
  gg_tsresiduals()

# What can you conclude from it? Does the model adequately capture the patterns in the data?
# There is still some small correlations left in the residuals, showing the model has not fully captured the available information.
# There also appears to be some heteroskedasticity in the residuals with larger variance in the first half the series.


# Check the Ljung-Box test
fit |>
  augment() |>
  filter(.model == "hw") |>
  features(.innov, ljung_box, lag = 24)

# Make a forecast and plot the result
fit |>
  forecast(h = 36) |>
  filter(.model == "hw") |>
  autoplot(aus_production)

# Does it look reasonable?
# While the point forecasts look ok, the intervals are excessively wide.
