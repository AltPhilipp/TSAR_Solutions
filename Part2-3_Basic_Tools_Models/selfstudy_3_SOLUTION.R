##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Self-Study 3: Basic Tools and Models for Forecasting
# gwendolin.wilke@fhnw.ch
##############################################

library(fpp3)



# ------ Task1: Select Appropriate Benchmark Methods ------

# (a) Australian Population
global_economy |>
  filter(Country == "Australia") |>
  autoplot(Population)
# Data has trend and no seasonality. Random walk with drift model is appropriate.

global_economy |>
  filter(Country == "Australia") |>
  model(RW(Population ~ drift())) |>
  forecast(h = "10 years") |>
  autoplot(global_economy)

# (b) Australian Clay Brick Production
aus_production |>
  filter(!is.na(Bricks)) |>
  autoplot(Bricks) +
  labs(title = "Clay brick production")
# This data appears to have more seasonality than trend, so of the models available, seasonal naive is most appropriate.

aus_production |>
  filter(!is.na(Bricks)) |>
  model(SNAIVE(Bricks)) |>
  forecast(h = "5 years") |>
  autoplot(aus_production)

# (c) NSW Lambs
nsw_lambs <- aus_livestock |>
  filter(State == "New South Wales", Animal == "Lambs")

nsw_lambs |>
  autoplot(Count)
# This data appears to have more seasonality than trend, so of the models available, seasonal naive is most appropriate.

nsw_lambs |>
  model(SNAIVE(Count)) |>
  forecast(h = "5 years") |>
  autoplot(nsw_lambs)

# (d) Household wealth
hh_budget |>
  autoplot(Wealth)
# Annual data with trend upwards, so we can use a random walk with drift.

hh_budget |>
  model(RW(Wealth ~ drift())) |>
  forecast(h = "5 years") |>
  autoplot(hh_budget)


# ------ Task 2: Residual Diagnostics (Australian Beer Production) ------

### (a) Recreating all plots and outputs from exercise 3 task 5

# Extract data of interest
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

# Plot the data
autoplot(recent_production, Beer)

# Define and estimate a model
fit <- recent_production |> model(SNAIVE(Beer))

# Look at the model parameters
tidy(fit)

# Look at the fitted values 
augment(fit) %>% print(n=12)

# Look at the forecasts. We also show the original series and the fitted values on the plot.
fit |>
  forecast() |> # forecast
  autoplot(recent_production) + # original series
  autolayer(augment(fit), .fitted, color = "blue", linetype = "dashed") # fitted values

# Look at the residuals
fit |> gg_tsresiduals()

# Check the residual mean
augment(fit)$.innov |> mean(na.rm = TRUE) # -1.57



### (b-d) Fitting the seasonal naive model with drift and doing residual diagnostics

# Fit the model
fit_snd <- recent_production |> model(SNAIVE(Beer ~ drift()))

# Look at the model parameters
tidy(fit_snd) # drift parameter estimate looks sensible and is significant (p=41% >> 5%).

# Look at the fitted values 
augment(fit_snd) %>% print(n=12)

# Look at the forecasts. We also show the original series and the fitted values on the plot.
fit_snd |>
  forecast() |> # forecast
  autoplot(recent_production) + # original series
  autolayer(augment(fit_snd), .fitted, color = "blue", linetype = "dashed") # fitted values

# Check the residual mean
augment(fit_snd)$.innov |> mean(na.rm = TRUE) # zero mean, as expected, because we included the trend. Thus the model is unbiased.

# Look at the residual plots
fit_snd |> gg_tsresiduals()

### Residual diagnostics:

# The residuals may or may not be white noise (borderline case):
# - 1 large spike that is significant
# - 3 significant spikes in total (16% >> 5%)

# The model is not bad in capturing patterns in the data:
# - We saw that the residuals may or may not white noise
# - The model is unbiased (mean of residuals is zero)

# But we can NOT trust the prediction intervals:
# - The variance looks mostly constant (OK!)
# - BUT the histogram does not resemble a normal distribution: it is strongly right skewed.

### (d) Comparing with the seasonal naive method without drift
# The model does a better job in capturing the patterns present in the data,
# but we need to keep in mind that the prediction intervals may be distorted.


