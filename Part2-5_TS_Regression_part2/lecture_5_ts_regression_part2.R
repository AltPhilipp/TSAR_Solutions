# ====================================
# Time Series Analysis with R
# gwendolin.wilke@fhnw.ch
# This script contains code used in 
# Part 2, Lecture 5 Time Series Regression Models (part 2)
# ==================================== 

library(fpp3)

# --------- Some useful predictors for time series regression  ---------


#### Modelling a Linear Trend in Regression 

# Filter the data to include only the years 1992 and later
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

# Plot the data
recent_production |>
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

# Fit a time series linear regression model to trend and seasonality
fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend()))
report(fit_beer)

# Make an 8-step forecast and plot it
forecast(fit_beer, h = 8) |>
  autoplot(recent_production) +
  autolayer(augment(fit_beer), .fitted, colour = "blue", linetype="dashed") +
  labs(y = "Megalitres",
       title = "Forecasts of beer production using regression")




#### Create a simulated time series with an outlier in 2016 

# Create a simulated yearly time series 
set.seed(12)
date <- seq(as.Date("1998-01-01"), by = "year", length.out = 20)
input <- rnorm(20, sd = 0.1)
output <- input + 0.7*rnorm(20, sd = 0.1)
ts_data <- data.frame(date, input, output)
ts_data <- as_tsibble(ts_data, index = date) 

# Copy the tsibble, create an outlier, and add it to the new tsibble 
ts_data_outlier <- ts_data
ts_data_outlier$output[ts_data_outlier$date == as_date("2016-01-01")] <- 1 

# scatter plot: data with outlier
ggplot(data = ts_data_outlier) +
  geom_point(mapping = aes(x=input, 
                           y=output)) +
  labs(title = "Scatter plot: data with outlier")

# time plot
ts_data_outlier %>% 
  ggplot(mapping = aes(x=date)) +
  geom_line(mapping = aes(y=input, colour="Input")) + 
  geom_line(mappin = aes(y=output, colour="Output")) +
  scale_colour_manual(values=c(Input="grey",Output="black")) +
  guides(colour = guide_legend(title = NULL)) +
  labs(title = "Time plot: data with outlier")


#### a) Simple linear regression with outlier removed

# Create a data set with the outlier removed
row_to_remove <- which(ts_data_outlier$date == as_date("2016-01-01"))
ts_data_no_outlier <- slice(ts_data_outlier, -row_to_remove) 

# Fit a simple model to the data 
simple_fit_no_outlier <- ts_data_no_outlier %>% model(TSLM(output ~ input)) 
report(simple_fit_no_outlier)
augment(simple_fit_no_outlier)

# scatter plot "input vs output with regression line"
ts_data_no_outlier %>%  
  ggplot(mapping = aes(x=input, y=output)) +
  geom_point() +
  geom_smooth(method = "lm", se =F) + 
  labs(title = "Input vs output: Simple linear regression fitted to data with outlier removed")

# time plot "actual vs fitted"
augment(simple_fit_no_outlier) |>
  ggplot(mapping = aes(x = date)) +
  geom_line(aes(y = output, colour = "Output")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Time plot 'Actual vs. Fitted': Simple linear regression with outlier removed",
  ) +
  scale_colour_manual(values=c(Output="black",Fitted="blue")) +
  guides(colour = guide_legend(title = NULL)) +
  coord_cartesian(ylim = c(-0.3, 1.1)) 

# scatter plot "actual vs. fitted"
augment(simple_fit_no_outlier) %>% 
  ggplot(mapping = aes(x=output, y=.fitted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype=2) + 
  labs(title = "Scatter plot 'Actual vs. Fitted: Simple linear regression with outlier removed")


#### b) Simple linear regression with outlier included 

# Fit a simple linear regression model to the data including the outlier
simple_fit_outlier <- ts_data_outlier %>% model(TSLM(output ~ input)) 
report(simple_fit_outlier)
augment(simple_fit_outlier)

# Scatter plot "input vs output with regression line"
#   Note: Inteh code below, we use geom_smooth(method = "lm", se =F).
#         The function lm() produces the same model as TSLM().
#         The only difference is that TSLM() can use the time index to provide additional functionalities.
#         Examples are the special functions of TSLM(), such as trend(), season(), etc.
#         You can use lm() also standalone outside of ggplot. E.g., use the following code:
#         lm(output ~ input, data = ts_data_outlier) %>% summary()
#         Compare the output to report(simple_fit_outlier), and you will see that the results are the same.
ts_data_outlier %>%  
  ggplot(mapping = aes(x=input, y=output)) +
  geom_point() +
  geom_smooth(method = "lm", se =F) + 
  labs(title = "Input vs output: Simple linear regression fitted to all data including outlier")

# Scatter plot "actual vs. fitted"
augment(simple_fit_outlier) %>% 
  ggplot(mapping = aes(x=output, y=.fitted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype=2) + 
  labs(title = "Scatter plot 'Actual vs. Fitted': Simple linear regression fitted to all data including outlier")

# Time plot "actual vs fitted)
# Note: the input time series is not shown in this plot.
augment(simple_fit_outlier) %>% 
  ggplot(mapping = aes(x = date)) +
  geom_line(aes(y = output, colour = "Output")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Time plot 'Actual vs. Fitted': Simple linear regression including outlier",
  ) +
  scale_colour_manual(values=c(Output="black",Fitted="blue")) +
  guides(colour = guide_legend(title = NULL))



#### c) Multiple linear regression with dummy variable for the outlier 

# Create a binary vector (a dummy variable) that indicates the outlier
ts_data_outlier$dummy <- ifelse(ts_data_outlier$date == as_date("2016-01-01"), 1, 0)
print(ts_data_outlier, n=100)

# Fit a model to all data points treating the outlier as dummy
multiple_fit_dummy <- ts_data_outlier %>% model(TSLM(output ~ input + dummy)) 
report(multiple_fit_dummy)
augment(multiple_fit_dummy)

# time plot "actual vs fitted"
augment(multiple_fit_dummy) |>
  ggplot(mapping = aes(x = date)) +
  geom_line(aes(y = output, colour = "Output")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Time plot 'Actual vs. Fitted': Multiple linear regression with outlier as dummy",
  ) +
  scale_colour_manual(values=c(Output="black",Fitted="blue")) +
  guides(colour = guide_legend(title = NULL))

# scatter plot "actual vs. fitted"
augment(multiple_fit_dummy) %>% 
  ggplot(mapping = aes(x=output, y=.fitted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype=2) + 
  labs(title = "Scatter plot 'Actual vs. Fitted': Multiple linear regression with outlier as dummy")



#### Seasonal dummies for the Australian quarterly beer production

# Filter the data to include only the years 1992 and later
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

# Plot the data
recent_production |>
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

# Fit a time series linear regression model to trend and seasonality
fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))

report(fit_beer)



# --------- Variable Selection  ---------

# Fit a multiple regression model to the US consumption data set
fit_consMR <- us_change |>
  model(tslm = TSLM(Consumption ~ Income + Production +
                      Unemployment + Savings))
report(fit_consMR)

# Use the glance() function to show the evaluation metrics
glance(fit_consMR) |>
  select(adj_r_squared, CV, AIC, AICc, BIC)


# --------- Forecasting with Time Series Regression  ---------


#### Ex-ante vs ex-post forecasts: Australian quarterly beer production

# Filter the data to include only the years 1992 and later
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

# Note: For the special predictors trend() and season(), there is no difference between ex-ante and ex-post forecasts!
fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))

# As usual, use the forecast() function to create the forecasts
fc_beer <- forecast(fit_beer)

# Plot the forecasts
# Notice that the 80% and 90% prediction intervals are automatically included in the plot (as always).
fc_beer |>
  autoplot(recent_production) +
  labs(
    title = "Forecasts of beer production using regression",
    y = "megalitres"
  )


#### Scenario Based Forecasting

# Fit a multiple regression model to the US consumption data set
fit_consBest <- us_change |>
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )

# Create scenarios for the future
# Note: The scenarios() function is used to create the scenarios.It creates one tsibble for each scenario.
#       The new_data() function is used to create the scenarios.
#       It is a convenient way to create new data frames with the same structure as the original data frame.
#
future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario")

# Look at the scenarios
future_scenarios

# Create the forecasts
# Use the argument 'new_data' to create the forecasts for the scenarios
fc <- forecast(fit_consBest, new_data = future_scenarios)

# Plot the scenario based forecasts
# Notice that autoplot is ahndling the different scenarios automatically.
us_change |>
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")
