##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Exercises 4+5: TS Regression Models
# gwendolin.wilke@fhnw.ch
##############################################
# This script contains only accompanying code for the exercises 4 and 5.
# For the written solutions, please download the word file from Moodle.
##############################################

library(fpp3)

# ----------- Exercise 4, Task 1  ----------- 

# Filter the data for January 2014
jan_vic_elec <- vic_elec |>
  filter(yearmonth(Time) == yearmonth("2014 Jan")) |>
  index_by(Date = as_date(Time)) |>
  summarise(Demand = sum(Demand), Temperature = max(Temperature))

# Time series plot of the two time series
jan_vic_elec |>
  pivot_longer(2:3, names_to="key", values_to="value")|>
  autoplot(.vars = value) +
  facet_grid(vars(key), scales = "free_y")

# Scatter plot of the two time series with a regression line
jan_vic_elec |>
  ggplot(aes(x = Temperature, y = Demand)) +
  labs(y = "Demand (MWh)",
       x = "Temperature (C)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Electricity Demand in Victoria (01.-31. Jan 2014)",
       subtitle = "Simple Linear Model"
  )

# Fit a simple linear time series regression model
fit <- jan_vic_elec |>
  model(TSLM(Demand ~ Temperature))

fit |> report()

# ----------- Exercise 4, Task 2  ----------- 

# Plot the residuals
fit |> gg_tsresiduals()

# Time plots of "actual and fitted" values
augment(fit) |>
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Demand, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(title = "Electricity Demand in Victoria (01.-31. Jan 2014)",
       subtitle = "Actual vs. Fitted") +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

# Scatter plot "actual vs. fitted"
augment(fit) %>% 
  ggplot(mapping = aes(x=Demand, y=.fitted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype=2, color="blue") + 
  labs(title = "Electricity Demand in Victoria (01.-31. Jan 2014)",
       subtitle = "Actual vs. Fitted")


# Check the residual mean:
augment(fit)$.innov %>%  mean(na.rm = T)

# Check the Ljung-Box test:
augment(fit) %>%  features(.innov, ljung_box, lag = 10)


# ----------- Exercise 5, Task 1  ----------- 

# Create scenarios for the next day
next_day <- scenarios(
  `Cold day` = new_data(jan_vic_elec, 1) |> mutate(Temperature = 15),
  `Hot day` =  new_data(jan_vic_elec, 1) |> mutate(Temperature = 35)
)

# Make the scenario forecasts
fc_next_day <- fit |>
  forecast(new_data = next_day)

# Plot the scenario forecasts
autoplot(jan_vic_elec, Demand) +
  autolayer(fc_next_day)


# ----------- Exercise 5, Task 2  ----------- 

# Filter the data for January 2014
jan_vic_elec <- vic_elec |>
  filter(yearmonth(Time) == yearmonth("2014 Jan")) |>
  index_by(Date = as_date(Time)) |>
  summarise(Demand = sum(Demand), Temperature = max(Temperature))

# Define the training set: first 28 days of Jan, leaving out the last 3 days.
train <- jan_vic_elec %>% slice(1:28) 
test <- jan_vic_elec %>% slice(29:31)

#### Model 1: only temperature as predictor

# fit the model to the training set
model1 <- train |>
  model(TSLM(Demand ~ Temperature))

model1 |> report()

# sample mean of the training data 
train$Demand %>% mean(na.rm = T)

# standard deviation of the training data
train$Demand %>% sd(na.rm = T)


# Time plots "actual vs. fitted"
augment(model1) |>
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Demand, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Electricity Demand in Victoria (01.-28. Jan 2014)",
       subtitle = "Simple Model, Actual vs. Fitted on Training Set"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))


# Scatter plot "actual vs. fitted"
augment(model1) %>% 
  ggplot(mapping = aes(x=Demand, y=.fitted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype=2, color="blue") + 
  labs(title = "Electricity Demand in Victoria (01.-28. Jan 2014)",
       subtitle = "Simple Model, Actual vs. Fitted on Training Data")

# Plot the residuals
model1 |> gg_tsresiduals()

# Check the residual mean:  
augment(model1)$.innov %>%  mean(na.rm = T)

# Check the Ljung-Box test:
augment(model1) %>%  features(.innov, ljung_box, lag = 10)

# Make forecasts
fc_model1 <- model1  %>% 
  forecast(new_data = test)

# Plot forecasts
autoplot(train, Demand) +
  autolayer(fc_model1) + 
  autolayer(test, Demand) +
  labs(title = "Model 1",
       subtitle = "Electricity Demand Forecast for 28.- 31. Jan 2014")

# Check model accuracy
accuracy(fc_model1, jan_vic_elec)
# Note: You need to provide the whole data set to the accuracy function, otherwise MASE and RMSSE cannot be calculated.
#       Remember that these measures compare the forecast to a naive / seasonal naive forecast on the training set.
#       To see that, try the following:
# accuracy(fc, test)


#### Model 2: trend as an additional predictor

model2 <- train |>
  model(TSLM(Demand ~ Temperature + trend()))

model2 |> report()

# Time plots of observed and fitted values
augment(model2) |>
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Demand, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Electricity Demand in Victoria (01.-28. Jan 2014)",
       subtitle = "Multiple Model with Trend, Actual vs. Fitted on Training Set"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

# Scatter plot "actual vs. fitted"
augment(model2) %>% 
  ggplot(mapping = aes(x=Demand, y=.fitted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype=2, color="blue") + 
  labs(title = "Electricity Demand in Victoria (01.-28. Jan 2014)",
       subtitle = "Multiple Model with Trend, Actual vs. Fitted on Training Data")

# Plot the residual diagnostics
model2 |> gg_tsresiduals()

# Check the residual mean:
augment(model2)$.innov %>%  mean(na.rm = T)

# Checkthe Ljung-Box test:
augment(model2) %>%  features(.innov, ljung_box, lag = 10)

# Make forecasts
fc_model2 <- model2  %>% 
  forecast(new_data = test)

# Plot forecasts
autoplot(train, Demand) +
  autolayer(fc_model2) +
  autolayer(test, Demand) +
  labs(title = "Model 2",
       subtitle = "Electricity Demand Forecast for 28.- 31. Jan 2014")

# Check model accuracy
accuracy(fc_model2, jan_vic_elec)
accuracy(fc_model2, test)



# ----------- Exercise 5, Task 3  ----------- 

olympic_running |>
  ggplot(aes(x = Year, y = Time, colour = Sex)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~Length, scales = "free_y", nrow = 2) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(y = "Running time (seconds)")

fit <- olympic_running |>
  model(TSLM(Time ~ trend()))
tidy(fit) %>% print(n=100)

augment(fit) |>
  ggplot(aes(x = Year, y = .innov, colour = Sex)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~Length, scales = "free_y", nrow = 2) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", legend.title = element_blank())

fit |>
  forecast(h = 1) |>
  mutate(PI = hilo(Time, 95)) |>
  select(-.model)


# ----------- Exercise 5, Task 4  ----------- 

vic_elec_holiday <- vic_elec |>
  index_by(Date = as_date(Time)) |>
  summarise(Demand = sum(Demand), Temperature = max(Temperature), Holiday = max(Holiday))

print(vic_elec_holiday, n=1000)

# Time series plot of the two time series
vic_elec_holiday |>
  pivot_longer(2:3, names_to="key", values_to="value")|>
  autoplot(.vars = value) +
  facet_grid(vars(key), scales = "free_y")

# Scatter plot of the two time series with a regression line
vic_elec_holiday |>
  ggplot(aes(x = Temperature, y = Demand)) +
  labs(y = "Demand (MWh)",
       x = "Temperature (C)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Electricity Demand in Victoria (01.-31. Jan 2014)",
       subtitle = "Simple Linear Model"
  )

# Fit a simple linear time series regression model
fit <- vic_elec_holiday |>
  model(TSLM(Demand ~ Temperature))

fit |> report()

# Fit a simple linear time series regression model with holiday dummy
fit_holiday <- vic_elec_holiday |>
  model(TSLM(Demand ~ Temperature + Holiday))

fit_holiday |> report()

# Fit a simple linear time series regression model with holiday and season dummy
fit_holiday_season <- vic_elec_holiday |>
  model(TSLM(Demand ~ Temperature + Holiday + season(period = 12)))

fit_holiday_season |> report()

