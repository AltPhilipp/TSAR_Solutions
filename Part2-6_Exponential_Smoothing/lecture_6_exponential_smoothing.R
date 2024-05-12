# ====================================
# Time Series Analysis with R
# gwendolin.wilke@fhnw.ch
# This script contains code used in 
# Part 2, Lecture 6: Exponential Smoothing
# ==================================== 

library(fpp3)

# --------- Simple Exponential Smoothing (SES)  ---------

# Example: Yearly Algerian Exports in % GDP
algeria_economy <- global_economy |>
  filter(Country == "Algeria")

algeria_economy |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")

# Estimate parameters for SES
fit <- algeria_economy |>
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))

augment(fit) # Returns the fitted values
tidy(fit) # Returns the model parameters
components(fit) # Returns the components of the model

# Make a 5-year forecast
fc <- fit |>
  forecast(h = 5)

# PLot the training data, the fitted values and the forecast
fc |>
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted), col="red",
            data = augment(fit)) +
  labs(y="% of GDP", title="Exports: Algeria") +
  guides(colour = "none")

# Plot the components
components(fit) %>% autoplot()

# --------- Holt's Linear Trend Method  ---------

# Filter the data for Australian population from the global_economy dataset
aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Pop = Population / 1e6)

# plot the data
autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population")

# Fit the Holt's Linear Trend Method to estimate its' parameters 
fit <- aus_economy |>
  model(
    AAN = ETS(Pop ~ error("A") + trend("A") + season("N"))
  )

augment(fit) # Returns the fitted values
tidy(fit) # Returns the model parameters
components(fit) # Returns the components of the model

# Make a 10-year forecast
fc <- fit |> forecast(h = 10)

# PLot the training data, the fitted values and the forecast
fc |>
  autoplot(aus_economy) +
  geom_line(aes(y = .fitted), col="red",
            data = augment(fit)) +
  labs(y="Millions", title="Australian population") +
  guides(colour = "none")

# Plot the components
components(fit) %>% autoplot()


# --------- Holt's Damped Trend Method  ---------

### Example: Australian Population (continued)

# Fit both, Holt's Linear Trend Megthod and the Damped Holt's Linear Trend Method 
# Then forecast the result and plot it.
# Note that we only plot the point forecast!
  aus_economy |>
  model(
    `Holt's method` = ETS(Pop ~ error("A") +
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") +
                                   trend("Ad", phi = 0.9) + season("N"))
  ) |>
  forecast(h = 15) |>
  autoplot(aus_economy, level = NULL) +
  labs(title = "Australian population",
       y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))

### Example: Internet Usage

www_usage <- as_tsibble(WWWusage)
www_usage |> autoplot(value) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

# We compare the one-step forecast accuracy of the three methods using time-series cross validation:
www_usage |>
  stretch_tsibble(.init = 10) |> # Creates the folds with an initial window size of 10 .init 
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  ) |>
  forecast(h = 1) |>
  accuracy(www_usage)

# Since Holt’s damped method is the winner, we proceed with it: 
# We train it now to the whole data set, and then make a forecast.
fit <- www_usage |>
  model(
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  )

# Estimated parameters:
tidy(fit)

# Make a 10-step forecast and plot it
fit |>
forecast(h = 10) |>
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")


# --------- The Holt-Winter’s Seasonal Method  ---------

### Domestic overnight trips in Australia

# Extract the relevant data from the tourism dataset
aus_holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips)/1e3)

# fit both versions of the Holt-Winter's seasonal method
fit <- aus_holidays |>
  model(
    additive = ETS(Trips ~ error("A") + trend("A") +
                     season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") +
                           season("M"))
  )

augment(fit) # Returns the fitted values
tidy(fit) # Returns the model parameters
components(fit) # Returns the components of the model

# Make a 3-year forecast 
fc <- fit |> forecast(h = "3 years")

# Plot it
fc |>
  autoplot(aus_holidays, level = NULL) +
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

augment(fit)
tidy(fit) # Estimated parameters
glance(fit) # Model fit statistics
components(fit) # Components of the model

# --------- The Holt-Winter’s Damped Seasonal Method  ---------

# Extract the relevant data from the pedestrian dataset
sth_cross_ped <- pedestrian |>
  filter(Date >= "2016-07-01",
         Sensor == "Southern Cross Station") |>
  index_by(Date) |>
  summarise(Count = sum(Count)/1000)

# Fit the Holt-Winter's Damped Seasonal Method
sth_cross_ped |>
  filter(Date <= "2016-07-31") |>
  model(
    hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))
  ) |>
  forecast(h = "2 weeks") |>
  autoplot(sth_cross_ped |> filter(Date <= "2016-08-14")) +
  labs(title = "Daily traffic: Southern Cross",
       y="Pedestrians ('000)")
