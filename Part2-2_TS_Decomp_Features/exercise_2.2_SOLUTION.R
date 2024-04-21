##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Exercise 2.2: Analysing a Feature Plot Matrix
# gwendolin.wilke@fhnw.ch
##############################################

library(fpp3)
library(GGally)

#  Create the feature plot matrix
tourism |>
  features(Trips, feat_stl) |>
  select(-Region, -State, -Purpose) |>
  mutate(
    seasonal_peak_year = factor(seasonal_peak_year),
    seasonal_trough_year = factor(seasonal_trough_year),
  ) |>
  ggpairs()

# Displaying the peak quarter for holidays in each state
tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips)) |>
  features(Trips, feat_stl) |>
  select(State, seasonal_peak_year)
