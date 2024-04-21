# ====================================
# Time Series Analysis with R
# gwendolin.wilke@fhnw.ch
# This script contains code used in 
# Part 2, Lecture 2.2 Time Series Features
# ==================================== 

library(fpp3)

# -------- Some Simple Statistics

# Consider the quarterly data set tourism from the tsibble package 
tourism

# Calculate the mean number of trips per time series.
#   - We use the features() function for that.
#   - The result is a tibble with one row per time series.
#     The time series are identified by their keys (Region, State, Purpose).
#   - Arranging them by mean value in increasing order, 
#     we can see that the time series with least average number of trips was 
#     visits to Kangaroo Island in South Australia with purpose “Other”.
tourism |>
  features(Trips, list(mean = mean)) |> # calculate the mean of each time series
  arrange(mean)

# Calculate 5 summary statistics at once using quantile().
tourism |> features(Trips, quantile)


# -------- ACF features

tourism |> features(Trips, feat_acf)


# -------- STL features

### Example: Employment in US retail sector: the time series has a strong trend

# Filter the us_employment data for the retail sector
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID) # there is only one series left, so we can remove the Series_ID column

# Calculate the STL decomposition
dcmp <- us_retail_employment |>
  model(stl = STL(Employed)) 

# Plot the decomposition panels
components(dcmp) |> 
  autoplot() +
  labs(title = "STL decomposition of total US retail employment (default parameters)")

# Now we want to plot the original series and the trend-cycle component on top of each other.  
#   NOTE: In order to pick out only one component with geom_line(), we must first transform the dable into a tsibble. 
#         The index is already given in the dable, we don't need to define it in as_tsibble().
components(dcmp) |>
  as_tsibble() |> 
  autoplot(Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail (original and trend-cycle)"
  )

# components(dcmp) also contains the seasonal adjusted series as a column.
# It contains trend-cycle + remainder (i.e., the original series with the seasonal component removed).
components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail (original and seasonally adjusted)")

# Calculate the STL features
#   We see that the strength of trend (0.999) and strength of seasonality (0.984) are both very high.
us_retail_employment |>
  features(Employed, feat_stl)


### Example: tourism data set

# Find the time series with the strongest trend 
tourism |>
  features(Trips, feat_stl) %>% 
  arrange(-trend_strength) 

# Find the time series with the strongest seasonality
# It's the Snowy Mountains in New South Wales - the most popular ski region of Australia
tourism |>
  features(Trips, feat_stl) %>% 
  arrange(-seasonal_strength_year) 


# Plot the strength of trend against the strength of seasonality for each time series
tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))


# Plot the time series with the strongest seasonality
tourism |>
  features(Trips, feat_stl) |> # calculate the STL features
  filter(seasonal_strength_year == max(seasonal_strength_year)) |> # find the time series with the strongest seasonality
  left_join(tourism, by = c("State", "Region", "Purpose")) |> # join with the original data set to get the time series with the strongest seasonality
  ggplot(aes(x = Quarter, y = Trips)) + # plot the time series with the strongest seasonality
  geom_line() +
  facet_grid(vars(State, Region, Purpose)) # There is only 1 facet... but adding this line adds the description on the right! (Try without it!)


# -------- Exploring Australian tourism data

# Install the package needed to calculate all the features in the package feasts
# install.packages("urca")
# install.packages("fracdiff")
library(urca)
library(fracdiff)

# Calculate all STL features at once
tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))

tourism_features

### Create plot matrix with GGally

# The package glue offers interpreted string literals that are small, fast, and dependency-free. 
# Glue does this by embedding R expressions in curly braces which are then evaluated and inserted into the argument string.
# We use it below to rename the seasonal peaks from numbers to Quarters (such as Q1, Q2, etc.)
library(glue) 

tourism_features |>
  select(contains("season"), all_of("Purpose")) |> # select all features that involve seasonality, along with the Purpose variable.
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0), # convert 0 to 4: If seasonal_peak_year equal 0, then (seasonal_peak_year==0) is TRUE, which is converted to 1. Then, 4*1 = 4 is added. Otherwise, 0 is added.
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0), # same
    seasonal_peak_year = glue("Q{seasonal_peak_year}"), # convert the number to a string starting with "Q" for "Quarter"
    seasonal_trough_year = glue("Q{seasonal_trough_year}"), # same
  ) |>
  GGally::ggpairs(mapping = aes(colour = Purpose))

### Dimensionality reduction using PCA (Principle Component Analysis)

# The broom package takes the messy output of built-in functions in R, 
# such as lm, nls, or t.test, and turns them into tidy tibbles.
library(broom)

# The prcomp() function performs PCA on the given data set.
pcs <- tourism_features |>
  select(-State, -Region, -Purpose) |> # remove the categorical variables
  prcomp(scale = TRUE) |> # scale = TRUE indicates taht the variables should be scaled to have unit variance before the analysis takes place - is advisable for PCA
  augment(tourism_features) # augment() adds the principal components to the original data set

# Plot the first two principal components
pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

# Identify the 4 outliers
outliers <- pcs |>
  filter(.fittedPC1 > 10) |>
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)
outliers

# Plot the outlying time series in PC space
outliers |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |> # join with the original data set to get the time series with the strongest seasonality
  mutate(Series = glue("{State}", "{Region}", "{Purpose}", .sep = "\n\n")) |> # create a new variable Series that combines State, Region, and Purpose
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")
