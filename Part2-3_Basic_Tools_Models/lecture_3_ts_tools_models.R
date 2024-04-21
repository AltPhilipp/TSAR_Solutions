# ====================================
# Time Series Analysis with R
# gwendolin.wilke@fhnw.ch
# This script contains code used in 
# Part 2, Lecture 3 Time Series Tools and Simple Models
# ==================================== 

library(fpp3)

# --------- A tidy forecasting workflow ---------

### Data preparation (tidy)
gdppc <- global_economy |>
  mutate(GDP_per_capita = GDP / Population)

### Plot the data (visualise)
gdppc |>
  filter(Country == "Sweden") |>
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

### Define a model (specify)
TSLM(GDP_per_capita ~ trend()).

### Train the model (estimate)
fit <- gdppc |>
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit

### Check model performance (evaluate)


### Produce forecasts (forecast)
fit |> forecast(h = "3 years")

### Plot the forecast (communicate)
fit |>
  forecast(h = "3 years") |>
  filter(Country == "Sweden") |>
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")

# --------- 4 Simple Benchmark Methods for Forecasting ---------

bricks <- aus_production |>
  filter_index("1970 Q1" ~ "2004 Q4") |>
  select(Bricks)


### Mean method
#   model() from package fabletools trains one or more specified models on a dataset.
#   The output is a mable (model table) that contains the fitted models.
#   forecast() generates h-step forecasts from the model(s).
bricks |> model(MEAN(Bricks)) |> forecast(h = 4)

### Naïve method
bricks |> model(NAIVE(Bricks)) |> forecast(h = 4)

### Seasonal naïve method
#   The seasonal naïve method is a special case of the lag method.
#   lag() computes a lagged version of a time series
#   lag("year") shifts the time series back by one year
bricks |> model(SNAIVE(Bricks ~ lag("year"))) |> forecast(h = 8)

### Drift method
#   The drift method is a special case of the random walk method. See ?RW for more details.
bricks |> model(RW(Bricks ~ drift())) |> forecast(h = 20) 

### Example: Australian quarterly beer production

# Specify training data from 1992 to 2006
#   Notice that this data still contains all columns (Beer, Tobacco, Bricks, etc.)
train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4") # 14 full years

# Fit the models to the column Beer
beer_fit <- train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )
# Note: 
#   The resulting model stored in "beer_fit" only contains data on Beer (no Tabacco, Bricks, etc.). 
#   You can check that using the function augment(), which shows model parameters and statistics:
augment(beer_fit) 
#   We see that there is onyl a Beer column. (The other columns in the output are discussed later.)

# Plot the fitted values of the Mean Method against the ground truth
#     Note: autolayer() is used to add another autoplot() layer to the plot from a different tsibble. 
#           Here, we add the original data to the plot. Notice that both have the same index structure.
augment(beer_fit) %>% 
  filter(.model == "Mean") %>% 
  autoplot(.fitted, colour = "blue", linetype = "dashed") +
  autolayer(train, Beer) +
  labs(y = "Megalitres",
       title = "Fitted and true values for quarterly beer production (Mean Method)")

# Generate forecasts for remaining quarters
#   Note: 
#   - We want to make forecasts for the remaining length of the original time series.
#     This way, we can use the original values as "test set" and compare our forecasts with it. 
#   - You can use tail(aus_production) to see that the last quarter of the original series is "2010 Q2"
#   - Thus, from 2007 Q1 to 2010 Q2, there are 14 quarters we want to forecast.
beer_fc <- beer_fit |> forecast(h = 14)

# Plot the fitted and forecast values of the Mean Method against the ground truth
autoplot(filter_index(aus_production, "1992 Q1" ~ "2006 Q4")) + # plot the ground truth (training data)
  autolayer(filter_index(aus_production, "2007 Q1" ~ .)) +      # add the ground truth (test data)
  autolayer(augment(beer_fit) %>% filter(.model == "Mean"), .fitted, # add the fitted values
            color="blue", linetype="dashed") + 
  autolayer(beer_fc |> filter(.model == "Mean"), level = NULL) + # add the forecasts
  labs(y = "Megalitres",
       title = "Forecasts for quarterly beer production (Mean method)")


# Plotting the forecast of all 3 models at once is super simple:
# We simply feed the model table in autoplot() without using augment().
#   Note: The fitted values of the training data are not shown here.
#   Also, the ground truth on the forecast period is not shown.
beer_fc %>% autoplot(train, level = NULL) +
  labs(y = "Megalitres", title = "Forecasts for quarterly beer production")

# We can add the ground truth for the forecast period easily: 
beer_fc %>% autoplot(train, level = NULL) + 
  autolayer(filter_index(aus_production, "2007 Q1" ~ .), colour = "black") + # Add ground truth
  labs(y = "Megalitres", title = "Forecasts for quarterly beer production") + # Set title
  guides(colour = guide_legend(title = "Forecast Method")) # Set legend title 


### Example: Google’s daily closing stock price

# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE) # Use update_tsibble() to update key and index for a tsibble: We set the index to day. regular = TRUE says that regular time intervals are used. 

# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)

# Fit the models
google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )

# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))

google_fc <- google_fit |>
  forecast(new_data = google_jan_2016)

# Plot the forecasts
google_fc |>
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))


# --------- Fitted values and residuals ---------


# Set training data from 1992 to 2006
train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4")

# Fit the models
beer_fit <- train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )

# Inspect the residuals using the function augment()
augment(beer_fit)

# Plots see above!


# --------- Residual diagnostics ---------

### Example: Naive Forecast of Google daily closing stock prices

# plot the data
autoplot(google_2015, Close) +
  labs(y = "$US",
       title = "Google daily closing stock prices in 2015")

# Fit the naïve model and inspect residuals
aug <- google_2015 |>
  model(NAIVE(Close)) |>
  augment()

# Plot the residuals
autoplot(aug, .innov) +
  labs(y = "$US",
       title = "Residuals from the naïve method")

# plot the histogram of residuals
aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")

# plot the ACF of residuals
aug |>
ACF(.innov) |>
  autoplot() +
  labs(title = "Residuals from the naïve method")

# gg_tsresiduals() is a convenient shortcut for producing the residual diagnostic plots
google_2015 |>
  model(NAIVE(Close)) |>
  gg_tsresiduals()

# The Box-Pierce test for autocorrelation in the residuals
aug |> features(.innov, box_pierce, lag = 10)

# The Ljung-Box test for autocorrelation in the residuals
aug |> features(.innov, ljung_box, lag = 10)



### Example: Drift Forecast of Google daily closing stock prices

# Fit the drift model 
fit <- google_2015 |> model(RW(Close ~ drift()))

# Extract the model parameters and associated statistics
#    tidy() is a generic method that turns an object into a tidy tibble.
#    It is imported to fabletools (see also ?tidy.mdl_df):
#    For a mable (a model table), the function shows the estimated parameters.
tidy(fit)

# Applying the Ljung-Box test
augment(fit) |> features(.innov, ljung_box, lag=10)



# --------- Forecast Distributions and Prediction Intervals ---------

### Example: Prediction intervals for the naïve forecast of the Google closing stock price in 2015 

google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE) 
google_2015 <- google_stock |> filter(year(Date) == 2015)

# The function hilo() augments the output of forecast() with the upper and 
# lower limits of the prediction intervals
#     Default are 80% and 95% prediction intervals.
#     The coverage percentage can be specified with the level argument.
google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  hilo()

# Plot the forecast with prediction intervals
google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  autoplot(google_2015) +
  labs(title="Google daily closing stock price", y="$US" )


# --------- Forecasting with Transformations ---------

# Example: Egg prices
# The prices data set from the fpp3 package contains annual prices for eggs in $US, adjusted for inflation.

# Plot the egg prices: We see that varianc changes with the level of the series
prices |>
  filter(!is.na(eggs)) |>
  autoplot(eggs) +
  labs(title = "Annual egg prices",
       y = "$US (in cents adjusted for inflation) ")

# Plot the log transformed egg prices (stabilizing variance)
prices |>
  filter(!is.na(eggs)) |>
  autoplot(log(eggs)) +
  labs(title = "Log transformed annual egg prices",
       y = "log($US) (in cents adjusted for inflation) ")

### What happens with the Point Forecasts?

# Forecast the log transfromed egg prices with the drift method.
#   Note: The log transformation is directly put into the model formula.
#         Because of this, the forecast is automatically back-transformed to the original scale.
fc <- prices |>
  filter(!is.na(eggs)) |>   # the egg time series has some NAs in the beginning, we remove them
  model(RW(log(eggs) ~ drift())) |>   # put the log transform in the model formula
  forecast(h = 50) 

# fc contains the forecasts in the original (back-transformed) scale
# We plot them using autoplot().
fc |>
  autoplot(prices |> filter(!is.na(eggs)), level = NULL) + # level = NULL to show no prediction interval
  labs(title = "Drift forecast of annual egg prices",
       y = "$US (in cents adjusted for inflation) ")

# Now lets look at the 80% prediction interval
fc %>% hilo()

#   Notice that the back.transformed prediction interval is not symmetric around the point forecast
fc |>
  autoplot(prices |> filter(!is.na(eggs)), level = 80) + # level = 80 to show only the 80% prediction interval
  labs(title = "Drift forecast of annual egg prices",
       y = "$US (in cents adjusted for inflation) ")

# The prices data set from the fpp3 package contains annual prices for eggs in $US, adjusted for inflation.
# Note: The log transfromation is directly put into the model formula.
fc_ba <- prices |>
  filter(!is.na(eggs)) |>
  model(RW(log(eggs) ~ drift())) |>
  forecast(h = 50) |>
  mutate(.median = median(eggs))

fc_ba |>
  autoplot(prices |> filter(!is.na(eggs)), level = 80) +
  geom_line(aes(y = .median), data = fc_ba, linetype = 2, col = "blue") +
  labs(title = "Annual egg prices",
       y = "$US (in cents adjusted for inflation) ")



# --------- Forecasting using Decomposition ---------

### Example: Employment in the US retail sector

# We filter the data to only include the retail trade sector from 1990 onwards
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade")

# Plot it to get a first impression
autoplot(us_retail_employment)

# We decompose the time series into trend-cycle and seasonality using STL 
# Remember that STL only provides additive decomposition - that is reasonable here, since the variance is stable.
dcmp <- us_retail_employment |> 
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) |> # Set the window size for the trend-cycle component to 7 Months.
  components() |> # extract the decomposed components
  select(-.model) # deselect the model name, since we only have one model anyways

# We fit a naive model to the seasonally adjusted component.
# Then we make a forecast (default forecast period is 2 years).
# Then we plot the result. 
dcmp |>
  model(NAIVE(season_adjust)) |>
  forecast() |> # forecast the seasonally adjusted component
  autoplot(dcmp) +
  labs(y = "Number of people",
       title = "US retail employment")

# Now we want to “reseasonalise” our forecast. 
# To do that, we want to calculate the seasonal naïve forecast of the seasonal component 
# and then add it to the forecast of the seasonally adjusted component produced above.

# Instead of doing both forecasts separately, and then putting them back together we can use the decomposition_model() function.
# It allows to decompose the time series and fit forecasting models for each resulting component in one single command:
#   - We specify the decomposition model using the STL() function, 
#   - We specify the forecasting model for the seasonally adjusted component using the NAIVE() function.
#   - We don't need to specify a forecasting model for the seasonal component, because it is automatically set to the SNAIVE() function. (A different model can be specified, if needed  though.)
fit_dcmp <- us_retail_employment |>
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

# It is now extremely easy to make forecasts:
# We just feed in the output of the decomposition_model() function into the forecast() function.
fit_dcmp |>
  forecast() |>
  autoplot(us_retail_employment)+
  labs(y = "Number of people",
       title = "US retail employment")



# --------- Evaluating Point Forecast Accuracy ---------

### Useful dplyr verbs to subset a time series

# Using the dplyr verb filter() for subsetting
aus_production |> filter(year(Quarter) >= 1995) # extracting all data from 1995 onward
aus_production |> filter_index("1995 Q1" ~ .) # same

# Using the dplyr verb slice() to subset by index
aus_production |> 
  slice(n()-19:0) # extract the last 20 observations (5 years)

aus_retail |>
  group_by(State, Industry) |>
  slice(1:12) # extract the first year of data for each State and Industry


### Forecast errors - Examples: Beer Production (seasonal)

# First we apply 4 forecast methods to the quarterly Australian beer production using data only to the end of 2007: 

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

beer_train <- recent_production |>
  filter(year(Quarter) <= 2007)

beer_fit <- beer_train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit |>
  forecast(h = 10)

beer_fc |>
  autoplot(
    aus_production |> filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


# We compute the forecast accuracy measures for this period.
accuracy(beer_fc, recent_production)


### Forecast errors - Examples: Google stocks (non-seasonal)

# First we apply 3 forecast methods to the data:

google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

google_fc <- google_fit |>
  forecast(google_jan_2016)

google_fc |>
  autoplot(bind_rows(google_2015, google_jan_2016),
           level = NULL) +
  labs(y = "$US",
       title = "Google closing stock prices from Jan 2015") +
  guides(colour = guide_legend(title = "Forecast"))

# We compute the forecast accuracy measures for this period.
accuracy(google_fc, google_stock)



# --------- Time series cross-validation ---------

### Example: Comparing TSCV with residual accuracy 

# The stretch_tsibble() function is used to create many training sets. 
#   We start with a training set of length .init=3. 
#   We then increase the size of successive training sets by .step=1.
google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1) |>
  relocate(Date, Symbol, .id)

# The .id column provides a new key indicating the various training sets. 
google_2015_tr

# The accuracy() function can be used to evaluate the forecast accuracy across the training sets:

# TSCV accuracy
google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 1) |>
  accuracy(google_2015)

# Training set accuracy
google_2015 |>
  model(RW(Close ~ drift())) |>
  accuracy()

### Example: Comparing the forecasting performance of 1- to 8-step-ahead drift forecasts 


google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1)

# Forecast the Google stock prices for 8 trading days ahead
fc <- google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 8) |> # set the forecast horizon to 8
  group_by(.id) |> # group the forecasts by the training set id
  mutate(h = row_number()) |> # add a column h that indicates the forecast horizon
  ungroup() |> 
  as_fable(response = "Close", distribution = Close) # a fable is created to store the forecasts

fc |>
  accuracy(google_2015, by = c("h", ".model")) |> # compute the accuracy measures‚
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()
