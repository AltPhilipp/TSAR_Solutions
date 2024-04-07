# ====================================
# Time Series Analysis with R
# gwendolin.wilke@fhnw.ch
# This script contains code used in 
# Part 2, Lecture 1.3 Visualizing Time Series
# ==================================== 

#install.packages("fpp3")
library(fpp3)


# ------ Plot Type: Time Plots

### Example: ansett data on airline passengers
#   We see the tsibble holds 30 differemt time series and 
#   that the observations are recorded weekly 
ansett

# We want to see passenger numbers of all flights from Melbourne to Sidney in economy class.
# The passenger numbers should be displayed in thousands.
melsyd_economy <- ansett %>% 
  filter(Airports == "MEL-SYD", Class == "Economy") %>% 
  mutate(Passengers = Passengers/1000)

# autoplot() recognises melsyd_economy as a time series and automatically produces a time plot
# We only need to specify the data set and the variable to be plotted
autoplot(melsyd_economy, Passengers) + 
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

### Example: Medicare pharmaceutical products 
#   We reuse the data set a10.RDS that we saved earlier. 
#   It contains the summary times series of total cost of prescriptions with ATC2 index A10.

# Load the data set
a10 <- readRDS("a10.RDS")

# autoplot() recognises a10 as a time series and automatically produces a time plot
autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

# ------ Plot Type: Seasonal Plots

#### Example: a10 data of prescription costs

# Create a seasonal plot using gg_season()
#     The argument "labels" specifies the position of the labels for seasonal period identifier.
#     "both" means that the labels are displayed on both sides of the plot.
a10 %>% 
  gg_season(Cost, labels = "both") + 
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")

#### Example: vic_elec data of electricity demand in Victoria

# The time interval of observations is half-hourly:
vic_elec

# The time plot shows that there is more than one seasonal pattern:
vic_elec %>%  
  autoplot(Demand) + 
  labs(y = "MWh", title = "Electricity demand: Victoria") -> timeplot_vic_elec

library(plotly)
ggplotly(timeplot_vic_elec)

# We use the "period" argument in gg_season to specify which seasonal period to plot:

# daily
vic_elec %>%  
  gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Daily Electricity demand in Victoria 2012 - 2015") 

# weekly
vic_elec %>%  
  gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Weekly Electricity demand in Victoria 2012 - 2015")

# monthly
vic_elec %>%  
  gg_season(Demand, period = "month") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Monthly Electricity demand in Victoria 2012 - 2015")

# yearly
vic_elec %>%  
  gg_season(Demand, period = "year") +
  labs(y="MWh", title="Yearly Electricity demand in Victoria 2012 - 2015")

# ------ Plot Type: Seasonal Subseries Plots

#### Example: a10 data of prescription costs

# Plot again the timeplot of a10 
autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

# Plot again the seasonal plot of a10 
a10 %>% 
  gg_season(Cost, labels = "both") + 
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")

# Now plot the seasonal subplots of a10
a10 %>% 
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

# ------ A Use Case: Australian Tourism

# We see that this is a tsibble containing quarterly data
tourism

tourism %>%  distinct(Region)
tourism %>%  distinct(State)
tourism %>%  distinct(Purpose)

# We want to analyse how many tourists come for holidays to Australia. 
#   Regarding the geographic level of detail: We want to analyse it by state (thus disregarding /grouping the single regions of each state).
#   Regarding the temporal level of detail: We want to analyse it per quarter. 

# To do thus, we first filter for Holiday and then group by State.
#   Note: We do not have to explicitly group by Quarters as this is the natural grouping in a tsibble.
tourism %>% 
  filter(Purpose == "Holiday") %>% 
  group_by(State) 

# Next, we use summarize() to sum up the number of trips over the groups (State and Quarter).
holidays <- tourism %>% 
  filter(Purpose == "Holiday") %>% 
  group_by(State)  %>% 
  summarise(Trips = sum(Trips)) 

# Using autoplot()we can create a time plot of the resulting 8 time series:
autoplot(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Total visitor nights spent on holiday by State")

# Using gg_season() we can create a seasonal plot of the 8 time series.
# The seasonality of 1 year is automatically detected.
gg_season(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

# Using gg_subseries() we can create seasonal subseries plots of the 8 time series.
holidays %>% 
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

# ------ Plot Type: Scatter Plots and Scatterplot Matrices

#### Example: Electricity demand and temperature in Victoria

# Half-hourly electricity demand (in Gigawatts) for Victoria
vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Demand) +
  labs(y = "GW",
       title = "Half-hourly electricity demand: Victoria")

# Half-hourly temperatures (in degrees Celsius) for Melbourne
vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

# Creating a scatterplot, temperature against demand.
vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")

#### Example: Tourism in Australia

# Creating the aggregated time series of overnight trips per State
#   (Notice that we use all trips now (holday AND business))
visitors <- tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

# autoplot() creates a figure that contains 8 time plots - one per time series 
autoplot(visitors, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Total visitor nights spent by State")

# Creating a seperate timeplot for each of the 8 time series using ggplot()
#    vars(State) specifies that we want to create a facet for each State.
#    scales = "free_y" ensures that the y-axis scales are independent for each facet.
visitors |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") + 
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")

# Creating a scatterplot matrix of the 8 states against each other (disregarding time)
visitors |>
  pivot_wider(values_from=Trips, names_from=State) |>
  GGally::ggpairs(columns = 2:9)


# ------ Plot Type: Lag Plots

#### Example: Australian beer production
#   We consider the panel data set aus_production from package tsibbledata.
#   It contains quarterly production of selected commodities in Australia. 
aus_production

# The time series starts in 1956. We consider it only from the year 2000 on:
recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)

# Let's look at the beer production from year 2000 on:
aus_production %>% 
  filter(year(Quarter) >= 2000) %>% 
  select(Beer) %>% 
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian beer production")


# Create a lag plot of the Beer production:
recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)") -> lagplot_beer

# To see the single values, plotly is helpful:
library(plotly)
ggplotly(lagplot_beer)

# ------ Autocorrelation

#### The ACF function

recent_production %>%  
  ACF(Beer, lag_max = 9)

#### Plot Type: Correlogram

recent_production |>
  ACF(Beer) |>
  autoplot() + labs(title="Australian beer production")

#### Trend and Seasonality in ACF PLots

# a10 data has trend and seasonality
autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

# In the correlogram, this shows as decreasing autocorrelation with increasing lags (trend)
# and a "scalloped" shape (seasonality)
a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot() +
  labs(title="Australian antidiabetic drug sales")

#### White Noise

# Example: A randomly generated time series
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)

# Plot the white noise time series
y %>%  
  autoplot(wn) + 
  labs(title = "White noise", y = "")

# Plot the ACF
y %>% 
  ACF(wn) %>% 
  autoplot() + labs(title = "White noise")


