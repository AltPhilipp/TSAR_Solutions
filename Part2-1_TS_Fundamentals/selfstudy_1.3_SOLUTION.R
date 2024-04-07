##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Self-Study 1.3: Tome Plots and White Noise
# gwendolin.wilke@fhnw.ch
##############################################


#### 1. tute1.csv

tute1 <- readr::read_csv(tute1)

View(tute1)

mytimeseries <- tute1 %>% 
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(index = Quarter)

mytimeseries %>% 
  pivot_longer(-Quarter, names_to="Key", values_to="Value") %>% 
  ggplot(aes(x = Quarter, y = Value, colour = Key)) +
  geom_line() +
  facet_grid(vars(Key), scales = "free_y")

# Without faceting:
mytimeseries %>% 
  pivot_longer(-Quarter, names_to="Key", values_to="Value") %>% 
  ggplot(aes(x = Quarter, y = Value, colour = Key)) +
  geom_line()

#### 2. Life Stock


vic_pigs <- aus_livestock %>% 
  filter(Animal == "Pigs", 
         State == "Victoria", 
         between(year(Month), 1990, 1995))
vic_pigs

vic_pigs %>% 
  autoplot(Count)
# Although the values appear to vary erratically between months, 
# a general upward trend is evident between 1990 and 1995. 
# In contrast, a white noise plot does not exhibit any trend.


vic_pigs %>%  
  ACF(Count) %>% 
  autoplot()
# The first 14 lags are significant, as the ACF slowly decays. 
# This suggests that the data contains a trend. 
# A white noise ACF plot would not usually contain any significant lags. 
# The large spike at lag 12 suggests there is some seasonality in the data.


aus_livestock |>
  filter(Animal == "Pigs", State == "Victoria") |>
  ACF(Count) |>
  autoplot()
# The longer series has much larger autocorrelations, plus clear evidence of 
# seasonality at the seasonal lags of 12, 24, ....


