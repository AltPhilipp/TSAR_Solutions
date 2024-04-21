# ====================================
# Time Series Analysis with R
# gwendolin.wilke@fhnw.ch
# This script contains code used in 
# Part 2, Lecture 2.1 Time Series Decomposition
# ==================================== 

library(fpp3)



# -------- Transformations and Adjustments

### Population adjustments
global_economy |>
  filter(Country == "Australia") |>
  autoplot(GDP/Population) +  # notice that we can use the / operator to divide two columns
  labs(title= "GDP per capita", y = "$US")

### Inflation adjustments
print_retail <- aus_retail |>
  filter(Industry == "Newspaper and book retailing") |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover = sum(Turnover))

aus_economy <- global_economy |>
  filter(Code == "AUS")

print_retail |>
  left_join(aus_economy, by = "Year") |>
  mutate(Adjusted_turnover = Turnover / CPI * 100) |>  # adjust for inflation
  pivot_longer(c(Turnover, Adjusted_turnover),
               values_to = "Turnover") |>
  mutate(name = factor(name,
                       levels=c("Turnover","Adjusted_turnover"))) |>  
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry",
       y = "$AU")

### Log transformation

# Load the a10 data on antidiabetic drug sales extracted from the data set tsibbledata::PBS
a10 <- readRDS("a10.RDS")
autoplot(a10, Cost) +
  labs(title = "Australian antidiabetic drug sales", y = "$ (millions)")

# Apply a log transformation to the data
a10 |>
  mutate(log_Cost = log(Cost)) |> # log transformation with the natural logarithm
  autoplot(log_Cost) +
  labs(title = "Log-transformed Australian antidiabetic drug sales", y = "log($ millions)")

### Box-Cox transformation

# Consider the time series of Australian gas production.
aus_production |> 
  autoplot(Gas, color ="grey") +
  labs(y = "",
       title = "Gas production (original series) ")

# We see that the seasonal variation increases a lot with increasing level of the series.
# A Box-Cox transformation can be used to stabilize the seasonal variance, so that it is 
# approximately constant over time.

# First, we find the best lambda for the Box-Cox transform
#    Remark:
#    - To do that, we use the function features() from package fabletools.  
#      It can be used to create scalar valued summary features for a dataset using a "feature function".
#    - As a feature function, we use the "guerrero method". It selects the lambda which minimizes the 
#      coefficient of variation for subseries of x.
lambda <- aus_production |>
  features(Gas, features = guerrero) |> # find the lambda that minimizes the coefficient of variation of the GAS series
  pull(lambda_guerrero) # the dplyr verb pull() extracts the value of the lambda 

# Now we can apply the Box-Cox transform with the best lambda
#   The function box_cox() from the fabletools package is used
box_cox(aus_production$Gas, lambda)

# Plot the transformed data to compare with the original series
#   The package latex2exp is used to display the lambda symbol in the title
# install.packages('latex2exp') 
library(latex2exp)
aus_production |>
  autoplot(box_cox(Gas, lambda)) + #specify that we want to see the transformed Gas column on the y-axes
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Box-Cox transformed gas production with $\\lambda$ = ",   # inserting LaTeX code to display lambda
         round(lambda,2))))

# We can see that the seasonal variation is now approximately constant over time.

# When we put both graphs in one chart, we see how much the original series has been dampened by the transformation
aus_production |>
  autoplot(Gas, color ="grey") +
  geom_line(aes(x = Quarter, y = box_cox(Gas, lambda))) + 
  labs(y = "",
       title = latex2exp::TeX(paste0("Gas production (original and Box-Cox transformed, $\\lambda$ = ", round(lambda,2),")")))



# -------- Moving Averages

# The original data 
global_economy |>
  filter(Country == "Australia") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

# Calculate the 5-year moving average using slide_dbl()
aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
                               .before = 2, .after = 2, .complete = TRUE)
  )

# Plot the origin data and the moving average
#   Play around with the order in the code above to see how the moving average changes
aus_exports |>
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports")




# -------- Moving Averages of Moving Averages: Estimating the trend-cycle with seasonal data

### Example: Australian Beer Production (quarterly data)

# The data set contains quarterly data
aus_production

# We select only for the beer column (later than 1992)
beer <- aus_production |>
  filter(year(Quarter) >= 1992) |>
  select(Quarter, Beer)

# plot the data
beer|>
  autoplot(Beer) +
  labs(y = "Beer production (megalitres)",
       title = "Australian Beer Production")

# Calculate the 5-MA:
beer_ma_5 <- beer |>
  mutate(
    `5-MA` = slider::slide_dbl(Beer, mean,
                               .before = 2, .after = 2, .complete = TRUE) # 5-MA
  ) 

# In the plot, we see that the trend-cycle is contaminated by the seasonality in the data:
beer_ma_5 |>
  autoplot(Beer) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "Beer production (megalitres)",
       title = "Total Australian Beer Production (5-MA)")

# Now try the 4-MA 
beer_ma_4 <- beer |>
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                               .before = 1, .after = 2, .complete = TRUE) # 5-MA
  ) 

# In the plot, we see that the smoothing works, but we are half a quarter ahead of time 
beer_ma_4 |>
  autoplot(Beer) +
  geom_line(aes(y = `4-MA`), colour = "#D55E00") +
  labs(y = "Beer production (megalitres)",
       title = "Total Australian Beer Production (4-MA)")

# Now we calculate the 4-MA and then the 2x4-MA
#   Note: We use mutate() to calculate both moving averages in one step!
beer_ma_2_4 <- beer |>
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                               .before = 1, .after = 2, .complete = TRUE), # 4-MA
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE) # 2x4-MA
  ) 

# In the plot we see that the seasonal variation is smoothed out completely by the 2x4-MA
# AND the 2x4 is not ahead of time, but is centred... :-)
beer_ma_2_4 |>
  autoplot(Beer) +
  geom_line(aes(y = `2x4-MA`), colour = "#D55E00") +
  labs(y = "Beer production (megalitres)",
       title = "Total Australian Beer Production (2x4-MA)")

# Compare the 2x4-MA to the 2x8-MA and the 2x12-MA
#   Notice: Since 8 and 12 are multiples of 4, they remove the seasonality as well.
#   Since the windows of the 2x8-MA and the 2x12-MA are larger, their smoothing effect is stronger. 
beer_ma_2_4_8_12 <- beer |>
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                               .before = 1, .after = 2, .complete = TRUE), # 4-MA
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE), # 2x4-MA
    `8-MA` = slider::slide_dbl(Beer, mean,
                               .before = 3, .after = 4, .complete = TRUE), # 8-MA
    `2x8-MA` = slider::slide_dbl(`8-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE), # 2x8-MA
    `12-MA` = slider::slide_dbl(Beer, mean,
                                .before = 5, .after = 6, .complete = TRUE), # 12-MA
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE) # 2x12-MA
  )

# Plot the 2x4-MA, 2x8-MA and 2x12-MA
#   Indeed we see that all of them remove the seasonality 
#   The 2x8-MA and 2x12-MA are smoother than the 2x4-MA

beer_ma_2_4_8_12 |>
  select(-c(`4-MA`, `8-MA`, `12-MA`)) |>
  pivot_longer(cols = c(`2x4-MA`,`2x8-MA`, `2x12-MA`), 
               names_to = "MA_type", 
               values_to = "Beer_smoothed") |> 
  mutate('MA_type' = factor(MA_type, levels = c("2x4-MA", "2x8-MA", "2x12-MA"))) %>%  # transform MA_type to a factor to have the facets below in correct order
  ggplot() +
  geom_line(aes(x = Quarter, y = Beer), color = "grey") +
  geom_line(aes(x = Quarter, y = Beer_smoothed), colour = "#D55E00") +
  facet_grid(MA_type ~ ., scales = "free_y") +
  labs(title = "Australian beer production (smoothed)",
       y = "Beer (megalitres)")



### Example: Employment in the US Retail Sector (monthly data)

# While the Beer data was quarterly, the us_employment data set is monthly
# We filter for the employment in the retail sector
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)

us_retail_employment

# Calculate the 12-MA and 2x12-MA
#   Note:Since the data is monthly, we must use a 2x12-MA to remove the seasonality
us_retail_employment_ma <- us_retail_employment |>
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )

# Plot the data: it worked!
us_retail_employment_ma |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")




# -------- Weighted Moving Averages

### Compare the 3x3-MA with the 5-MA for Australian exports

# Calculate the 3x3-MA 
aus_exports_3_3 <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `3-MA` = slider::slide_dbl(Exports, mean,
                               .before = 1, .after = 1, .complete = TRUE),
    `3x3-MA` = slider::slide_dbl(`3-MA`, mean,
                                 .before = 1, .after = 1, .complete = TRUE)
  )

# Plot the 3x3-MA
aus_exports_3_3 |>
  autoplot(Exports) +
  geom_line(aes(y = `3x3-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports (3x3-MA)")

# Plot the weights of the 3x3-MA as a bar plot
data.frame( # specify the weight vector
  window = c(1,2,3,4,5),
  weights = 1/9*c(1,2,3,2,1)
  ) |> 
ggplot() + # plot the weights
  geom_bar(aes(x = window, y = weights), stat = "identity", color = "red", fill = "white") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Weight",
       title = "Weights for a 3x3-MA")

# Calculate the 5-MA
aus_exports_5 <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
                               .before = 2, .after = 2, .complete = TRUE)
  )

# Plot the 5-MA
aus_exports |>
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports (5-MA)")

# Plot the weights of the 3x3-MA as a bar plot
data.frame(
  window = c(1,2,3,4,5),
  weights = 1/5*c(1,1,1,1,1)
) |>
ggplot() + 
  geom_bar(aes(x = window, y = weights), stat = "identity", color = "red", fill = "white") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Weight",
       title = "Weights for a 5-MA")



# -------- Classical Decomposition

### Example: Additive decomposition of the US retail employment data

# Filter the us_employment data for the retail sector
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID) # there is only one series left, so we can remove the Series_ID column

# Plot the original series
autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

# Apply the classical additive decomposition
us_retail_employment |>
  model(
    classical_decomposition(Employed, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")

### Example: Multiplicative decomposition of the Australian antidiabetic drug sales

# Load the a10 data on antidiabetic drug sales extracted from the data set tsibbledata::PBS
a10 <- readRDS("a10.RDS")
autoplot(a10, Cost) +
  labs(title = "Australian antidiabetic drug sales", y = "$ (millions)")

# Apply the classical multiplicative decomposition 
a10 |>
  model(
    classical_decomposition(Cost, type = "multiplicative")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical multiplicative decomposition of Australian antidiabetic drug sales")


 
# -------- STL Decomposition

### Example: STL decomposition of the US retail employment data

# Apply the STL decomposition to the US retail employment data
#   Note:
#   - The function model() from package fabletools learns one or several time series models 
#     from one or several time series stored in a tsibble. 
#     The different time series are identified by the key structure of the tsibble, and one model 
#     is learned for each series.
#   - In the second argument of model(), we specify which model we want to learn from the data.
#     To learn a STL decomposition, we use the function STL() from package feasts. 
#     With "stl" we specify the name of model column in the resulting "mable" (see below) that will hold the STL model.
dcmp <- us_retail_employment |>
   model(stl = STL(Employed)) 

# The output is a "mable" (a "model table", data class: mdl_df).
#   A mable is a tibble-like data structure for storing multiple models learned from a tsibble. 
#   Each row of the mable refers to a different time series within the tsibble. 
#   Each column contains all models from the model definition. 
#   In our case, we only have only one column holding the STL model, 
#   and only one row, because us_retail_employment only contains one time series.
dcmp

# The components() function from fabletools extracts the components of the decomposition.
#   Note: the components are stored as a "dable” (a "decomposition table", data class: dcmp_df).
components(dcmp) 

# Plot the dable
components(dcmp) |> 
  autoplot() +
  labs(title = "STL decomposition of total US retail employment (default parameters)")
 
# Play around with the parameters of the STL decomposition
#     Note: 
#     - As always in R,the tilde ~ defines a formula.
#     - Here, the formula specifies that the "Employed" column is decomposed into a trend-cycle and a seasonal component.
#     - The window arguments specify the length of the trend and seasonal windows respectively, see help(STL).
us_retail_employment |>
  model(
    STL(Employed ~ trend(window = 7) + # trend-cycle is more flexible now
          season(window = "periodic"), # the seasonal pattern is fixed
        robust = TRUE)) |> # robust = TRUE makes the decomposition more robust to outliers  
  components() |>
  autoplot() +
  labs(title = "STL of total US retail emp. (robust, fixed seasonal pattern, trend window=7)")
 
 
 