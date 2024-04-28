# ====================================
# Time Series Analysis with R
# gwendolin.wilke@fhnw.ch
# This script contains code used in 
# Part 2, Lecture 4 Time Series Regression (part1)
# ==================================== 

library(fpp3)

# --------- Simple Linear Regression ---------

### Example: US consumption expenditure

# A time plot of both time series 
us_change |>
  pivot_longer(c(Consumption, Income), names_to="Series") |>
  autoplot(value) +
  labs(y = "% change")

# Scatter plot of the two time series with a regression line
us_change |>
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Use the TSLM() function to fit a simple linear regression model
us_change |>
  model(TSLM(Consumption ~ Income)) |>
  report()



# --------- Multiple Linear Regression ---------

# Plot the 3 additional time series contained in the data set
us_change |>
  select(-Consumption, -Income) |>
  pivot_longer(-Quarter) |>
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")

# Plot the pairwise scatterplots
us_change |>
  GGally::ggpairs(columns = 2:6)


fit_consMR <- us_change |>
  model(tslm = TSLM(Consumption ~ Income + Production +
                      Unemployment + Savings))

report(fit_consMR)


# --------- Fitted Values ---------

# Time plots of observed and fitted values
augment(fit_consMR) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))


# Scatter plot of observed and fitted values
augment(fit_consMR) |>
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)



# --------- Evaluating a Regression Model---------

### Residual diagnostics

# ACF plot and histogram of residuals
fit_consMR |> gg_tsresiduals()

# Ljung-Box test for autocorrelation in residuals
augment(fit_consMR) |>
  features(.innov, ljung_box, lag = 10)

# Plot residuals against predictors
us_change |>
left_join(residuals(fit_consMR), by = "Quarter") |>
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") |>
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

# Plot residuals against fitted values
augment(fit_consMR) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")

# Spurious regression

fit <- aus_airpassengers |>
  filter(Year <= 2011) |>
  left_join(guinea_rice, by = "Year") |>
  model(TSLM(Passengers ~ Production))

report(fit)

fit |> gg_tsresiduals()
